package net.andimiller.gatto

import cats._
import cats.data.{EitherT, Nested, NonEmptyList, StateT}
import cats.implicits._

import scala.language.higherKinds

object Parser {

  case class Position(line: Long, character: Long)

  case class ParserT[F[_]: Monad, S, O](value: StateT[EitherT[F, String, *], S, O]) {
    // ways to run the parser
    def parse(s: S): EitherT[F, String, (S, O)] = value.run(s)

    // combinators
    def orElse[O2 <: O](other: ParserT[F, S, O2]): ParserT[F, S, O] = ParserT(value.orElse(other.value.widen[O]))

    def subFlatMap[O2](subParser: ParserT[F, O, O2]): ParserT[F, S, O2] =
      ParserT(value.flatMapF { o =>
        subParser.value.run(o).map(_._2)
      })

    def validate(f: O => Boolean, otherwise: String): ParserT[F, S, O] =
      ParserT(value.flatMapF(o => EitherT.fromEither[F](Either.cond(f(o), o, otherwise))))

    def filter(f: O => Boolean): ParserT[F, S, O] = validate(f, "Failed filter function")

    def rep(implicit M: Monad[ParserT[F, S, *]]): ParserT[F, S, List[O]] =
      List
        .empty[Either[String, O]]
        .iterateWhileM { os =>
          this.attempt.map(o => o :: os)
        }(_.forall(_.isRight))
        .map(_.separate._2.reverse)

    def rep1(implicit M: Monad[ParserT[F, S, *]]): ParserT[F, S, NonEmptyList[O]] =
      (this, this.rep).tupled.map { case (o, os) => NonEmptyList(o, os) }

    def opt: ParserT[F, S, Option[O]] =
      this.map(_.some).orElse(ParserT.pure[F, S, Option[O]](none))

  }

  object ParserT {
    case class Par[F[_]: Monad, S, O](value: StateT[EitherT[F, String, *], S, O]) {}
    object Par {
      implicit def parApplicative[F[_]: Monad, S: Eq: Show, C](implicit cpf: CanParseFrom[S, C]): Applicative[Par[F, S, *]] =
        new Applicative[Par[F, S, *]] {
          private val underlying = Monad[StateT[EitherT[F, String, *], S, *]]

          def pure[A](a: A): Par[F, S, A] = Par(underlying.pure(a))

          def ap[A, B](ff: Par[F, S, A => B])(fa: Par[F, S, A]): Par[F, S, B] = {
            Par(StateT { s: S =>
              (ff.value.run(s), fa.value.run(s)).bisequence.subflatMap {
                case ((s1, f), (s2, a)) =>
                  (cpf.orElse(s1, s2), f(a)).asRight[String]
              }
            })
          }
          override def product[A, B](fa:  Par[F, S, A], fb:  Par[F, S, B]): Par[F, S, (A, B)] =
            Par(StateT { s: S =>
              (fa.value.run(s), fb.value.run(s)).bisequence.subflatMap {
                case ((s1, f), (s2, a)) =>
                  if (s1 === s2){
                    (s1, (f, a)).asRight[String]
                  } else {
                    "Parsers must consume the same tokens to be applied in parallel".asLeft[(S, (A, B))]
                  }
              }
            })
        }
    }

    implicit def parserTParallel[M[_]: Monad, S: Eq: Show, C](implicit cpf: CanParseFrom[S, C]): Parallel.Aux[ParserT[M, S, *], Par[M, S, *]] = new Parallel[ParserT[M, S, *]] {
      type F[A] = Par[M, S, A]
      override def applicative: Applicative[F] =
        Par.parApplicative[M, S, C]
      override def monad: Monad[ParserT[M, S, *]] =
        ParserT.parserTMonad[M, S]
      override def sequential: F ~> ParserT[M, S, *] = new (F ~> ParserT[M, S, *]) {
        def apply[A](fa: F[A]): ParserT[M, S, A] = ParserT(fa.value)
      }
      override def parallel: ParserT[M, S, *] ~> F = new (ParserT[M, S, *] ~> F) {
        def apply[A](fa: ParserT[M, S, A]): F[A] = Par(fa.value)
      }
    }

    implicit def parserTSemigroupK[M[_]: Monad, S: Show]: SemigroupK[ParserT[M, S, *]] = new SemigroupK[ParserT[M, S, *]] {
      def combineK[A](x: ParserT[M, S, A], y: ParserT[M, S, A]): ParserT[M, S, A] =
        x.orElse(y)
    }

    def pure[F[_]: Monad, S, O](o: O): ParserT[F, S, O] =
      ParserT(StateT.liftF(EitherT.pure[F, String](o)))

    //    type InternalParserT[F[_], S, O] = StateT[EitherT[F, String, ?], S, O]
    def liftF[F[_]: Monad, S, O](f: S => F[Either[String, (S, O)]]): ParserT[F, S, O] =
      ParserT(StateT({ s: S =>
        EitherT(f(s))
      }))

    implicit def parserTMonad[F[_]: Monad, S]: MonadError[ParserT[F, S, *], String] =
      new Monad[ParserT[F, S, *]] with MonadError[ParserT[F, S, *], String] {
        private val underlying           = Monad[StateT[EitherT[F, String, *], S, *]]
        private val underlyingMonadError = MonadError[StateT[EitherT[F, String, *], S, *], String]

        def pure[A](x: A): ParserT[F, S, A] = ParserT(underlying.pure(x))

        def flatMap[A, B](fa: ParserT[F, S, A])(f: A => ParserT[F, S, B]): ParserT[F, S, B] =
          ParserT(fa.value.flatMap(r => f(r).value))

        def tailRecM[A, B](a: A)(f: A => ParserT[F, S, Either[A, B]]): ParserT[F, S, B] =
          ParserT(underlying.tailRecM(a)(f.map(_.value)))

        def raiseError[A](e: String): ParserT[F, S, A] =
          ParserT(underlyingMonadError.raiseError(e))

        def handleErrorWith[A](fa: ParserT[F, S, A])(f: String => ParserT[F, S, A]): ParserT[F, S, A] =
          ParserT(underlyingMonadError.handleErrorWith(fa.value)(f.map(_.value)))

      }

  }

  abstract class Dsl[F[_]: Monad: Defer, S, T: Eq: Show](implicit cpf: CanParseFrom[S, T], pm: Monad[ParserT[F, S, *]]) {

    def takeOne: ParserT[F, S, T] = ParserT.liftF[F, S, T] { s: S =>
      Defer[F].defer {
        Monad[F].point(
          Either.fromOption(cpf.take1(s), "Cannot consume empty input")
        )
      }
    }

    def takeWhile(f: T => Boolean): ParserT[F, S, S] = takeOne.validate(f, "").rep.map(cpf.combine)

    def takeWhile1(f: T => Boolean): ParserT[F, S, S] = takeOne.validate(f, "").rep1.map(ss => cpf.combine(ss.toList))

    def takeIf(f: T => Boolean, otherwise: String): ParserT[F, S, T] = takeOne.validate(f, otherwise)

    def get: ParserT[F, S, T] = ParserT.liftF[F, S, T] { s: S =>
      Defer[F].defer {
        Monad[F].point(
          Either.fromOption(cpf.take1(s).map { case (_, t) => (s, t) }, "Cannot consume empty input")
        )
      }
    }

    def literal(target: T): ParserT[F, S, T] = ParserT.liftF[F, S, T] { s: S =>
      Defer[F].defer {
        Monad[F].point(
          Either
            .fromOption(cpf.take1(s), "Cannot consume empty input")
            .flatMap {
              case (s, t) =>
                t match {
                  case _ if t === target => (s, t).asRight[String]
                  case _                 => s"Expected: ${target.show}, Got: ${t.show}".asLeft[(S, T)]
                }
            }
        )
      }
    }

    def literals(target: S): ParserT[F, S, S] = cpf.split(target).traverse(t => literal(t)).map(cpf.combine)

    /*    def takeIf[O](f: T => Either[String, O]): ParserT[F, S, O] = ParserT.liftF[F, S, O] { s: S =>
      Defer[F].defer {
        Monad[F].point(
          Either
            .fromOption(cpf.take1(s), "Cannot consume empty input")
            .flatMap {
              case (s, t) =>
                f(t).map(o => (s, o))
            }
        )
      }
    }
  }
   */

  }

}
