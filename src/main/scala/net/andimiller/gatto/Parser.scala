package net.andimiller.gatto

import cats._
import cats.data.{EitherT, NonEmptyList, StateT}
import cats.implicits._

import scala.language.higherKinds

object Parser {

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
    def pure[F[_]: Monad, S, O](o: O): ParserT[F, S, O] =
      ParserT(StateT.liftF(EitherT.pure[F, String](o)))

    //    type InternalParserT[F[_], S, O] = StateT[EitherT[F, String, ?], S, O]
    def liftF[F[_]: Monad, S, O](f: S => F[Either[String, (S, O)]]): ParserT[F, S, O] =
      ParserT(StateT({ s: S =>
        EitherT(f(s))
      }))

    implicit def parserTMonad[F[_]: Monad, S]: MonadError[ParserT[F, S, *], String] =
      new Monad[ParserT[F, S, *]] with MonadError[ParserT[F, S, *], String] {
        val underlying           = Monad[StateT[EitherT[F, String, *], S, *]]
        val underlyingMonadError = MonadError[StateT[EitherT[F, String, *], S, *], String]

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

  abstract class Dsl[F[_]: Monad: Defer, S, T: Eq: Show](implicit cpf: CanParseFrom[S, T]) {

    import ParserT._

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
