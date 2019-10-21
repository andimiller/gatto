package net.andimiller.gatto

import cats._
import cats.arrow.Profunctor
import cats.data.{EitherT, IndexedStateT, NonEmptyList, StateT}
import cats.implicits._

import scala.language.higherKinds

object Parser {

  implicit class MergeOps[F[_]: Monad, S, O](parser: ParserT[F, S, S, O]) {
    def valueMerged: StateT[EitherT[F, String, *], S, O] = parser.value

    // combinators
    def orElse[O2 <: O](other: ParserT[F, S, S, O2]): ParserT[F, S, S, O] = ParserT(this.valueMerged.orElse(other.widen[O].value))

    def rep(implicit M: Monad[ParserT[F, S, S, *]]): ParserT[F, S, S, List[O]] =
      List
        .empty[Either[String, O]]
        .iterateWhileM { os =>
          parser.attempt.map(o => o :: os)
        }(_.forall(_.isRight))
        .map(_.separate._2.reverse)

    def rep1(implicit M: Monad[ParserT[F, S, S, *]]): ParserT[F, S, S, NonEmptyList[O]] =
      (parser, parser.rep).tupled.map { case (o, os) => NonEmptyList(o, os) }

    def opt: ParserT[F, S, S, Option[O]] =
      parser.map(_.some).orElse(ParserT.pure[F, S, Option[O]](None))
  }

  case class ParserT[F[_]: Monad, S, S2,  O](value: IndexedStateT[EitherT[F, String, *], S, S2, O]) {
    // ways to run the parser
    def parse(s: S): EitherT[F, String, (S2, O)] = value.run(s)

    // modification methods
    def widen[O2 >: O]: ParserT[F, S, S2, O2] = this.asInstanceOf[ParserT[F, S, S2, O2]]

    // combinators
    def validate(f: O => Boolean, otherwise: String): ParserT[F, S, S2, O] =
      ParserT(value.flatMapF(o => EitherT.fromEither[F](Either.cond(f(o), o, otherwise))))

    def validateMap[O2](f: O => Either[String, O2]): ParserT[F, S, S2, O2] =
      ParserT(value.flatMapF(o => EitherT.fromEither[F](f(o))))

    def filter(f: O => Boolean): ParserT[F, S, S2, O] = validate(f, "Failed filter function")

    def map[B](f: O => B): ParserT[F, S, S2, B] =
      ParserT(value.map(f))

    def contramap[S0](f: S0 => S): ParserT[F, S0, S2, O] =
      ParserT(value.contramap(f))

    def stateMap[S3](f: S2 => S3): ParserT[F, S, S3, O] =
      ParserT(value.modify(f))

  }

  trait LowPriorityParserTImplicits {
    implicit def parserTFunctorFilter[F[_]: Monad, S, S2]: Functor[ParserT[F, S, S2, *]] = new Functor[ParserT[F, S, S2, *]] {
      private val underlying           = Functor[IndexedStateT[EitherT[F, String, *], S, S2, *]]
      override def map[A, B](fa: ParserT[F, S, S2, A])(f: A => B): ParserT[F, S, S2, B] =
        ParserT(underlying.map(fa.value)(f))
    }

    implicit def parserTContravariant[F[_]: Monad, S2, O]: Contravariant[ParserT[F, *, S2, O]] = new Contravariant[ParserT[F, *, S2, O]] {
      private val underlying = implicitly[Contravariant[IndexedStateT[EitherT[F, String, *], *, S2, O]]]
      override def contramap[A, B](fa: ParserT[F, A, S2, O])(f: B => A): ParserT[F, B, S2, O] =
        ParserT(underlying.contramap(fa.value)(f))
    }
  }

  object ParserT extends LowPriorityParserTImplicits {
    def pure[F[_]: Monad, S, O](o: O): ParserT[F, S, S, O] =
      new ParserT(StateT.liftF(EitherT.pure[F, String](o)))

    //    type InternalParserT[F[_], S, O] = StateT[EitherT[F, String, ?], S, O]
    def liftF[F[_]: Monad, S, O](f: S => F[Either[String, (S, O)]]): ParserT[F, S, S, O] =
      ParserT(StateT({ s: S =>
        EitherT(f(s))
      }))

    implicit def parserTMonadError[F[_]: Monad, S]: MonadError[ParserT[F, S, S, *], String] =
      new Monad[ParserT[F, S, S, *]] with MonadError[ParserT[F, S, S, *], String] {
        private val underlying           = Monad[IndexedStateT[EitherT[F, String, *], S, S, *]]
        private val underlyingMonadError = MonadError[IndexedStateT[EitherT[F, String, *], S, S, *], String]

        def pure[A](x: A): ParserT[F, S, S, A] = ParserT(underlying.pure(x))

        def flatMap[A, B](fa: ParserT[F, S, S, A])(f: A => ParserT[F, S, S, B]): ParserT[F, S, S, B] =
          ParserT(fa.value.flatMap(r => f(r).value))

        def tailRecM[A, B](a: A)(f: A => ParserT[F, S, S, Either[A, B]]): ParserT[F, S, S, B] =
          ParserT(underlying.tailRecM(a)(f.map(_.value)))

        def raiseError[A](e: String): ParserT[F, S, S, A] =
          new ParserT(underlyingMonadError.raiseError(e))

        def handleErrorWith[A](fa: ParserT[F, S, S, A])(f: String => ParserT[F, S, S, A]): ParserT[F, S, S, A] =
          new ParserT(underlyingMonadError.handleErrorWith(fa.value)(f.map(_.value)))
      }

    implicit def semigroupK[F[_]: Monad, T]: SemigroupK[ParserT[F, T, T, *]] = new SemigroupK[ParserT[F, T, T, *]] {
      override def combineK[A](x: ParserT[F, T, T, A], y: ParserT[F, T, T, A]): ParserT[F, T, T, A] =
        x orElse y
    }

    implicit def semigroup[F[_]: Monad, T, O]: Semigroup[ParserT[F, T, T, O]] = new Semigroup[ParserT[F, T, T, O]] {
      override def combine(x: ParserT[F, T, T, O], y: ParserT[F, T, T, O]): ParserT[F, T, T, O] =
        x orElse y
    }

  }

  abstract class Dsl[F[_]: Monad, S, T: Eq: Show](implicit cpf: CanParseFrom[S, T], pm: Monad[ParserT[F, S, S, *]]) {

    def pure[A](a: A): ParserT[F, S, S, A] = ParserT.pure[F, S, A](a)

    def takeOne: ParserT[F, S, S, T] = ParserT.liftF[F, S, T] { s: S =>
      Monad[F].point(
        Either.fromOption(cpf.take1(s), "Cannot consume empty input")
      )
    }

    def takeWhile(f: T => Boolean): ParserT[F, S, S, S] = ParserT.liftF[F, S, S] { s: S =>
      Monad[F].point(
        cpf.takeWhile(s, f).asRight[String]
      )
    }

    def takeWhile1(f: T => Boolean): ParserT[F, S, S, S] = takeWhile(f).validate(cpf.nonEmpty, "Must consume at least one token")

    def takeIf(f: T => Boolean, otherwise: String): ParserT[F, S, S, T] = takeOne.validate(f, otherwise)

    def get: ParserT[F, S, S, T] = ParserT.liftF[F, S, T] { s: S =>
      Monad[F].point(
        Either.fromOption(cpf.take1(s).map { case (_, t) => (s, t) }, "Cannot consume empty input")
      )
    }

    def literal(target: T): ParserT[F, S, S, T] = ParserT.liftF[F, S, T] { s: S =>
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

    def literals(target: S): ParserT[F, S, S, S] = ParserT.liftF[F, S, S] { s: S =>
      Monad[F].point(
        Either.fromOption(
          cpf.takeLiteral(s, target), s"expected $s"
        )
      )
    }

  }

}
