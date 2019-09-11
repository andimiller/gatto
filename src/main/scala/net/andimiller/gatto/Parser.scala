package net.andimiller.gatto

import cats._
import cats.data.{EitherT, StateT}
import cats.implicits._

import scala.language.higherKinds

object Parser {

  // we assume errors are Strings for now
  type ParserT[F[_], S, O] = StateT[EitherT[F, String, ?], S, O]
  object ParserT {
    def apply[F[_]: Monad, S, O](f: S => F[Either[String, (S, O)]]): ParserT[F, S, O] = StateT({s: S => EitherT(f(s))})
  }

  class Dsl[F[_]: Monad: Defer, S, T: Eq: Show](implicit cpf: CanParseFrom[S, T]) {
    def literal(target: T): ParserT[F, S, T] = ParserT[F, S, T] { s: S =>
      Defer[F].defer {
        Monad[F].point(
          Either.fromOption(cpf.take1(s), "Cannot consume empty input")
            .flatMap { case (t, s) =>
                t match {
                  case _ if t === target => (s, t).asRight[String]
                  case _ => s"Expected: ${target.show}, Got: ${t.show}".asLeft[(S, T)]
                }
            }
        )
      }
    }
    def literals(target: S): ParserT[F, S, S] = cpf.split(target).traverse(t => literal(t)).map(cpf.combine)
  }

}
