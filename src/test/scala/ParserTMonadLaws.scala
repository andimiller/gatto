import cats._
import cats.implicits._
import cats.data._
import cats.laws.discipline.FunctorTests
import net.andimiller.gatto.Parser.ParserT
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.typelevel.discipline.scalatest.Discipline


class ParserTMonadLaws extends AnyFunSuite with Discipline {
  import Arbitraries._
  checkAll("Tree.FunctorLaws", FunctorTests[ParserT[Eval, String, *]].functor[Int, Int, String])
}

object Arbitraries {
  implicit def parserTEq[F[_]: Monad: Defer, O: Show: Eq] = Eq[StateT[EitherT[F, String, *], String, O]] = {
    implicitly[Eq[StateT[EitherT[F, String, *], String, O]]]
  }

  implicit def arbParserT[M[_]: Monad: Defer, O: Show: Arbitrary]: Arbitrary[ParserT[M, String, O]] = Arbitrary({
    for {
      o <- implicitly[Arbitrary[O]].arbitrary
    } yield ParserT.liftF({ s: String =>
      Defer[M].defer {
        Monad[M].point {
          (s, o).asRight[String]
        }
      }
    })
  })
}
