import org.scalatest.{MustMatchers, WordSpec}
import cats._
import cats.implicits._
import cats.effect._
import net.andimiller.gatto.Parser
import net.andimiller.gatto.Parser._

class ParserTSpec extends WordSpec with MustMatchers {
  val dsl = new Dsl[IO, String, Char] {}
  import dsl._

  "basic parsers which use literals" should {
    "work with normal data" in {
      val literalParser: Parser.ParserT[IO, String, (Char, Char)] = for {
        a <- literal('a')
        b <- literal('b')
      } yield (a, b)
      val literalsParser: Parser.ParserT[IO, String, (String, String)] = for {
        as <- literals("aa")
        bs <- literals("bb")
      } yield (as, bs)
      literalParser.parse("ab").value.unsafeRunSync() must equal(Right("", ('a', 'b')))
      literalParser.parse("abc").value.unsafeRunSync() must equal(Right("c", ('a', 'b')))
      literalParser.parse("nah").value.unsafeRunSync() must equal(Left("Expected: a, Got: n"))
    }
  }

}
