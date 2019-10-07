import org.scalatest.{MustMatchers, WordSpec}
import cats._
import cats.implicits._
import cats.effect._
import net.andimiller.gatto.Parser
import net.andimiller.gatto.Parser._

class ParserTSpec extends WordSpec with MustMatchers {
  val dsl = new Dsl[Eval, String, Char] {}
  import dsl._

  "basic parsers which use literals" should {
    "work with normal data" in {
      val literalParser: Parser.ParserT[Eval, String, String, (Char, Char)] = for {
        a <- literal('a')
        b <- literal('b')
      } yield (a, b)
      val literalsParser: Parser.ParserT[Eval, String, String, (String, String)] = for {
        as <- literals("aa")
        bs <- literals("bb")
      } yield (as, bs)
      literalParser.parse("ab").value.value must equal(Right("", ('a', 'b')))
      literalParser.parse("abc").value.value must equal(Right("c", ('a', 'b')))
      literalParser.parse("nah").value.value must equal(Left("Expected: a, Got: n"))
    }
  }
  "takeWhile" should {
    "be able to takeWhile" in {
      val takeAs = takeWhile(_ == 'a')
      takeAs.parse("aaaaa").value.value must equal(Right("", "aaaaa"))
      takeAs.parse("aaaaabc").value.value must equal(Right("bc", "aaaaa"))
    }
  }

  "opt" should {
    "make parsers optional" in {
      val parser = for {
        a <- literal('a')
        b <- literal('b').opt
        c <- literal('c')
        d <- literal('d').opt
        e <- literal('e')
      } yield (a, b, c, d, e)
      parser.parse("abcde").value.value must equal(Right("", ('a', Some('b'), 'c', Some('d'), 'e')))
      parser.parse("ace").value.value must equal(Right("", ('a', None, 'c', None, 'e')))
    }
  }

  "rep" should {
    "make parsers repeat" in {
      val parser: ParserT[Eval, String, String, (List[Char], List[Char])] = for {
        letters <- takeIf(_.isLetter, "").rep
        _       <- literal(' ')
        digits  <- takeIf(_.isDigit, "").rep
      } yield (letters, digits)
      parser.parse("abc 123").value.value must equal(Right(("", (List('a', 'b', 'c'), List('1', '2', '3')))))
      parser.parse(" ").value.value must equal(Right(("", (List.empty, List.empty))))
    }
  }

  "subparsers" should {
    "work" in {
      val takeLowercase = takeWhile(_.isLower)
      object Subparsers {
        val dsl = new Dsl[Eval, List[Int], Int] {}
        import dsl._
        val evens    = takeIf(_ % 2 == 0, "must be even").as("even")
        val odds     = takeIf(_ % 2 != 0, "must be odd").as("odd")
        val combined = evens.orElse(odds)
      }
    }
  }

}
