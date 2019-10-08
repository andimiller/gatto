package net.andimiller.gatto

import cats._
import cats.implicits._
import cats.effect._
import net.andimiller.gatto.Parser.Dsl

object Parsers extends Dsl[IO, String, Char] {
  val literalParser: Parser.ParserT[IO, String, (Char, Char)] = for {
    a <- literal('a')
    b <- literal('b')
  } yield (a, b)
  val literalsParser: Parser.ParserT[IO, String, (String, String)] = for {
    as <- literals("aa")
    bs <- literals("bb")
  } yield (as, bs)
  val repeatedParser = for {
    as <- literal('a').rep
    b  <- literal('b')
  } yield (as, b)
}

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    for {
      res1 <- Parsers.repeatedParser.parse("ab").value
      res2 <- Parsers.repeatedParser.parse("aaaaaabc").value
      res3 <- Parsers.repeatedParser.parse("nah").value
      _ <- IO {
            println(res1)
            println(res2)
            println(res3)
          }
    } yield ExitCode.Success
}
