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
}

object Main extends IOApp {


  override def run(args: List[String]): IO[ExitCode] =
    for {
      res1 <- Parsers.literalParser.parse("ab").value
      res2 <- Parsers.literalParser.parse("abc").value
      res3 <- Parsers.literalParser.parse("nah").value
      _ <- IO {
        println(res1)
        println(res2)
        println(res3)
      }
    } yield ExitCode.Success
}
