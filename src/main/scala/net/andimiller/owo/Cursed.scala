package net.andimiller.owo

import net.andimiller.gatto.Parser._
import cats.implicits._
import cats._
import cats.data.NonEmptyList
import cats.effect._

object Cursed extends Dsl[Eval, String, Char] with App {

  object Furry {
    val letters: ParserT[Eval, String, String] = NonEmptyList.of(
      literal('l').as('w'),
      literal('r').as('w'),
      literal('L').as('W'),
      literal('R').as('W')
    ).reduceK.map(_.toString)

    val strings: ParserT[Eval, String, String] = NonEmptyList.of(
      literals("no").as("nu"),
      literals("has").as("haz"),
      literals("o ").as("owo "),
      literals(". ").as(" :3 "),
      literals("hi ").as("hoi ")
    ).reduceK

    val passthrough: ParserT[Eval, String, String] = takeOne.map(_.toString)

    def uwu(s: String): Either[String, (String, String)] = (strings orElse letters orElse passthrough).rep.map(_.mkString).parse(s).value.value
  }

  object Pirate {
    def casePreservingMatch(s: String): ParserT[Eval, String, String] =
      s.toLowerCase.toList.map(literal).map(_.contramap{c: Char => c.toLower})


    val replacements = Map(
      "this" -> "'tis",
      "g " -> "' ",
      "you" -> "ye",
      "hello" -> "ahoy",
      "friend" -> "matey",
      "friends" -> "maties",
      "you" -> "ye",
      "yes" -> "aye",
      "ar" -> "arr"
    ).map { case (k, v) =>
        k -> v
    }
  }
  println(Furry.uwu("Alexander Boris de Pfeffel Johnson is a British politician, writer, and former journalist serving as Prime Minister of the United Kingdom and Leader of the Conservative Party since July 2019. "))

}
