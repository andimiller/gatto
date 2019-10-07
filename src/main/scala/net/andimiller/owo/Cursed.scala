package net.andimiller.owo

import net.andimiller.gatto.Parser._
import cats.implicits._
import cats._
import cats.data.NonEmptyList
import cats.effect._

import scala.collection.immutable

object Cursed extends Dsl[Eval, String, Char] with App {
  object Furry {
    val letters: ParserT[Eval, String, String, String] = NonEmptyList.of(
      literal('l').as('w'),
      literal('r').as('w'),
      literal('L').as('W'),
      literal('R').as('W')
    ).reduceK.map(_.toString)

    val strings: ParserT[Eval, String, String, String] = NonEmptyList.of(
      literals("no").as("nu"),
      literals("has").as("haz"),
      literals("o ").as("owo "),
      literals(". ").as(" :3 "),
      literals("hi ").as("hoi ")
    ).reduceK

    val passthrough: ParserT[Eval, String, String, String] = takeOne.map(_.toString)

    def uwu(s: String): Either[String, (String, String)] = (strings orElse letters orElse passthrough).rep.map(_.mkString).parse(s).value.value
  }

  object Pirate {
    def casePreservingMatch(s: String): ParserT[Eval, String, String, String] =
      s.toList.map { c =>
        takeOne.validateMap { consumed =>
          Either.cond(consumed.toLower == c.toLower, consumed match {
            case _ if consumed.isUpper => c.toUpper
            case _ if consumed.isLower => c.toLower
            case _ => c
          }, s"Expected $c and got $consumed")
        }
      }.sequence.map(_.mkString)


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
        casePreservingMatch(k).map {
          case s if s.forall(_.isUpper) => v.toUpperCase
          case s if s.forall(_.isLower) => v.toLowerCase
          case s if s.head.isUpper && !s.tail.forall(_.isUpper) => v.capitalize
          case _ => v
        }
    }.reduce(_ orElse _)

    val passthrough: ParserT[Eval, String, String, String] = takeOne.map(_.toString)

    def yarr(s: String): Either[String, (String, String)] = (replacements orElse passthrough).rep.map(_.mkString).parse(s).value.value
  }
  println(Furry.uwu("Alexander Boris de Pfeffel Johnson is a British politician, writer, and former journalist serving as Prime Minister of the United Kingdom and Leader of the Conservative Party since July 2019. "))
  println(Pirate.yarr("Alexander Boris de Pfeffel Johnson is a British politician, writer, and former journalist serving as Prime Minister of the United Kingdom and Leader of the Conservative Party since July 2019. "))

}
