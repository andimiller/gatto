package net.andimiller.gatto.bench

import java.util.concurrent.atomic.AtomicLong

import atto.ParseResult
import cats.{Eval, Id}
import cats.data.EitherT
import cats.effect._
import cats.implicits._
import fastparse.Parsed
import net.andimiller.gatto.bench.BasicBench.BenchState
import org.openjdk.jmh.annotations.{Benchmark, Level, Scope, State}

object BasicBench extends IOApp {

  @State(Scope.Thread)
  class BenchState {
    final val input = "aabbcc"

    final val attoParser: String => ParseResult[List[String]] = {
      import atto._
      import Atto._
      val p = many(string("a") | string("b") | string("c"))

      { s: String => p.parseOnly(s) }
    }

    final val gattoParser: String => EitherT[Id, String, (String, List[Char])] = {
      import cats.implicits._
      import net.andimiller.gatto.Parser._
      val dsl = new Dsl[cats.Id, String, Char]() {}
      import dsl._

      val p = (literal('a').orElse(literal('b')).orElse(literal('c'))).rep

      { s: String => p.parse(s) }
    }

    final val fastParseParser: String => Parsed[Seq[String]] = {
      import fastparse._, NoWhitespace._

      def p[_: P] = (P("a") | P("b") | P("c")).!.rep

      { s: String => parse(s, p(_)) }
    }
  }

  import scala.concurrent.duration._

  def runThingForXSeconds(f: () => Unit): IO[Long] = {
    val counter = new AtomicLong(0L)
    for {
      started <- IO {
        while (true) {
          f()
          counter.incrementAndGet()
        }
      }.start
      _ <- IO.sleep(5.seconds)
      _ <- started.cancel
    } yield counter.get()
  }

  override def run(args: List[String]): IO[ExitCode] = for {
    _ <- IO { println("starting benchmarks") }
    s = new BenchState
    atto <- runThingForXSeconds({() => s.attoParser(s.input) })
    _ <- IO { println(s"atto warmup: $atto")}
    atto2 <- runThingForXSeconds({() => s.attoParser(s.input) })
    _ <- IO { println(s"atto real: $atto2")}
    gatto <- runThingForXSeconds({() => s.gattoParser(s.input) })
    _ <- IO { println(s"gtto warmup: $gatto")}
    gatto2 <- runThingForXSeconds({() => s.gattoParser(s.input) })
    _ <- IO { println(s"gatto real: $gatto2")}
  } yield ExitCode.Success

}

class BasicBench {
  @Benchmark
  def parseStringAtto(s: BenchState): Unit = {
    s.attoParser(s.input)
  }

  @Benchmark
  def parseStringGatto(s: BenchState): Unit = {
    s.gattoParser(s.input)
  }

  @Benchmark
  def parseStringFastparse(s: BenchState): Unit = {
    s.fastParseParser(s.input)
  }

}
