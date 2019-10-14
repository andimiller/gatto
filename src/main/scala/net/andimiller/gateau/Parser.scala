package net.andimiller.gateau

import scala.annotation.tailrec
import scala.language.higherKinds

object Parser {
  // Deterministic, Error-Correcting Combinator Parsers - S. Doaitse Swierstra and Luc Duponcheel 1996
  // first define the Text typeclass they use
  type ShowS = String => String
  trait Text[A] {
    def showList(as: List[A]): ShowS
    def shows(a: A): ShowS
    def show(a: A): String
  }

  // and hope scala's Ordering is good enough
  trait Symbol[A] extends Text[A] with Ordering[A]


  trait Parsing[P[_, _]] {
    def empty[S: Symbol, A](a: A): P[S, A]
    def symbol[S: Symbol](s: S): P[S, S]

    def `<?>`[S: Symbol, A](p: P[S, A], a: (A, String)): P[S, A]
    def <|>[S: Symbol, A](p1: P[S, A], p2: P[S, A]): P[S, A]
    def <*>[S: Symbol, A, B](pf: P[S, B => A], pb: P[S, B]): P[S, A]
    def ap[S: Symbol, A, B](pf: P[S, B => A])(pb: P[S, B]): P[S, A]
    def `<$>`[S: Symbol, A, B](f: B => A, p: P[S, B]): P[S, A] =
      <*>(empty(f), (p))
    def map[S: Symbol, A, B](p: P[S, B])(f: B => A): P[S, A] =
      <*>(empty(f), p)
    def opt[S: Symbol, A, B](p: P[S, A], v: A): P[S, A] =
      <|>(p, empty(v))


  }
  object Parsing {
    def apply[P[_, _]](implicit P: Parsing[P]): Parsing[P] = P
  }

  // Combinators
  // this will 100% blow the stack, rewrite with trampoline
  //@tailrec
  def many[P[_, _]: Parsing, S: Symbol, A](p: P[S, A]): P[S, List[A]] = {
    val f: A => List[A] => List[A] = a => as => a :: as
    ((f `<$>` p) <*> many(p)) opt List.empty
  }

  def chainr[P[_, _]: Parsing, S: Symbol, A](x: P[S, A], op: P[S, A => A => A]): P[S, A] = {
    (op <*> chainr(x, op)).opt(identity) ap x
  }


  // Syntax
  implicit class ParsingOps[P[_, _], S: Symbol, A](p: P[S, A])(implicit val Parsing: Parsing[P]) {
    def `<?>`(a: (A, String)): P[S, A] = Parsing.<?>(p, a)
    def <|>(p2: P[S, A]): P[S, A] = Parsing.<|>(p, p2)
    def opt(a: A): P[S, A] = Parsing.opt(p, a)
    def map[B](f: A => B): P[S, B] = Parsing.map(p)(f)
  }

  implicit class ParsingFuncOps[B, A](f: B => A) {
    def `<$>`[P[_, _]: Parsing, S: Symbol](p: P[S, B]): P[S, A] = implicitly[Parsing[P]].`<$>`(f, p)
  }

  implicit class ParsingFuncOps2[P[_, _]: Parsing, S: Symbol, B, A](pf: P[S, B => A]) {
    def <*>(pb: P[S, B]): P[S, A] = Parsing[P].<*>(pf, pb)
    def ap(pb: P[S, B]): P[S, A] = Parsing[P].ap(pf)(pb)
  }

  // Instances
  implicit def textForString(implicit O: Ordering[String]): Text[String] with Ordering[String] = new Text[String] with Ordering[String] {
    override def showList(as: List[String]): ShowS = {s: String => as.mkString(s)}
    override def shows(a: String): ShowS = {s: String => s + a}
    override def show(a: String): String = a
    override def compare(x: String, y: String): Int = O.compare(x, y)
  }
  implicit def textForChar(implicit O: Ordering[Char]): Symbol[Char] = new Symbol[Char] {
    override def showList(as: List[Char]): ShowS =  {s: String => as.mkString}
    override def shows(a: Char): ShowS = {s: String => a.toString ++ s}
    override def show(a: Char): String = a.toString
    override def compare(x: Char, y: Char): Int = O.compare(x,y)
  }

  // actual thingos
  type LC = List[Char]
  private def concat(a: LC): (LC => LC) = { b: LC => a ++ b }

  def sym[P[_, _]: Parsing, S: Symbol, A](s: S): P[S, List[S]] =
    Parsing[P].symbol[S](s).map(List.apply(_))

  def stats[P[_, _]: Parsing]: P[Char, List[Char]] =
    chainr(stat, {s: LC => x: LC => y: LC => x++s++y } `<$>` sym(';'))

  def stat[P[_, _]: Parsing]: P[Char, List[Char]] = if_stat <|> while_stat <|> assignment // <?> ("<stat>", inserted "<stat>")

  def if_stat[P[_, _]: Parsing]: P[Char, List[Char]] = {
    val f: LC => LC => LC => LC => LC => LC = i => c => tp => ep => fs => i ::: c ::: tp ::: ep ::: fs
    (f `<$>` sym('I')) <*> cond <*> then_part <*> else_part <*> sym('F')
  }


  def then_part[P[_, _]: Parsing]: P[Char, List[Char]] = {
    val f: LC => LC => LC = t => ss => t ::: ss
    (f `<$>` sym('T')) <*> stats
  }

  def else_part[P[_, _]: Parsing]: P[Char, List[Char]] = {
    val f: LC => LC => LC = {e => ss => e ::: ss}
    (f `<$>` sym('E')) <*> stats opt List.empty
  }


  def while_stat[P[_, _]: Parsing]: P[Char, List[Char]] = {
    val f: LC => LC => LC => LC => LC => LC =  {w => { c => { d => { ss => { o => w ++ c ++ d ++ ss ++ o}}}}}
    (f `<$>` sym('W')) <*> cond <*> sym('D') <*> stats <*> sym('O')
  }

  def assignment[P[_, _]: Parsing]: P[Char, List[Char]] = sym('a')

  def cond[P[_, _]: Parsing]: P[Char, List[Char]] = sym('c')


  // more instances



  def main(args: Array[String]): Unit = {

  }

}
