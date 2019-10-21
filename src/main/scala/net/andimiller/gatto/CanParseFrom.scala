package net.andimiller.gatto

trait CanParseFrom[S, T] {
  def take1(s: S): Option[(S, T)]
  def takeN(s: S, n: Int): Option[(S, S)]
  def combine(ts: List[T]): S
  def split(s: S): List[T]
  def takeWhile(s: S, f: T => Boolean): (S, S)
  def nonEmpty(s: S): Boolean
  def takeLiteral(s: S, head: S): Option[(S, S)]
}

object CanParseFrom {
  implicit val canParseFromString: CanParseFrom[String, Char] = new CanParseFrom[String, Char] {
    override def take1(s: String): Option[(String, Char)] = s.headOption.map(h => (s.tail, h))
    override def combine(ts: List[Char]): String          = ts.mkString
    override def split(s: String): List[Char]             = s.toCharArray.toList
    override def takeN(s: String, n: Int): Option[(String, String)] =  {
      val r = s.slice(0, n)
      if (r.length == n)
        Some((s.slice(n, s.length), r))
      else
        None
    }
    override def takeWhile(s: String, f: Char => Boolean): (String, String) = {
      val r = s.takeWhile(f)
      (s.drop(r.length) , r)
    }
    def nonEmpty(s: String): Boolean = s.nonEmpty
    def takeLiteral(s: String, head: String): Option[(String, String)] = if (s.startsWith(head)) Some((s.drop(head.length), head)) else None
  }

  implicit def canParseFromList[T]: CanParseFrom[List[T], T] = new CanParseFrom[List[T], T] {
    override def take1(s: List[T]): Option[(List[T], T)] = s.headOption.map(h => (s.tail, h))
    override def combine(ts: List[T]): List[T]           = ts
    override def split(s: List[T]): List[T]              = s
    override def takeN(s: List[T], n: Int): Option[(List[T], List[T])] =
      if (n < s.length)
        Some(s.splitAt(n))
      else
        None
    def takeWhile(s: List[T], f: T => Boolean): (List[T], List[T]) = {
      val r = s.takeWhile(f)
      (s.drop(r.length), r)
    }
    def nonEmpty(s: List[T]): Boolean = s.nonEmpty
    def takeLiteral(s: List[T], head: List[T]): Option[(List[T], List[T])] = if (s.startsWith(head)) Some((s.drop(head.length), head)) else None
  }
}
