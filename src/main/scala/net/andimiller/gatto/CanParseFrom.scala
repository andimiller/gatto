package net.andimiller.gatto

trait CanParseFrom[S, T] {
  def take1(s: S): Option[(T, S)]
  def combine(ts: List[T]): S
  def split(s: S): List[T]
}

object CanParseFrom {
  implicit val canParseFromString = new CanParseFrom[String, Char] {
    override def take1(s: String): Option[(Char, String)] = s.headOption.map(h => (h, s.tail))
    override def combine(ts: List[Char]): String = ts.mkString
    override def split(s: String): List[Char] = s.toCharArray.toList
  }

  implicit def canParseFromList[T]: CanParseFrom[List[T], T] = new CanParseFrom[List[T], T] {
    override def take1(s: List[T]): Option[(T, List[T])] = s.headOption.map(h => (h, s.tail))
    override def combine(ts: List[T]): List[T] = ts
    override def split(s: List[T]): List[T] = s
  }
}
