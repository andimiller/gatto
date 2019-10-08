package net.andimiller.gatto

trait CanParseFrom[S, T] {
  def take1(s: S): Option[(S, T)]
  def combine(ts: List[T]): S
  def split(s: S): List[T]
  def orElse(s1: S, s2: S): S
}

object CanParseFrom {
  implicit val canParseFromString: CanParseFrom[String, Char] = new CanParseFrom[String, Char] {
    override def take1(s: String): Option[(String, Char)] = s.headOption.map(h => (s.tail, h))
    override def combine(ts: List[Char]): String          = ts.mkString
    override def split(s: String): List[Char]             = s.toCharArray.toList
    override def orElse(s1: String, s2: String): String   = if (s1.length < s2.length) s1 else s2
  }

  implicit def canParseFromList[T]: CanParseFrom[List[T], T] = new CanParseFrom[List[T], T] {
    override def take1(s: List[T]): Option[(List[T], T)] = s.headOption.map(h => (s.tail, h))
    override def combine(ts: List[T]): List[T]           = ts
    override def split(s: List[T]): List[T]              = s
    override def orElse(s1: List[T], s2: List[T]): List[T] = if (s1.length < s2.length) s1 else s2
  }
}
