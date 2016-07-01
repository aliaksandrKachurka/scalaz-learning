trait Monoid[A] {
  def append(a: A, b: A): A
  def zero: A
}

implicit object IntMonoid extends Monoid[Int] {
  override def append(a: Int, b: Int): Int = a + b
  override def zero: Int = 0
}

def sum[A: Monoid](xs: List[A]) = {
  val m = implicitly[Monoid[A]]
  xs.foldLeft(m.zero)(m.append)
}

sum(List(1, 2, 7))