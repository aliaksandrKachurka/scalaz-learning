trait Monoid[A] {
  def append(a: A, b: A): A
  def zero: A
}

implicit object IntMonoid extends Monoid[Int] {
  override def append(a: Int, b: Int): Int = a + b
  override def zero: Int = 0
}

trait FoldLeft[F[_]] {
  def foldLeft[A, B](xs: F[A], b: B, f: (B, A) => B): B
}

object FoldLeft {
  implicit val FoldLeftList: FoldLeft[List] = new FoldLeft[List] {
    def foldLeft[A, B](xs: List[A], b: B, f: (B, A) => B) = xs.foldLeft(b)(f)
  }
}

def sum[M[_]: FoldLeft, A: Monoid](xs: M[A]): A = {
  val m = implicitly[Monoid[A]]
  val fl = implicitly[FoldLeft[M]]
  fl.foldLeft(xs, m.zero, m.append)
}

sum(List(1, 2, 7))