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

trait MonoidOp[A] {
  val F: Monoid[A]
  val value: A
  def |+|(a2: A) = F.append(value, a2)
}

implicit def toMonoidOp[A: Monoid](a: A): MonoidOp[A] = new MonoidOp[A] {
  val F: Monoid[A] = implicitly[Monoid[A]]
  val value = a
}

3 |+| 10

import scalaz.Scalaz._
import scalaz._

1.some | 2

Option.empty | 2

Option(1).getOrElse(2)

(1 > 10)? 1 | 2

if (1 > 10) 1 else 2