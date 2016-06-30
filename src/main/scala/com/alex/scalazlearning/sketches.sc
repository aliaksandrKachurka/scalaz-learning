import scalaz.std.list._
import scalaz.syntax.bind._

List(List(1)).join

List(true, false).ifM(List(0, 1), List(2, 3))
