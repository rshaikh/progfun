trait List[T] {

  def isEmpty: Boolean

  def head: T

  def tail: List[T]

}

object List {
  def apply[Int](x1: Int, x2: Int): List[Int] = new Cons[Int](x1, new Cons[Int](x2, new Nil[Int]))
}

class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException("Nil.head")

  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false
}

List(1, 2)
