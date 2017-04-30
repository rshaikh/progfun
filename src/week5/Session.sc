def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("last of empty list")
  case List(x) => x
  case y :: ys => last(ys)
}

last(List(1, 2, 3, 4))

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(_) => Nil
  case y :: ys => y :: init(ys)
}

init(List(1, 2, 3, 4))

def removeAt[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n+1)

val numbers = List(1,2,3,4,5,6)

removeAt(4, numbers)

numbers take 4
numbers drop 5

def flatten(xs: List[Any]): List[Any] = xs match {
  case Nil => Nil
  case (head: List[_]) :: tail => flatten(head) ++ flatten(tail)
  case head :: tail => head :: flatten(tail)
}

flatten(List(List(1, 1), 2, List(3, List(5, 8))))

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val (first, rest) = xs span (y => x == y)
    first :: pack(rest)
  }
}

def encode[T](xs: List[T]): List[(T, Int)] = {
  pack(xs) map (x => (x.head, x.length))
}
pack(List("a", "a", "a", "b", "c", "c", "a"))
encode(List("a", "a", "a", "b", "c", "c", "a"))

def concat[T](xs: List[T], ys: List[T]): List[T] =
  (xs foldRight ys) (_ :: _)

concat(List(1,2), List(3,4))

List(2,4,6,8,9) map ( x => x + 1)
List(2,4,6,8,9) map {x => x + 1; 0} // block style return last line as result

List(2,4,6,8,9) map ( _ + 1)
List(2,4,6,8,9) map { _ + 1}
List(2,4,6,8,9) map { 2 + }

def sum(xs: List[Int]) = (0 :: xs) reduceLeft ((x, y) => x + y)

sum(List(1,2,3,4))