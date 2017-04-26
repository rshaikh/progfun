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

