def isort(input: List[Int]): List[Int] = input match {
  case List() => List()
  case x :: xs => insert(x, isort(xs))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x <= y) x :: y :: ys else y :: insert(x, ys)
}

isort(List(5, 2, 3, 2, 1, 0, 99))
