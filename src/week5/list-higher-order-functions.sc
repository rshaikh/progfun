def squareList(xs: List[Int]): List[Int] =
  xs match {
    case Nil => Nil
    case y :: ys => y * y :: squareList(ys)
  }

def squareList1(xs: List[Int]): List[Int] =
  xs map (x => x * x)

squareList(List(2,4,6))
squareList1(List(2,4,6))