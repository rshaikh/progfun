def msort(xs: List[Int]): List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    val (fst, snd) = xs splitAt n
    merge1(msort(fst), msort(snd))
  }
}


def merge1(xs: List[Int], ys: List[Int]): List[Int] =
  (xs, ys) match {
    case (Nil, _) => ys
    case (_, Nil) => xs
    case (x :: xs1, y :: ys1) =>
      if (x < y) x :: merge1(xs1, ys)
      else y :: merge1(xs, ys1)
  }

msort(List(100,7,5,99,12))