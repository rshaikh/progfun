def msort[T](xs: List[T])(op: (T, T) => Boolean): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    val (fst, snd) = xs splitAt n
    merge[T](msort(fst)(op), msort(snd)(op))(op)
  }
}


def merge[T](xs: List[T], ys: List[T])(op: (T, T) => Boolean): List[T] =
  (xs, ys) match {
    case (Nil, _) => ys
    case (_, Nil) => xs
    case (x :: xs1, y :: ys1) =>
      if (op(x,y)) x :: merge(xs1, ys)(op)
      else y :: merge(xs, ys1)(op)
  }

msort(List(100,7,5,99,12))((x, y) => x < y)
msort(List("z", "b", "a"))((x, y) => x.compareTo(y) < 0)