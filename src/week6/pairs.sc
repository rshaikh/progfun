def isPrime(n: Int): Boolean = (2 until n) forall (n % _ != 0)

val n = 7
(1 until 7) flatMap (i =>
  (1 until i) map(j => (i, j))) filter (pair => isPrime(pair._1 + pair._2))

/**
  * Above implementation is not that readable
  * We can user For Expression to achieve the same result
  * Syntax - for (generator filter) yields result
  * Here:
  * generator - an expression returning a seq. c <- expression
  * filter - a predicate which will used to filter the elements
  *
  * for expression is combination of filter and map
  */

for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)