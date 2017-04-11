def abs(x: Double): Double = if (x < 0) -x else x

def sqrt(x: Double): Double = {
  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improveGuess(guess))

  def isGoodEnough(guess: Double): Boolean =
    abs(guess * guess - x) / x < 0.001

  def improveGuess(guess: Double): Double =
    (guess + x / guess) / 2

  sqrtIter(1.0)
}
sqrt(2)
sqrt(49)


def factorial(n: Double): Double = {
  fact(1, n)
}

def fact(a: Double, n: Double): Double = {
  if (n == 0) a
  else {
    val acc = a * n
    fact(acc, n - 1)
  }
}

factorial(5)