def sum(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sumF(a + 1, b)
  }

  sumF
}

sum((x: Int) => x)(1, 5)

def sumOfCubes = sum(x => x * x * x)
def sumOfSquare = sum(x => x * x)
sumOfCubes(1, 5)
sumOfSquare(1, 5)

//We don't need intermediate function sumF as we are using multi-param list
def sumMultiParameter(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 0 else f(a) + sumMultiParameter(f)(a + 1, b)
}

sumMultiParameter(x => x)(1, 5)

def product(f: Int => Int)(a: Int, b: Int) : Int = {
  if(a > b) 1 else f(a) * product(f)(a + 1, b)
}

product(x => x)(1, 5)

def fact(n: Int): Int = {
  product(x => x)(1, n)
}

fact(5)

def genericFunction(f: Int => Int,
                    applicationFun: (Int, Int) => Int,
                    zeroVal: Int)(a: Int, b: Int) : Int = {
  if (a>b) zeroVal
  else applicationFun(f(a), genericFunction(f, applicationFun, zeroVal) (a + 1, b))
}

def productUsingGenericFun(f: Int => Int)(a: Int, b:Int) =
  genericFunction(f, (x, y) => x*y, 1)(a, b)

productUsingGenericFun(x => x)(1, 5)