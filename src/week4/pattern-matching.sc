trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(x, y) => x.eval + y.eval
  }
}

case class Number(n: Int) extends Expr {

}

case class Sum(e1: Expr, e2: Expr) extends Expr {
}

case class Prod(e1: Expr, e2: Expr) extends Expr {
}

case class Var(x: String) extends Expr {

}

def show(e: Expr): String = e match {
  case Number(n) => n.toString
  case Sum(x, y) => show(x) + " + " + show(y)
  case Prod(a, b) => "%s * %s".format(
    a match {
      case Sum(_, _) => "(%s)".format(show(a))
      case _ => show(a)
    },
    b match {
      case Sum(_, _) => "(%s)".format(show(b))
      case _ => show(b)
    }
  )
  case Var(x) => x
}

Sum(Number(10), Number(20)).eval

show(Sum(Number(10), Number(20)))

Number(20).eval

show(Number(20))

show(Sum(Number(10), Sum(Number(1), Number(40))))

Sum(Number(10), Sum(Number(1), Number(40))).eval

show(Sum(Prod(Number(2), Var("x")), Var("y")))

show(Prod(Sum(Number(2), Var("x")), Var("y")))

/**
  * Here we can put eval in base trait or in each subclass
  * Both of these approaches have trade-offs
  * This problem is known as "Expression Problem"
  */

