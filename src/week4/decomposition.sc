trait Expr {
  def isNumber: Boolean

  def isSum: Boolean

  def numValue: Int

  def leftOp: Expr

  def rightOp: Expr
}

class Number(n: Int) extends Expr {
  override def isNumber: Boolean = true

  override def isSum: Boolean = false

  override def numValue: Int = n

  override def leftOp: Expr = throw new Error("Number.leftOp")

  override def rightOp: Expr = throw new Error("Number.rightOp")
}

class Sum(e1: Expr, e2: Expr) extends Expr {
  override def isNumber: Boolean = false

  override def isSum: Boolean = true

  override def numValue: Int = throw new Error("Sum.numValue")

  override def leftOp: Expr = e1

  override def rightOp: Expr = e2
}

def eval(e: Expr): Int = {
  if(e.isNumber) e.numValue
  else if(e.isSum) eval(e.leftOp) + eval(e.rightOp)
  else throw new Error("Unrecognised expression" + e)
}

eval(new Sum(new Number(10), new Number(20)))

/**
  * So the problem here starts when we want to implement Operations
  * like Prod anv Var. To implement these operations we need to keep on
  * adding many methods like isNumber, isSum into the trait and implement
  * in all sub classes
  *
  * We can do one of the following
  * 1. Add an instance check and type cast to save addition of methods
  * like isNumber and isSum in base trait
  *
  * 2.We can have object oriented solution by having eval method in each
  * type.
  *
  * In case of second solution think of following scenario
  * a * b + a * c = a * (b + c)
  *
  * This kind of simplification involves the whole subtree not the single node, it means
  * we can't push simplification to single node
  *
  * In order to do this we again need test methods and access methods
  * So, we are back to the original problem
  */
