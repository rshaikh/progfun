abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat

  def successor: Nat = new Succ(this)

  def +(that: Nat): Nat = that

  def -(that: Nat): Nat
}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def predecessor: Nat = throw new IllegalArgumentException


  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = {
    if (that == Zero) this
    else throw new Error("")
  }
}

class Succ(n: Nat) extends Nat{
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def +(that: Nat): Nat = ???

  override def -(that: Nat): Nat = ???
}