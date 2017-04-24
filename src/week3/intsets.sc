abstract class IntSet {
  def contains(x: Int): Boolean

  def include(x: Int): IntSet

  def union(other: IntSet): IntSet
}

class EmptySet extends IntSet {
  override def contains(x: Int): Boolean = false

  override def include(x: Int): IntSet = new NonEmptySet(x, new EmptySet, new EmptySet)

  override def toString() = "."

  override def union(other: IntSet): IntSet = other
}

class NonEmptySet(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def contains(x: Int): Boolean = {
    if (x > elem) right.contains(x)
    else if (x < elem) left.contains(x)
    else true
  }

  override def include(x: Int): IntSet = {
    if (x > elem) new NonEmptySet(elem, left, right.include(x))
    else if (x < elem) new NonEmptySet(elem, left.include(x), right)
    else this
  }

  override def toString: String = "{" + left + elem + right + "}"

  override def union(other: IntSet): IntSet = ((left union right) union other) include elem
}

//val a = new EmptySet()
//val b = a.include(10)
//val set1 = b.include(15).include(9)
//
//val d = new NonEmptySet(50, new EmptySet, new EmptySet)
//val set2 = d.include(55).include(10)

//set1.union(set2)

val a: Array[NonEmptySet] = Array(new NonEmptySet(1, new EmptySet, new EmptySet))

val b: Array[IntSet] = a
