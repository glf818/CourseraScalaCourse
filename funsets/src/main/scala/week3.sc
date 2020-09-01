

println("Week 3 Hiearachy")
val t0 = new NonEmptySet(17, EmptySet, EmptySet)
val t1 = t0 incl 21 incl 18 incl 21 incl 4 incl 10 incl 3 incl 7 incl 19
val t2 = new NonEmptySet( 35, EmptySet, EmptySet) incl 42 incl 61 incl 13 incl 19 incl 27 incl 4 incl 38 incl 22
t2 union t1




abstract class IntSet {
  def incl(x : Int) : IntSet
  def contains(x: Int) : Boolean
  def union(other: IntSet): IntSet
}

object EmptySet extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmptySet(x, EmptySet, EmptySet)
  def union(other: IntSet) : IntSet  = {
    print("Single Right For Union" + other)
    other
  }
  override def toString = "."
}

class NonEmptySet(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x:Int): Boolean = {
    if (elem==x) true
    else if (elem > x) left contains x
    else right contains x
  }

  def incl(x: Int): IntSet = {
    if (x > elem) new NonEmptySet(elem, left, right incl x)
    else if (x < elem) new NonEmptySet(elem, left incl x, right)
    else this
  }

  def union(other: IntSet): IntSet = {
    println(this + "+++" + other)
    if(other==EmptySet) this else
    println(left+"+++"+right)
    ((left union right) union other ) incl elem
  }
  override def toString = "{" + left + elem + right + "}"
}


