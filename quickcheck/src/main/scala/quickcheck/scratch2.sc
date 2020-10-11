import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract sealed class Tree[T] {
  def merge(t: Tree[T]) = Internal(List(this, t))
  def size: Int = this match {
    case Leaf(_) => 1
    case Internal(children) => (children :\ 0) (_.size + _)
  }
}
case class Internal[T](children: Seq[Tree[T]]) extends Tree[T]

case class Leaf[T](elem: T) extends Tree[T]
  {override def  toString:String = elem.toString}


implicit def arbTree[T](implicit a: Arbitrary[T]): Arbitrary[Tree[T]] =
  Arbitrary {
    val genLeaf = for (e <- Arbitrary.arbitrary[T]) yield Leaf(e)
    def genInternal(sz: Int): Gen[Tree[T]] = for {
      n <- Gen.choose(sz/3, sz/2)
      c <- Gen.listOfN(n, sizedTree(sz/2))
    } yield Internal(c)

    def sizedTree(sz: Int) =
      if(sz <= 0) genLeaf
      else Gen.frequency((1, genLeaf), (3, genInternal(sz)))

    Gen.sized(sz => sizedTree(sz))
  }

for (i <- 1 to 10) yield
  arbTree[Int].sample

val propMergeTree = forAll( (t1: Tree[Int], t2: Tree[Int]) =>
  t1.size + t2.size == t1.merge(t2).size)
propMergeTree.check





