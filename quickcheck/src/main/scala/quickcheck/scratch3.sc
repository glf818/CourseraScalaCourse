import org.scalacheck._
import org.scalacheck.Prop
import org.scalacheck.Properties
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.Test.{check, Result, Failed, PropException}
import quickcheck._
import Gen._
import scala.util.Random


object QuickCheckBinomialHeap extends QuickCheckHeap with BinomialHeap
import QuickCheckBinomialHeap._

type Hp = QuickCheckBinomialHeap.H
lazy val emptyHeap: Gen[Hp] = QuickCheckBinomialHeap.empty

lazy val genHp: Gen[Hp] = for {
  n <- arbitrary[Int]
  h <- frequency((2, emptyHeap), (8, genHp))
} yield QuickCheckBinomialHeap.insert(n % 1000, h)

implicit lazy val arbHeap: Arbitrary[Hp] = Arbitrary(genHp)


val y =
forAll { (h: Hp) =>
  val m = if (QuickCheckBinomialHeap.isEmpty(h)) 0 else QuickCheckBinomialHeap.findMin(h)
  val toI = if (QuickCheckBinomialHeap.isEmpty(h)) 0 else m+2
  QuickCheckBinomialHeap.findMin(QuickCheckBinomialHeap.insert(toI, h)) == m
}

//y.check()
val p2 = forAll { (ii: Int) =>
  val i = ii % 1000
  val ept = QuickCheckBinomialHeap.empty
  val hp1 = QuickCheckBinomialHeap.insert(i, ept)
  val hp2 = QuickCheckBinomialHeap.insert(i+1, ept)
  val hpmeld = QuickCheckBinomialHeap.meld(hp1, hp2)
  QuickCheckBinomialHeap.findMin(hpmeld)==i
}
//p2.check

val p3 = forAll { (ii: Int) =>
  val i = ii % 1000
  val ept = QuickCheckBinomialHeap.empty
  val hp1 = QuickCheckBinomialHeap.insert(i, ept)
  val hp2 = QuickCheckBinomialHeap.insert(i+1, ept)
  val hpmeld = QuickCheckBinomialHeap.meld(hp1, hp2)
  QuickCheckBinomialHeap.findMin(QuickCheckBinomialHeap.deleteMin(hpmeld)) > i
}
//p3.check


val nums= Gen.pick(5, 1 to 5)
val numsPermed = nums.map(Random.shuffle(_))
//val whatsthis = for {sInt <- numsPermed} yield sInt.foldRight[Hp](empty)((x, acc)=>insert(x,acc))



implicit lazy val arbSeq: Arbitrary[scala.collection.Seq[Int]] = Arbitrary(numsPermed)
val p4 = forAll {(sInt: scala.collection.Seq[Int]) =>
  val ahp = sInt.foldRight[Hp](empty)((x, acc)=>insert(x,acc))
  (findMin(ahp)< findMin(deleteMin(ahp)))
}

//p4.check
val p5 = forAll { (ii: Int) =>
  val i = ii % 1000
  val ept = empty
  val hp1 = insert(i, ept)
  val hp2 = insert(i-1, ept)
  val hpmeld = meld(hp1, hp2)
  findMin(hpmeld) == i-1
}

p5.check

































