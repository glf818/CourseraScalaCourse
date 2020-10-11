package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[Int]
    h <- frequency((2, emptyHeap), (8, genHeap))
  } yield insert(n % 1000, h)

  lazy val emptyHeap: Gen[H] = empty

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }


  property("singleElement") = forAll { i: A =>
    val x = insert(i, empty)
    findMin(x) == i
  }

  property("insertAndDelete") = forAll { i: A =>
    val x = insert(i, empty)
    isEmpty(deleteMin(x))
  }

  property("orderNotMatter") = forAll { (m: Int, n: Int) =>
    val h1 = insert(n, insert(m, empty))
    val h2 = insert(n, insert(m, empty))
    findMin(h1) == findMin(h2)
  }

  property("HeapsMelded") = forAll(genHeap, genHeap) { (h1, h2) =>
    findMin(meld(h1, h2)) == (if (ord.lt(findMin(h1), findMin(h2))) findMin(h1) else findMin(h2))
  }

  property("sortedList") = forAll { l: List[A] =>
    heapEqualsSortedList(makeHeapFromList(l), l.sorted)
  }

  property("compareTwoNumbers") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    val n1 = findMin(h)
    val n2 = findMin(deleteMin(h))
    n1 <= n2
  }

  property("Sorted") = Prop.forAll(genHeap) { (h) =>
    heapSorted(h)
  }

  property("meldMove") = Prop.forAll(genHeap, genHeap) { (h1, h2) =>
    heapsEqual(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

  def makeHeapFromList(l: List[Int]): H =
    addListToHeap(empty, l)

  def addListToHeap(h: H, l: List[Int]): H = l match {
    case Nil     => h
    case x :: xs => addListToHeap(insert(x, h), xs)
  }

  def heapEqualsSortedList(h: H, l: List[Int]): Boolean = l match {
    case Nil     => isEmpty(h)
    case x :: xs => (x == findMin(h)) && heapEqualsSortedList(deleteMin(h), xs)
  }

  def heapsEqual(h1: H, h2: H): Boolean =
    if (isEmpty(h1) && isEmpty(h2))
      true
    else if (isEmpty(h1) && isEmpty(h2))
      false
    else {
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      m1 == m2 && heapsEqual(deleteMin(h1), deleteMin(h2))
    }

  def heapSorted(h: H): Boolean =
    if (isEmpty(h))
      true
    else {
      val min = findMin(h)
      val rest = deleteMin(h)
      isEmpty(rest) || (min <= findMin(rest) && heapSorted(rest))
    }


}
