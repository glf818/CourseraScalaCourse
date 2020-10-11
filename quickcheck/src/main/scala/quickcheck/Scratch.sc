import org.scalacheck._

object StringSpecification extends Properties("String") {
  import Prop.forAll

  property("startsWith") = forAll { (a: String, b: String) =>
    (a+b).startsWith(a)
  }

  property("endsWith") = forAll { (a: String, b: String) =>
    (a+b).endsWith(b)
  }

  property("substring") = forAll { (a: String, b: String) =>
    (a+b).substring(a.length) == b
  }

  property("substring") = forAll { (a: String, b: String, c: String) =>
    (a+b+c).substring(a.length, a.length+b.length) == b
  }
}

//StringSpecification.main( Array("-n", "200"))

StringSpecification.check()

def myMagicFunction(n: Int, m: Int) = n + m

import org.scalacheck.Prop.{forAll, propBoolean}

val complexProp = forAll { (m: Int, n: Int) =>
  val res = myMagicFunction(n, m)
  (res >= m)    :| "result > #1" &&
    (res >= n)    :| "result > #2"
}
complexProp.check

import org.scalacheck.Prop.{forAll, propBoolean, all}

val propMul = forAll { (n: Int, m: Int) =>
  val res = n*m
  ("evidence = " + res) |: all(
    "div1" |: m != 0 ==> (res / m == n),
    "div2" |: n != 0 ==> (res / n == m),
    "lt1"  |: res > m,
    "lt2"  |: res > n
  )
}

propMul.check
/*
Generators
Generators are responsible
 */

val myGen = for {
  n <- Gen.choose(10, 20)
  m <- Gen.choose(2*n, 500)
} yield (n,m)

for (i <- 1 to 5) yield myGen.sample

val vowel = Gen.oneOf('A', 'E', 'I', 'O', 'U', 'Y')
for( i <- 1 to 10) yield vowel.sample
sealed abstract class Tree
case class Node(left: Tree, right: Tree, v: Int) extends Tree
case object Leaf extends Tree
import Arbitrary.arbitrary
import Gen._

val genLeaf = const(Leaf)

val genNode = for {
  v <- arbitrary[Int]
  left <- genTree
  right <- genTree
} yield Node(left, right, v)

def genTree: Gen[Tree] = oneOf(genLeaf, lzy(genNode))

for {i <- 1 to 2} yield genTree.sample

def matrix[T](g: Gen[T]): Gen[Seq[Seq[T]]] =
  Gen.sized { size =>
    val side = scala.math.sqrt(size).asInstanceOf[Int]
    Gen.listOfN(side, Gen.listOfN(side, g))
  }

val arbInt = oneOf('A','B','c','D')
val listGen = Gen.listOfN(5, arbInt)

matrix(arbInt).sample

val smallEvenInteger = Gen.choose(0,200) suchThat(_%2==0)
for (i <- 1 to 20) yield
smallEvenInteger.sample

val genIntList        = Gen.containerOf[List,Int](Gen.oneOf(1, 3, 5))
(for(i <- 1 to 100)
yield genIntList.sample) map {case Some(x)=> x.length}

listGen.sample
genIntList.sample
val oneOrMoreDigits = Gen.atLeastOne(1 to 9)
oneOrMoreDigits.sample
val zeroOrMoreDigits = Gen.someOf(1 to 9)
zeroOrMoreDigits.sample
val threeLetters = Gen.pick(3, 'A' to 'Z')
threeLetters.sample
import scala.util.Random

val threeLettersPermuted = threeLetters.map(Random.shuffle(_))
val evenInt = Arbitrary.arbitrary[Int] suchThat (_%2 == 0)

for {i <- 1 to 10} yield evenInt.sample















































































