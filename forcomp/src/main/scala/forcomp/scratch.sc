

import java.util.Random
val rand = new Random

trait Generator[+T] {
  self =>
  def generate: T
  def map[U](f: T=>U): Generator[U] =
    new Generator[U] {
      def generate = f(self.generate)
    }
  def flatMap[S](f : T=>Generator[S]): Generator[S] = new Generator[S]{
    def generate: S = f(self.generate).generate
  }
}



val integers = new Generator[Integer] {
  val rand = new java.util.Random
  def generate = rand.nextInt
}

val integers1 = new Generator[Integer] {
  val rand = new java.util.Random
  def generate = rand.nextInt
}

val integers2 = new Generator[Integer] {
  val rand = new java.util.Random
  def generate = rand.nextInt
}

def booleans = integers map (x => x > 0)

def pairs[T, U](m: Generator[T], n: Generator[U]) =
  m flatMap { xm =>
    n map { xn => (xm, xn) }
  }

val pairGenerator = pairs(integers1, integers2)

for {i <- 1 to 20} yield pairGenerator.generate

def single[T](x:T): Generator[T] = {
  new Generator[T] {
    def generate = x
  }
}

def choose(low: Integer, high: Integer): Generator[Integer] = integers map {x => low + x % (high-low)}
def oneOf[T](xs: T*): Generator[T] = for (x<- choose(0, xs.length)) yield xs(x)

def emptyLists = single(Nil)

type listInt = List[Integer]

def lists: Generator[listInt] = for {
  isEmpty <- booleans
  list <- if (isEmpty) emptyLists else nonEmptyLists
} yield list

def nonEmptyLists: Generator[listInt] = for {
  head <- integers
  tail <- lists
} yield head::tail
for ( i <- 1 to 10) yield lists.generate

abstract class Tree[+T]
case class Leaf[+T](v:T) extends Tree[T]
case class Inner[+T](l:Tree[T], r:Tree[T]) extends Tree[T]

def leafs: Generator[Tree[Integer]] = for{ i <- integers}
  yield {Leaf[Integer](i)}

def inners: Generator[Inner[Integer]] = for {
   lt <- trees
   rt <-  trees
} yield {Inner(lt, rt)}

def trees: Generator[Tree[Integer]] = for {
   flag <- booleans
   t <- if(flag) inners else leafs
} yield t



val innerTrees = for(i <- 1 to 10) yield inners.generate
innerTrees mkString "\n"























































