package reductions

import org.scalameter._

object LineOfSightRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }
}

sealed abstract class Tree {
  def maxPrevious: Float
}

case class Node(left: Tree, right: Tree) extends Tree {
  val maxPrevious = left.maxPrevious.max(right.maxPrevious)
}

case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

object LineOfSight extends LineOfSightInterface {

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
   val tanges = (0 until input.length).map(i=> if(i>0) (input(i)/(i*1.0)).toFloat else 0F).toArray
    var maxV = tanges(0);
    var i = 0;
    while (i < input.length){
      maxV = math.max(maxV, tanges(i))
      output(i) = maxV
      i= i+1
    }

  }

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    var maxV = if (from==0) 0F else (input(from)/from).toFloat
    var i = from + 1
    while(i < until){
      if (maxV < (input(i)/i).toFloat) {
        maxV = (input(i)/i).toFloat
      }
      i= i+1
    }
    maxV
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], from: Int, end: Int,
    threshold: Int): Tree = {
    if( end - from <= threshold) {Leaf(from, end, upsweepSequential(input, from, end))}
    else {
      var mid =from + (end-from) / 2
      val (l, r) = parallel(upsweep(input, from, mid, threshold), upsweep(input, mid, end, threshold))
      Node(l, r)
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float],
    startingAngle: Float, from: Int, until: Int): Unit = {
    output(from) = if(from==0) 0F else math.max(startingAngle, (input(from)/from).toFloat)
    var i = from + 1
    var maxAngle = if(from==0) startingAngle else math.max(startingAngle, (input(from)/from).toFloat)
    while (i < until){
      maxAngle =  math.max(maxAngle , (input(i)/i).toFloat)
      output(i) = maxAngle
      i= i+1
    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepSequential` to write
   *  the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
    tree: Tree): Unit = {
    tree match {
      case Leaf(from, until, maxPrevious) => downsweepSequential(input, output, startingAngle, from, until)
      case Node(left, right) => parallel(downsweep(input, output, startingAngle, left),
                                       downsweep(input, output, math.max(startingAngle,left.maxPrevious),right))
    }
  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
    threshold: Int): Unit = {
    val upTree = upsweep(input, 0,  input.length, threshold)
    downsweep(input, output, 0.0F, upTree)
  }
}
