package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var pos  = 0;
    var segNet = 0;
    while(pos < chars.length){
        chars(pos) match {
          case '(' => {segNet = segNet+1}
          case ')' => {segNet = segNet-1;
            if(segNet < 0) {return false;}}
          case _ =>{}
        }
        pos = pos + 1;
      }
    return (segNet==0);
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int)  = {
      var pos  = idx;
      var segNet = 0;
      var mins = 0;
      while(pos < until){
        chars(pos) match {
          case '(' => {segNet = segNet+1}
          case ')' => {segNet = segNet-1;
            if(segNet < mins) {mins = segNet}}
          case _ => {}
        }
        pos = pos + 1;
      }
      (segNet, mins)
    }

    def reduce(from: Int, until: Int): (Int, Int) /*: ???*/ = {
      if ((until - from) <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = (until + from) / 2
        val (a, b) = parallel(reduce(from, mid), reduce(mid, until))
        (a._1 + b._1, Math.min(a._2, a._1 + b._2) )
      }
    }
    val v = reduce(0, chars.length)
    v._1 == 0 && v._2 >= 0

  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
