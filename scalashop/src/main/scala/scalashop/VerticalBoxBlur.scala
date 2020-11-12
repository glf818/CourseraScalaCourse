package scalashop

import org.scalameter._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    println("nmsl")
    val seqtime = standardConfig measure {
      // VerticalBoxBlur.blur(src, dst, 0, width, radius)
      val f = 0;
      val d = width;
      for (colNum <- f until d) {
        for (rowNum <- 0 until src.height) {
          println(colNum, rowNum);
          // dst(colNum, rowNum) = boxBlurKernel(src, colNum, rowNum, radius);
        }
      }
    }


    println(s"sequential blur time: $seqtime")

    //val numTasks = 32
    //val partime = standardConfig measure {
    //  VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    //}
    //println(s"fork/join blur time: $partime")
    //println(s"speedup: ${seqtime.value / partime.value}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur extends VerticalBoxBlurInterface {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    (from until end).foreach{
      colNum => {
        (0 until src.height).foreach{
          rowNum => {
            dst(colNum, rowNum) = boxBlurKernel(src, colNum, rowNum, radius);
          }
        }
      }
    }
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method
    val t = src.width ;
    if(scala.math.floorDiv(t,numTasks) > 0) {
      val l = (0 to t by scala.math.floorDiv(t, numTasks)).toArray.slice(0, numTasks).toList;
      val tailL = l.tail ++ List(t);
      val endPtList = l.zip(tailL);
      val taskList = endPtList.map { case (b, e) => {
        val tsk = task {
          val result = "nmsl";
          blur(src, dst, b, e, radius);
          result
        }
        tsk
      }
      }
      taskList.foreach(_.join())
    }
    else {
      blur(src,dst, 0, src.width-1, radius);
    }
  }

}
