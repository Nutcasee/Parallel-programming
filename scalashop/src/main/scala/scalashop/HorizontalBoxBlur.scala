package scalashop

import org.scalameter.*
import scala.math._

object HorizontalBoxBlurRunner:

  val standardConfig = config(
    Key.exec.minWarmupRuns := 5,
    Key.exec.maxWarmupRuns := 10,
    Key.exec.benchRuns := 10,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val radius = 3
    val width = 1920
    val height = 1080
    val src = Img(width, height)
    val dst = Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")

/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur extends HorizontalBoxBlurInterface:

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit =
    // TODO implement this method using the `boxBlurKernel` method
    // some dude know how to code functioning progs so don't have to worry 
    // about corner cases of if (as below)...plus i fucked up i,j order
    for(i <- from until end){
      for(j <- 0 until src.width){
        if (i >= 0 && i < src.height) { 
          dst(j,i) = boxBlurKernel(src, j, i, radius)
          // dst.update(j, i, boxBlurKernel(src, j, i, radius)) 
        } 
      }     
    }
    // for(y <- from until end){
    //   for(x <- 0 until src.width){
    //     dst(x,y) = boxBlurKernel(src, x, y, radius)
    //   }
    // }
    // for {
    //   row <- from until end
    //   column <- 0 until src.width
    // } yield dst.update(column, row, boxBlurKernel(src, column, row, radius))
    
  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit =
    // TODO implement using the `task` construct and the `blur` method
    val i = max(src.height / numTasks, 1)
    val rangeFromTo = Range(0, src.height) by i
    val tasks = rangeFromTo.map(t => {
      task(blur(src, dst, t, t + i, radius))
    })
    tasks.foreach(_.join())
    // tasks foreach (_.join())
    // tasks.map(t => t.join())

