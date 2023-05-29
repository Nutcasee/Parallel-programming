package scalashop

import java.util.concurrent.*

import org.scalameter.*
import scala.math._

object VerticalBoxBlurRunner:

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
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")


/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur extends VerticalBoxBlurInterface:

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit =
    // TODO implement this method using the `boxBlurKernel` method
    // ???
    for i <- from to (end - 1) do
      for j <- 0 to (src.height - 1) do
        if (i >=0 && i < src.width)
          dst(i,j) = boxBlurKernel(src, i, j, radius)
          // dst.update(i, j, boxBlurKernel(src, i, j, radius))

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit =
    // TODO implement using the `task` construct and the `blur` method
    // ???
    // 1 to 10 by 2   // Range(1, 3, 5, 7, 9)
    val i = max(src.width / numTasks, 1)
    val rangeFromTo = Range(0, src.width) by i
    val tasks = rangeFromTo.map(t => {
      task(blur(src, dst, t, t + i, radius))
    })
    // next line is not original from me...
    tasks.foreach(_.join())
    // tasks foreach (_.join())
    // tasks.map(t => t.join())
