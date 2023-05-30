package scalashop

import java.util.concurrent.*
import scala.util.DynamicVariable

import org.scalameter.*
import scala.language.postfixOps
import scala.math._

/** The value of every pixel is represented as a 32 bit integer. */
type RGBA = Int

/** Returns the red component. */
def red(c: RGBA): Int = (0xff000000 & c) >>> 24

/** Returns the green component. */
def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

/** Returns the blue component. */
def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

/** Returns the alpha component. */
def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

/** Used to create an RGBA value from separate components. */
def rgba(r: Int, g: Int, b: Int, a: Int): RGBA =
  (r << 24) | (g << 16) | (b << 8) | (a << 0)

/** Restricts the integer into the specified range. */
def clamp(v: Int, min: Int, max: Int): Int =
  if v < min then min
  else if v > max then max
  else v

/** Image is a two-dimensional matrix of pixel values. */
class Img(val width: Int, val height: Int, private val data: Array[RGBA]):
  def this(w: Int, h: Int) = this(w, h, new Array(w * h))
  def apply(x: Int, y: Int): RGBA = data(y * width + x)
  def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c

/** Computes the blurred RGBA value of a single pixel of the input image. */
def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA =

  // TODO implement using while loops
  // var eachX = clamp(x - radius, 0, x + radius)
  // var eachY = clamp(y - radius, 0, y + radius)
  // var srcWidth = src.width
  // var srcHeight = src.height
  // var sumR = 0
  // var sumG = 0
  // var sumB = 0
  // var sumA = 0
  // var count = 0
  // var minY = min((y + radius), (srcHeight - 1))
  // var minX = min((x + radius), (srcWidth - 1))
  // /** ah, i need 'support' from net for:
  //   src(eachX, eachY)
  // */
  // while eachY <= clamp((y + radius), 0, (srcHeight - 1)) do
  //   // if eachY >= 0 then
  //   while eachX <= clamp((x + radius), 0, (srcWidth - 1)) do
  //     // if eachX >=0 then
  //     sumR = sumR + red(src(eachX, eachY))  
  //     sumG = sumG + green(src(eachX, eachY))
  //     sumB = sumB + blue(src(eachX, eachY))
  //     sumA = sumA + alpha(src(eachX, eachY))
  //     count = count + 1
  //     eachX += 1
  //   eachY = eachY + 1
  // rgba(sumR / count, sumG / count, sumB / count, sumA / count)

  var srcWidth = src.width
  var srcHeight = src.height
  var sumR = 0
  var sumG = 0
  var sumB = 0
  var sumA = 0
  var count = 0

  for (j <- (-radius) to radius) {
    var yj = clamp(y + j, 0, srcHeight - 1)
    for (i <- (-radius) to radius) {
      var xi = clamp(x + i, 0, srcWidth - 1)
      sumR = sumR + red(src(xi, yj))  
      sumG = sumG + green(src(xi, yj))
      sumB = sumB + blue(src(xi, yj))
      sumA = sumA + alpha(src(xi, yj))
      count = count + 1
      xi += 1
    }
    yj += 1
  }

  rgba(sumR / count, sumG / count, sumB / count, sumA / count)


  //   val pixels = {
  //   for (
  //     i <- -radius to radius;
  //     j <- -radius to radius
  //   ) yield (scalashop.clamp(x + i, 0, src.width - 1), scalashop.clamp(y + j, 0, src.height - 1))
  // }.distinct.map({
  //   case (x, y) =>
  //     val pixel = src(x, y)
  //     (red(pixel), green(pixel), blue(pixel), alpha(pixel))
  // })

  // rgba(
  //   pixels.map(_._1).sum / pixels.length,
  //   pixels.map(_._2).sum / pixels.length,
  //   pixels.map(_._3).sum / pixels.length,
  //   pixels.map(_._4).sum / pixels.length
  // )
val forkJoinPool = ForkJoinPool()

abstract class TaskScheduler:
  def schedule[T](body: => T): ForkJoinTask[T]
  def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
    val right = task {
      taskB
    }
    val left = taskA
    (left, right.join())

class DefaultTaskScheduler extends TaskScheduler:
  def schedule[T](body: => T): ForkJoinTask[T] =
    val t = new RecursiveTask[T] {
      def compute = body
    }
    Thread.currentThread match
      case wt: ForkJoinWorkerThread =>
        t.fork()
      case _ =>
        forkJoinPool.execute(t)
    t

val scheduler =
  DynamicVariable[TaskScheduler](DefaultTaskScheduler())

def task[T](body: => T): ForkJoinTask[T] =
  scheduler.value.schedule(body)

def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
  scheduler.value.parallel(taskA, taskB)

def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) =
  val ta = task { taskA }
  val tb = task { taskB }
  val tc = task { taskC }
  val td = taskD
  (ta.join(), tb.join(), tc.join(), td)
