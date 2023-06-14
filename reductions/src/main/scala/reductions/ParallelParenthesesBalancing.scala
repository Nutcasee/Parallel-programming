package reductions

import scala.annotation.*
import org.scalameter.*

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
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

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface:

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean =
    // ???
    // leapard eats my shit, at traverse below...4 comment line next is irrelevant instantly
    // i know many guys love useing helper 'method' for this, but 
    // imperative code make me feel at home, fuck function-ism..
    // in weak defense of ...For maximum performance, use a while loop in the traverse 
    // method, or make traverse tail-recursive -- do not use a Range.
    var bl = 0
    var i = 0
    while (i < chars.length)
      if (bl < 0)
        false
      else if (chars(i) == '(')
        bl += 1
      else if (chars(i) == ')')
        bl += -1                                                                             
      i += 1
    bl == 0

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean =

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) : (Int,Int) /*: ???*/ = {
      // ???
      var startByOpenOrCloseBracket = false
      if (idx == until) 
        (arg1,arg2)
      else if (chars(idx) == '(')
        
        traverse(idx + 1, until, arg1 + 1, arg2)
      else if (chars(idx) == ')')
        traverse(idx + 1, until, arg1, arg2 + 1)
      else
        traverse(idx + 1, until, arg1, arg2)
      // while (idx < until) {
      //   if (chars(idx) == '(')
      //     arg1 += 1
      //   else if (chars(idx) == ')')
      //     arg2 += 1
      //   idx += 1
      // arg1 - arg2
      // }
    }

    def reduce(from: Int, until: Int) : (Int,Int) /*: ???*/ = {
      // ???
      if (until - from < threshold)
        traverse(from, until, 0, 0)
      else
        var mid = (from + until) / 2
        var ((r1,r2), (r3,r4)) = parallel(reduce(from, mid), reduce(mid, until))
        ((r1 + r3), (r2 + r4))
    }

    var (rl, rr) = reduce(0, chars.length)  // ???
    (rl + rr == 0) && rl >= 0
  // For those who want more:
  // Prove that your reduction operator is associative!

