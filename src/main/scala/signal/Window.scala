package signal

import math.{ cos, Pi }
import scala.collection.immutable._

object Window {

  /** Hann (Hanning / raised cosine) window.
   *  
   *  The Hann window is a pair of touching cosine functions.  If the
   *  `periodic` flag is set, a window of length `n+1` is computed, and the
   *  first `n` values are returned.
   *  
   *  ===Algorithm===
   *  The Hann window is defined by the equation:
   *  {{{
   *  w(x) = [ 0.5 * (1 - cos(2*Pi*(x)))   if n > 1
   *         [ 1                           if n = 1
   *  }}}
   *  The Hann window is equivalent to a Tukey window with parameter `a=1`.
   *  
   *  @param n number of samples in the window
   *  @param periodic if this is `true`, a window of `n+1` is computed and
   *    the first `n` values are returned
   *  @return window */
  def hann(n: Int, periodic: Boolean = false): IndexedSeq[Double] =
    if (periodic) tukeywin(n + 1, 1).take(n) else tukeywin(n, 1)  

  /** Rectangular window
   *  
   *  A rectangular window is equivalent to no window at all.  It is simply
   *  a sequence of 1.0 repeated `n` times.
   *  
   *  @param n number of samples in the window
   *  @return window */
  def rectwin(n: Int): IndexedSeq[Double] = Vector.fill[Double](n)(1)
    
  /** Tukey (tapered cosine) window.
   * 
   *  Tukey windows are tapered cosines.  The left and right parts of the
   *  window are cosine tapers and the central region is a rectangular window.
   *  The width of each of the two cosine tapers is `a/2`.  If `a<=0`, the
   *  Tukey window becomes equivalent to a box window.  If `a>=1`, the Tukey
   *  window becomes equivalent to a Hann window.
   *  
   *  ===Algorithm===
   *  The Tukey window is defined by the equation:
   *  {{{
   *         [ 0.5 * (1 + cos(2*Pi/a * (x - a/2)))      0 <= x < a/2
   *  w(x) = [ 1                                        a/2 <= x < 1-a/2
   *         [ 0.5 * (1 + cos(2*Pi/a * (x - 1 + a/2)))  1-a/2 <= x <= 1
   *  }}}
   *  where `x` is the fraction along the width of the window.
   *  
   *  @param n number of samples in the window
   *  @param a width parameter for the cosine tapers.  `a` must be between
   *    0 and 1, and represents the sum of the left and right cosine taper
   *    widths
   *  @return window */
  def tukeywin(n: Int, a: Double = 0.5): IndexedSeq[Double] = 
  new IndexedSeq[Double] {
    require(n > 0)
    private val aa = if (a < 0) 0.0 else if (a > 1) 1.0 else a
    private val pi2Ona = 2 * Pi / aa
    private val aOn2 = aa / 2
    val length = n
    def apply(item: Int): Double = {
      val x = item.toDouble / (length - 1)
      if (x < aa / 2) {
        0.5 * (1 + cos(pi2Ona * (x - aOn2)))
      } else if (x >= 1 - aa / 2) {
        0.5 * (1 + cos(pi2Ona * (x - 1 + aOn2)))
      } else {
        1
      }
    }
  }

}