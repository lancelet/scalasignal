package signal

import scala.collection.immutable._
import scala.collection.parallel._
import scala.collection.generic.CanBuildFrom

object Detrend {

  /** Removes a linear trend from a signal. 
   *  
   *  If the signal in `y` has a linear trend, it is removed, and the signal
   *  without a linear trend is returned.
   *  
   *  @param y signal from which to remove a linear trend
   *  @tparam T type of the signal; must be a `Fractional[T]`
   *  @tparam Repr representation of the signal; must be available as a
   *    `Seq[T]`
   *  @return signal with linear trend removed */
  def detrend[T, Repr](y: Repr)
  (implicit seqY: Repr => Seq[T],
   frac: Fractional[T],
   bf: CanBuildFrom[Repr, T, Repr]): Repr = {
    
    import frac._

    // compute linear fit for (index of y) vs (y): y = a + b*x
    val ypar = implicitly[Seq[T]](y).par  // parallel version of y
    val n = ypar.length
    val s = frac.fromInt(n)
    val sx = frac.fromInt(n * (n + 1) / 2 - n)
    val sy = ypar.sum
    val sxx = frac.fromInt(n * (n + 1) * (2 * n + 1) / 6 - n * n)
    val sxy = ypar.zipWithIndex.map(i => i._1 * frac.fromInt(i._2)).sum
    val delta = s * sxx - (sx * sx)
    val a = (sxx * sy - sx * sxy) / delta
    val b = (s * sxy - sx * sy) / delta

    // subtract the fit from y
    def f(x: Int): T = a + frac.fromInt(x) * b
    val yd = ypar.zipWithIndex.map(i => i._1 - f(i._2)).seq
    
    // create a builder to return the result as the same collection type
    val builder = bf(y)
    builder ++= yd
    builder.result
  }
  
}
