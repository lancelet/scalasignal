package signal

import scala.collection.immutable._
import scala.collection.generic.CanBuildFrom
import edu.emory.mathcs.jtransforms.fft.DoubleFFT_1D
import breeze.math.Complex

/** Fast Fourier Transforms. */
object FFT {
  
  /** Finds the next greatest power of two from a given positive number.
   *  
   *  @param x number from which to search
   *  @return the next greatest power of two */
  def nextPow2(x: Int): Int = {
    require(x >= 0)
    import math.{ pow, ceil, log }
    if (x <= 1) {
      2
    } else {
      pow(2.0, ceil(log(x) / log(2.0))).toInt
    }
  }
  
  /** Converts an array of alternating real and imaginary values to an
   *  `IndexedSeq[Complex]`. */
  private class PackedComplexSeq(a: Array[Double]) 
  extends IndexedSeq[Complex] {
    require(a.length % 2 == 0)
    val length = a.length / 2
    def apply(item: Int) = Complex(a(item * 2), a(item * 2 + 1))
  }
  
  /** `IndexedSeq[Complex]` which handles the JTransforms packing of FFT
   *  data returned by a `realForward` transformation of an array with an
   *  even number of elements.
   *  
   *  This class makes the returned result appear to be symmetric, and
   *  equivalent to a `realForwardFull` transformation. */
  private class PackedSymmetricEvenComplexSeq(a: Array[Double])
  extends IndexedSeq[Complex] {
    require(a.length % 2 == 0)
    private val Nyquist = a.length / 2
    val length = a.length
    def apply(item: Int) = item match {
      case 0 => Complex(a(0), 0)
      case Nyquist => Complex(a(1), 0)
      case _ => {
        val (ii, f) = if (item < Nyquist) (item, 1) else (a.length - item, -1)
        Complex(a(2 * ii), f * a(2 * ii + 1))
      }
    }
  }

  /** `IndexedSeq[Complex]` which handles the JTransforms packing of FFT
   *  data returned by a `realForward` transformation of an array with an
   *  odd number of elements.
   *  
   *  This class makes the returned result appear to be symmetric, and
   *  equivalent to a `realForwardFull` transformation. */
  private class PackedSymmetricOddComplexSeq(a: Array[Double])
  extends IndexedSeq[Complex] {
    require(a.length % 2 == 1)
    private val Nyquistn1 = (a.length - 1) / 2
    private val Nyquistp1 = (a.length + 1) / 2
    val length = a.length
    def apply(item: Int) = item match {
      case 0 => Complex(a(0), 0)
      case Nyquistn1 => Complex(a(a.length - 1), a(1))
      case Nyquistp1 => Complex(a(a.length - 1), -a(1))
      case _ => {
        val (ii, f) = if (item < Nyquistn1) (item, 1)
                      else (a.length - item, -1)
        Complex(a(2 * ii), f * a(2 * ii + 1))
      }
    }
  }
  
  /** Fast Fourier Transformation.
   *  
   *  Currently, the FFT is computed using the JTransforms library.
   *  
   *  @param x signal to transform. `x` must be available as a `Seq[Double]`
   *  @tparam repr representation of the signal.  it must be transformable
   *    to a `Seq[Double]`
   *    
   *  @return FFT of the signal */
  def fft[Repr](x: Repr)
  (implicit xSeq: Repr => Seq[Double]): IndexedSeq[Complex] = {
    val fft = new DoubleFFT_1D(x.length)
    val a = x.toArray
    fft.realForward(a)
    if (a.length % 2 == 0) {
      new PackedSymmetricEvenComplexSeq(a)
    } else {
      new PackedSymmetricOddComplexSeq(a)
    }
  }

  /** Inverse Fast Fourier Transformation.
   *  
   *  Currently, the IFFT is computed using the JTransforms library.
   *  
   *  @param h signal to transform.  `h` must be available as a `Seq[Complex]`
   *  @tparam Repr representation of the signal.  it must be transformable
   *    to a `Seq[Double]`
   *  @return the inverse FFT of the signal */
  def ifft[Repr](h: Repr)
  (implicit hSeq: Repr => Seq[Complex],
   bf: CanBuildFrom[Repr, Complex, Repr]): Repr = {
    val fft = new DoubleFFT_1D(h.length)
    val a = h.map(c => List(c.real, c.imag)).flatten.toArray
    fft.complexInverse(a, true)

    // create a builder to return the same collection type
    val builder = bf(h)
    builder ++= new PackedComplexSeq(a)
    builder.result
  }
  
}
