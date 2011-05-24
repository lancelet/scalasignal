package signal

import collection.generic.CanBuildFrom
import org.apache.commons.math.complex.{ Complex => CComplex }
import org.apache.commons.math.transform.FastFourierTransformer
import scalala.scalar.Complex

/** Fast Fourier Transforms. */
object FFT {

  /** Converts an Apache `Complex` into a Scalala `Complex`. 
   *  TODO: Move this out to more generic implicits when required. */
  private implicit def apacheComplexToScalalaComplex(c: CComplex) =
    Complex(c.getReal, c.getImaginary)

  /** Converts a Scalala `Complex` into an Apache `Complex`.
   *  TODO: Move this out to more generic implicits when required. */
  private implicit def scalalaComplexToApacheComplex(c: Complex) =
    new CComplex(c.real, c.imag)

  /** Check that a number is a power of two. */
  private def isPowerOfTwo(x: Int): Boolean = {
    import math.{ pow, ceil, log }
    val np2 = pow(2.0, ceil(log(x) / log(2.0))).toInt
    np2 == x
  }
    
  /** Fast Fourier Transformation.
   *  
   *  Currently, the FFT is computed using the Apache Commons Math library.
   *  
   *  @param x signal to transform. `x` must be available as a `Seq[Double]`
   *  @tparam repr representation of the signal.  it must be transformable
   *    to a `Seq[Double]`
   *    
   *  @return FFT of the signal */
  def fft[Repr](x: Repr)
  (implicit xSeq: Repr => Seq[Double]): Vector[Complex] = {
    require(isPowerOfTwo(x.length))
    val h = (new FastFourierTransformer()).transform(x.toArray)
    Vector.empty[Complex] ++ h.map(implicitly[Complex](_))
  }

  /** Inverse Fast Fourier Transformation.
   *  
   *  Currently, the IFFT is computed using the Apache Commons Math library.
   *  
   *  @param h signal to transform.  `h` must be available as a `Seq[Complex]`
   *  @tparam Repr representation of the signal.  it must be transformable
   *    to a `Seq[Double]`
   *  @return the inverse FFT of the signal */
  def ifft[Repr](h: Repr)
  (implicit hSeq: Repr => Seq[Complex],
   bf: CanBuildFrom[Repr, Complex, Repr]): Repr = {
    require(isPowerOfTwo(h.length))
    // compute IFFT using Apache Commons Math
    val hApache = h.map(implicitly[CComplex](_))
    val xf = new FastFourierTransformer()
    val x = xf.inversetransform(hApache.toArray)
    
    // create a builder to hold the result
    val builder = bf(h)
    builder ++= x.map(implicitly[Complex](_))
    builder.result
  }
  
}
