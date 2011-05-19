package signal

import annotation.tailrec
import collection.generic.CanBuildFrom
import collection.immutable.{ LinearSeq, Seq }
import scalala.tensor.Matrix

/** Single second order section (SOS) digital filter.
  *
  * The filter is described by the `z`-space transfer function:
  * {{{
  *        b0 + b1*z^{-1} + b2*z^{-2}
  * H(z) = --------------------------
  *          a1*z^{-1} + a2*z^{-2}
  * }}}
  * This is a second order IIR filter with an `a0` coefficient of 1.
  *
  * ===Algorithm===
  * The function uses the Direct Form II Transposed Structure of an IIR filter
  * in order to evaluate the result.  The filter is evaluated as a finite
  * difference equation using a state variable '''z''':
  * {{{
  *  y(m) = b0 * x(m) + z0(m-1)
  * z0(m) = b1 * x(m) + z1(m-1) - a1 * ym
  * z1(m) = b2 * x(m)           - a2 * ym
  * }}}
  * in which `m` is the sample number.
  * 
  * @tparam T type of the filter (must be available as a `Numeric[T]`)
  * @param b0 filter coefficient
  * @param b1 filter coefficient
  * @param b2 filter coefficient
  * @param a1 filter coefficient
  * @param a2 filter coefficient
  * @author Jonathan Merritt <merritt@unimelb.edu.au> */
case class SOSFilt[@specialized(Float,Double) T](b0: T, b1: T, b2: T, a1: T, a2: T)
(implicit n: Numeric[T]) {

  import n._

  /** Applies the filter.
    *
    * The return type of the method is determined by the signal, `x`.  If `x`
    * is a lazy collection (for example, a `Stream`), then the filter is also
    * evaluated lazily.
    *
    * @tparam Repr the type of the signal, which must be available as an
    *   `collection.immutable.Seq[T]`
    * @tparam That the return type, which must have a
    *   `CanBuildFrom[Repr, T, That]`
    * @param x signal to filter
    * @return filtered signal */
  def apply[Repr]
  (x: Repr)
  (implicit seqX: Repr => Seq[T],
   bf: CanBuildFrom[Repr, T, Repr],
   m: ClassManifest[T]): Repr = 
  {
    // filter iterator
    val filterIterator = new Iterator[T] {
      private var (z0, z1) = (n.zero, n.zero)  // initial state
      private val xIterator = x.iterator  // iterator over x signal
      override def hasNext: Boolean = xIterator.hasNext
      override def next(): T = {
	val xm: T = xIterator.next
	val ym: T = b0 * xm + z0
	z0 = b1 * xm + z1 - a1 * ym
	z1 = b2 * xm - a2 * ym
	ym
      }
    }

    // create a builder for the result
    val builder = bf(x)
    builder ++= filterIterator
    builder.result
  }

}


object SOSFilt {

  /** Creates an instance of the filter with a non-unity `a0` coefficient.
    *
    * Normally, `SOSFilter` objects are created with an un-specified `a0`
    * coefficient of 1.  This method allows the user to specify the value
    * of the `a0` coefficient.  All other coefficients are divided by the
    * `a0` value before being passed on to the normal constructor. */
  def apply[T](b0: T, b1: T, b2: T, a0: T, a1: T, a2: T)
  (implicit f: Fractional[T]): SOSFilt[T] = {
    import f._
    SOSFilt(b0 / a0, b1 / a0, b2 / a0, a1 / a0, a2 / a0)
  }

  /** Applies an `SOSFilt` to a signal.
    *
    * @tparam T the type of the signal and filter
    * @tparam Repr the signal representation, which must be available as a
    *   `collection.immutable.Seq[T]`
    * @param sos second order section filter to apply
    * @param x signal to which the filter should be applied
    * @return filtered signal */
  def sosfilt[T, Repr](sos: SOSFilt[T], x: Repr)
  (implicit seqX: Repr => Seq[T],
   bf: CanBuildFrom[Repr, T, Repr],
   m: ClassManifest[T]): Repr = sos(x)

  /** Applies a stack of `SOSFilt`s to a signal.
    *
    * @tparam T the type of the signal and filter
    * @tparam SOSRepr the filter stack representation, which must be available
    *   as a `collection.immutable.LinearSeq[SOSFilt[T]]`
    * @tparam XRepr the signal representation, which must be available as a
    *   `collection.immutable.Seq[T]`
    * @param sos stack of second order section filters to apply
    * @param x signal to which the filter stack should be applied
    * @return filtered signal */
  def sosfilt[T, SOSRepr, XRepr](sos: SOSRepr, x: XRepr)
  (implicit xSeq: XRepr => Seq[T],
   sosSeq: SOSRepr => LinearSeq[SOSFilt[T]],
   bf: CanBuildFrom[XRepr, T, XRepr],
   m: ClassManifest[T]): XRepr = {
    val builder = bf(x)
    builder ++= sosfiltTailRec(sos, x)
    builder.result
  }
  @tailrec
  private def sosfiltTailRec[T](sos: LinearSeq[SOSFilt[T]], x: Seq[T])
  (implicit m: ClassManifest[T]): Seq[T] = {
    val head = sos.head
    val tail = sos.tail
    if (tail.isEmpty) {
      sosfilt(head, x)
    } else {
      sosfiltTailRec(tail, sosfilt(head, x))
    }
  }

  /** Applies SOS filter(s), specified as a `Matrix`.
    *
    * The `sos` matrix of filters should be specified as:
    * {{{
    * [ b00 b01 b02 a00 a01 a02 ]
    * [ b10 b11 b12 a10 a11 a12 ]
    * [           ...           ]
    * [ bN0 bN1 bN1 aN0 aN1 aN2 ]
    * }}}
    * where `N` is the number of stacked filters.
    * 
    * @tparam T the type of the signal and filter
    * @tparam Repr the signal representation, which must be available as a
    *   `collection.immutable.Seq[T]`
    * @param sos matrix of second-order section filter(s) to apply
    * @param x signal to which the SOS filter(s) should be applied
    * @return filtered signal */
  def sosfilt[T, Repr](sos: Matrix[T], x: Repr)
  (implicit xSeq: Repr => Seq[T],
   f: Fractional[T],
   bf: CanBuildFrom[Repr, T, Repr],
   m: ClassManifest[T]): Repr = {
    // check the matrix size
    require(sos.numCols == 6)
    require(sos.numRows >= 1)

    // convert the matrix to a sequence of filters and apply them
    val filterSeq = (for (j <- 0 until sos.numRows) yield {
      SOSFilt(sos(j,0), sos(j,1), sos(j,2), sos(j,3), sos(j,4), sos(j,5))
    }).toList
    sosfilt(filterSeq, x)
  }

}
