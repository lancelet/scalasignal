package signal

import collection.Iterator
import collection.generic.CanBuildFrom
import collection.immutable.{ IndexedSeq, Iterable, Seq }

object Filter {

  /** Applies an FIR or IIR filter.
   *
   *  The filter is described by the `z`-space transfer function:
   *  {{{
   *        b0 + b1*z^{-1} + b2*z^{-2} + ... + bN*z^{-N}
   *  H(z) = --------------------------------------------
   *        a0 + a1*z^{-1} + a2*z^{-2} + ... + aM*z^{-M}
   *  }}}
   *  For an FIR filter, `a0 = 1` and `a(n > 0) = 0`.
   *
   *  This filter function may be applied to any `Iterable[T]`, where `T` has
   *  a `Fractional[T]` representation.  The return type is determined by the
   *  type of `x`.  If `x` is a lazy collection (for example, a `Stream`), the
   *  filter is also evaluated lazily.
   *
   *  ===Algorithm===
   *  The function uses the Direct Form II Transposed Structure of an FIR or
   *  IIR filter.  The filter is evaluated as a finite difference equation
   *  using a state space variable '''z''':
   *  {{{
   *      y(m) =   b(0) * x(m) +   z(0, m-1)
   *   z(0, m) =   b(1) * x(m) +   z(1, m-1) - a(1) * y(m)
   *   z(1, m) =   b(2) * x(m) +   z(2, m-1) - a(2) * y(m)
   *      ...                 ...
   *  z(n-3, m) = b(n-2) * x(m) + z(n-2, m-1) - a(n-2) * y(m)
   *  z(n-2, m) = b(n-1) * x(m)               - a(n-1) * y(m)
   *  }}}
   *  In which `m` is the sample number and `n = max(a.size, b.size)`,
   *  and `z.size = n - 1`.  The initial state of the filter, '''z(0)''', can
   *  optionally be supplied in `si`.  If `si` is not supplied, the initial
   *  state is zero.  In these equations, the `a` and `b` coefficients have
   *  all been normalized by dividing them by `a(0)`.
   *
   *  @param b numerator coefficients of the filter, when the filter is
   *  expressed in Direct Form II structure
   *  @param a denominator coefficients of the filter, when the filter is
   *  expressed in Direct Form II structure
   *  @param x input signal
   *  @param si optional initial state for the filter.  If `si` is supplied, it
   *  should have a length equal to `max(a.size, b.size)`
   *  @tparam T type of the elements of the signal to be filtered
   *  @tparam A type of the elements of `a`
   *  @tparam B type of the elements of `b`
   *  @tparam C type of the elements of `si`
   *  @tparam Repr the type of the signal, which must be available as an
   *   `collection.immutable.Seq[T]`
   *  @return filtered signal
   *
   *  TODO: Make filter specialized in Float and Double.  Currently
   *       causes a compiler crash (Scala 2.9.0).
   *
   *  @author Jonathan Merritt <merritt@unimelb.edu.au>
   */
  def filter[ /* @specialized(Float, Double) */ T, A, B, C, Repr]
  (b: Seq[B], a: Seq[A], x: Repr, si: Option[Seq[C]] = None)
  (implicit seqX: Repr => Seq[T],
   n: Fractional[T],
   aToT: A => T,
   bToT: B => T,
   cToT: C => T,
   bf: CanBuildFrom[Repr, T, Repr],
   m: ClassManifest[T]): Repr = {

    import n._

    val filterIterator = new Iterator[T] {
      // normalize a and b; dividing both by a(0), and pad them with zeros so 
      //  they're the same length as each other
      private val a0: T = a.head
      private val abSz = math.max(a.size, b.size)
      private val aNorm = a.map(implicitly[T](_) / a0).toIndexedSeq.
        padTo(abSz, n.zero)
      private val bNorm = b.map(implicitly[T](_) / a0).toIndexedSeq.
        padTo(abSz, n.zero)

      // create initial state array; use zeroes if it's not provided
      private val z: Array[T] = (if (si.isDefined) {
        si.get.map { implicitly[T](_) }
      } else {
        List.fill(abSz - 1)(n.zero)
      }).toArray
      if (z.size != abSz - 1) {
        throw new IllegalArgumentException(
            "si.size must be (max(a.size, b.size) - 1)")
      }

      // method to update the state array (z) on each iteration
      private def updateZ(z: Array[T],
        bTail: IndexedSeq[T], aTail: IndexedSeq[T],
        xm: T, ym: T) {
        for (i <- 0 until (z.size - 1)) {
          z(i) = bTail(i) * xm + z(i + 1) - aTail(i) * ym
        }
        z(z.size - 1) = bTail(z.size - 1) * xm - aTail(z.size - 1) * ym
      }

      private val b0 = bNorm.head
      private val aTail = aNorm.tail
      private val bTail = bNorm.tail

      private val xIterator = x.iterator

      override def hasNext: Boolean = xIterator.hasNext
      override def next(): T = {
        val xm: T = xIterator.next
        val ym: T = b0 * xm + z(0)
        updateZ(z, bTail, aTail, xm, ym)
        ym
      }
    }

    // we create a builder here; for lazy collections (eg. Stream), the
    //  filterIterator should also be evaluated lazily
    val builder = bf(x)
    builder ++= filterIterator
    builder.result
  }

}
