package signal

import collection.generic.CanBuildFrom
import collection.immutable.{ IndexedSeq, Iterable, Vector }
import scalala.operators.Implicits._
import scalala.operators.{ BinaryOp, OpSub, OpMul, OpNeg, OpSolveMatrixBy, UnaryOp }
import scalala.scalar.Scalar
import scalala.tensor.::
import scalala.tensor.dense.DenseMatrix
import scalala.tensor.dense.DenseMatrix.{ eye, horzcat, vertcat, zeros }

object FiltFilt {

  /** Computes the stable state of the filter.
    *
    * The method used here is described in the following paper:
    *
    * Gustafsson, F. (1996) Determining the Initial States in Forward-Backward
    *   Filtering.  IEEE Transactions on Signal Processing.  44(4):988-992.
    * 
    * @param bNorm numerator coefficients divided by a(0) denominator coefficient
    * @param aNorm denominator coefficients divided by a(0) denominator coefficient
    * @return initial filter states
    * @author Jonathan Merritt <merritt@unimelb.edu.au> */
  private def computeZi[T](bNorm: IndexedSeq[T], aNorm: IndexedSeq[T])
  (implicit s: Scalar[T],
   neg: UnaryOp[T, OpNeg, T],
   sub: BinaryOp[T, T, OpSub, T],
   mul: BinaryOp[T, T, OpMul, T],
   sm: BinaryOp[DenseMatrix[T], DenseMatrix[T], OpSolveMatrixBy, DenseMatrix[T]],
   m: ClassManifest[T]): List[T] = {

    require(aNorm.size == bNorm.size)
    val fo = aNorm.size - 1  // filter order

    val b0 = bNorm.head
    val aTail = aNorm.tail.toArray.asMatrix(fo, 1)  // a(1, 2, ...) as col matrix
    val bTail = bNorm.tail.toArray.asMatrix(bNorm.size - 1, 1)  // b(1, 2, ...) as col matrix
    
    val za = eye(fo) - horzcat(-aTail, vertcat(eye(fo - 1), zeros(1, fo - 1)))
    val zb = bTail - aTail * b0
    val zi = za \ zb

    zi(::, 0).toList
  }

  /** Forward-reverse, zero phase lag digital filtering.
    *
    * Applies a digital filter first in the forward direction and then in the
    * reverse direction to the signal contained in `x`.  The filter's numerator
    * coefficients are contained in `b`, while the denominator coefficients
    * are contained in `a`.  Due to the double application of the filter, the
    * magnitude of its transfer function is squared, while the phase is zero.
    *
    * Transient responses of the signal are removed from the start and end
    * using the method described by:
    *  - Gustafsson, F. (1996) Determining the Initial States in
    *    Forward-Backward Filtering.
    *    ''IEEE Transactions on Signal Processing''.  '''44(4)''':988-992.
    *
    * @param b numerator coefficients of the filter
    * @param a denominator coefficients of the filter
    * @param x signal to filter.  `x.size` must be greater than 3 times the
    *   order of the filter.
    * @tparam T type of the elements of `x`
    * @tparam Repr collection of `x`, which must be available as an
    *   `Iterable[T]`.
    * @tparam That collection of `x` that will be produced by the filtering
    * @see [[scala.signal.Filter.filter]]
    * @author Jonathan Merritt <merritt@unimelb.edu.au> */
  def filtfilt[T, A, B, Repr, That]
  (b: Iterable[B], a: Iterable[A], x: Repr)
  (implicit iterableX: Repr => Iterable[T],
   n: Fractional[T],
   aToT: A => T,
   bToT: B => T,
   s: Scalar[T],
   sub: BinaryOp[T, T, OpSub, T],
   mul: BinaryOp[T, T, OpMul, T],
   neg: UnaryOp[T, OpNeg, T],
   solveMatrixBy: BinaryOp[DenseMatrix[T], DenseMatrix[T], OpSolveMatrixBy, DenseMatrix[T]],
   bf: CanBuildFrom[Repr, T, That],
   m: ClassManifest[T]): That = {

    import n._

    // compute the filter order, and the approximate length of transients
    val filterOrder = math.max(b.size, a.size) - 1
    val tL = 3 * filterOrder   // transient length

    // check that the number of samples exceeds the expected transient length.
    //  we need this so that the startup transients can be removed effectively.
    if (x.size <= tL) throw new IllegalArgumentException(
      "x.size must be > (3 * filter order)"
    )

    // normalize a and b (divide by a0), and pad them out to the same length
    val a0 = a.head
    val aNorm = a.map(implicitly[T](_) / a0).toIndexedSeq.padTo(filterOrder + 1, n.zero)
    val bNorm = b.map(implicitly[T](_) / a0).toIndexedSeq.padTo(filterOrder + 1, n.zero)

    // compute the stable conditions for the filter's state variable (si / x(0)).
    //  see Filter.filter() for more information on the state variable.
    val zi = computeZi(bNorm, aNorm)

    // now we reverse and reflect the ends of the signal, appending and pre-pending
    //  the reversed and reflected data.  this helps to reduce the startup transients
    //  in the filtering by allowing the filter to accomodate itself to the
    //  artificially-added ends before it reaches the true signal in the center.
    val t2 = n.fromInt(2)
    val xStart = x.drop(1).take(tL).toIndexedSeq.reverse.map { q: T => x.head * t2 - q }
    val xEnd = x.takeRight(tL+1).toIndexedSeq.reverse.drop(1).map { q: T => x.last * t2 - q }
    val xx = xStart ++ x ++ xEnd

    // apply the filter in forward and reverse, computing the initial state
    //  vector as appropriate.
    val fwd = Filter.filter(b, a, xx, Some(zi.map(_ * xx.head)))
    val rev = Filter.filter(b, a, fwd.reverse, Some(zi.map(_ * fwd.last)))

    // trim out the central region of the signal (minus the padding for transients)
    val y = rev.reverse.slice(tL, rev.size - tL)

    // build an appropriate collection for the returned result
    val builder = bf(x)
    builder ++= y
    builder.result

  }

}
