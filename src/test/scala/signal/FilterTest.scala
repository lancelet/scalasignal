package signal

import collection.immutable.{ Iterable, Stream }
import org.scalatest.FunSuite
import scalala.scalar.{ Complex => C }

class FilterTest extends FunSuite {

  import Comparisons._

  test("apply an FIR filter with no initial state") {
    val a = List(1)
    val b = List(0.25, 0.50, 0.75)
    val x = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10.0)
    val y = Filter.filter(b, a, x)
    val yExpected = List(0.25, 1, 2.5, 4, 5.5, 7, 8.5, 10, 11.5, 13.0)
    eq(y, yExpected)
  }

  test("apply an FIR filter with specified initial state") {
    val a = List(1)
    val b = List(0.25, 0.50, 0.75)
    val x = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10.0)
    val si = List(5, 7.)
    val y = Filter.filter(b, a, x, Some(si))
    val yExpected = List(5.25, 8, 2.5, 4, 5.5, 7, 8.5, 10, 11.5, 13)
    eq(y, yExpected)
  }

  test("apply an IIR filter with no initial state") {
    val a = List(1, 5, 7)
    val b = List(0.25, 0.50, 0.75)
    val x = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10.)
    val y = Filter.filter(b, a, x)
    val yExpected = List(0.25, -0.25, 2, -4.25, 12.75, -27, 54.25, -72.25, -7, 
    					 553.75)
    eq(y, yExpected)					 
  }

  test("apply an IIR filter with specified initial state") {
    val a = List(1, 5, 7)
    val b = List(0.25, 0.50, 0.75)
    val x = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10.)
    val si = List(-3., 4)
    val y = Filter.filter(b, a, x, Some(si))
    val yExpected = List(-2.75, 18.75, -72, 232.75, -654.25, 1649, -3656.75,
			 6750.75, -8145, -6517.25)
    eq(y, yExpected)	 
  }

  test("filter is called with an invalid initial state") {
    val a = List(1, 5, 7)
    val b = List(0.25, 0.50, 0.75)
    val x = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10.)
    val si = List(-3., 4, 11)  // wrong length
    intercept[IllegalArgumentException] {
      Filter.filter(b, a, x, Some(si))
    }
  }

  test("apply a low-pass 2nd order Butterworth filter to an ECG phantom") {
    // this is a comparison with Matlab data
    val a = List(1, -1.8227, 0.8372)
    val b = List(0.0036, 0.0072, 0.0036)
    val y = Filter.filter(b, a, ECG.noisyecg)
    val yExpected = ECG.butter2filter
    eq(y, yExpected)
    /* // Plot in case of problems
    import scalala.library.Plotting._
    import scalala.scalar.Scalar
    import scalala.tensor.dense.DenseVector
    val x = DenseVector.range(0, ECG.noisyecg.size)
    plot.hold = true
    plot(x, ECG.noisyecg.toArray)
    plot(x, yExpected.toArray)
    plot(x, y.toArray, '.')
    */
  }

  test("filter should be lazy where possible") {
    // to test the lazy behaviour, we will create a custom Iterable[Double] 
    //  that keeps track of the number of elements requested.  this is slightly 
    //  kludgy, as it depends upon un-documented features of Stream, but it can 
    //  be adapted if Stream changes.
    val trackingIterable = new Iterable[Double] {
      private var _pullCount: Int = 0
      def pullCount: Int = _pullCount
      def iterator: Iterator[Double] = new Iterator[Double] {
	override def hasNext: Boolean = true
	override def next(): Double = {
	  _pullCount = _pullCount + 1
	  0.0
	}
      }
    }

    // create a lazy evaluation situation
    val inStream = Stream() ++ trackingIterable  // trackingIterable not eval'd
    val a = List(1)
    val b = List(0.5, 0.5)
    val si = List(0)
    val y: Stream[Double] = Filter.filter(b, a, inStream, Some(si))
    assert(trackingIterable.pullCount === 1)  // stream will pull 1
    assert(y.isInstanceOf[Stream[_]])
    y.drop(5)  // force evaluation
    assert(trackingIterable.pullCount === 6)
    y.drop(6)  // force evaluation
    assert(trackingIterable.pullCount === 7)
  }

  test("apply a filter to a sequence of complex numbers") (pending)

}
