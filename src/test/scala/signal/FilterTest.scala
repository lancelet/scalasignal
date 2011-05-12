package signal

import collection.immutable.{ Iterable, Stream }
import org.scalatest.FunSuite

class FilterTest extends FunSuite {

  // Tests for approximate equality between two Doubles.
  def doubleAeq(a: Double, y: Double, tol: Double = 1.0E-10): Boolean =
    (a + tol >= y) && (a - tol <= y)

  // Tests for approximate equality between two Double iterables.
  def doubleItAeq(a: Iterable[Double], b: Iterable[Double], tol: Double = 1.0E-10): Boolean = 
    (a.size == b.size) && (a zip b).forall { ab => doubleAeq(ab._1, ab._2, tol) }

  test("apply an FIR filter with no initial state") {
    val a = List(1.0)
    val b = List(0.25, 0.50, 0.75)
    val x = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10.0)
    val y = Filter.filter(b, a, x)
    val yExpected = List(0.25, 1, 2.5, 4, 5.5, 7, 8.5, 10, 11.5, 13.0)
    assert(doubleItAeq(y, yExpected))
  }

  test("apply an FIR filter with specified initial state") {
    val a = List(1.0)
    val b = List(0.25, 0.50, 0.75)
    val x = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10.0)
    val si = List(5, 7.)
    val y = Filter.filter(b, a, x, Some(si))
    val yExpected = List(5.25, 8, 2.5, 4, 5.5, 7, 8.5, 10, 11.5, 13)
    assert(doubleItAeq(y, yExpected))
  }

  test("apply an IIR filter with no initial state") {
    val a = List(1., 5, 7)
    val b = List(0.25, 0.50, 0.75)
    val x = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10.)
    val y = Filter.filter(b, a, x)
    val yExpected = List(0.25, -0.25, 2, -4.25, 12.75, -27, 54.25, -72.25, -7, 553.75)
    assert(doubleItAeq(y, yExpected))
  }

  test("apply an IIR filter with specified initial state") {
    val a = List(1., 5, 7)
    val b = List(0.25, 0.50, 0.75)
    val x = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10.)
    val si = List(-3., 4)
    val y = Filter.filter(b, a, x, Some(si))
    val yExpected = List(-2.75, 18.75, -72, 232.75, -654.25, 1649, -3656.75,
			 6750.75, -8145, -6517.25)
    assert(doubleItAeq(y, yExpected))
  }

  test("filter is called with an invalid initial state") {
    val a = List(1., 5, 7)
    val b = List(0.25, 0.50, 0.75)
    val x = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10.)
    val si = List(-3., 4, 11)  // wrong length
    intercept[IllegalArgumentException] {
      Filter.filter(b, a, x, Some(si))
    }
  }

  test("filter should be lazy where possible; requesting x elements only when required") {
    // to test the lazy behaviour, we will create a custom Iterable[Double] that keeps
    //  track of the number of elements requested.  this is slightly kludgy, as it depends
    //  upon un-documented features of Stream, but it can be adapted if Stream changes.
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
    val inStream = Stream() ++ trackingIterable  // trackingIterable is not eval'd
    val a = List(1.0)
    val b = List(0.5, 0.5)
    val si = List(0.0)
    val y = Filter.filter(b, a, inStream, Some(si))
    assert(trackingIterable.pullCount === 2)  // stream will pull 2 to begin with
    assert(y.isInstanceOf[Stream[Double]])
    y.drop(5)  // force evaluation
    assert(trackingIterable.pullCount === 7)
    y.drop(6)  // force evaluation
    assert(trackingIterable.pullCount === 8)
  }

}
