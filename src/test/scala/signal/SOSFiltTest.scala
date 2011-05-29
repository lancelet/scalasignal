package signal

import org.scalatest.FunSuite
import scalala.tensor.dense.DenseMatrix

class SOSFiltTest extends FunSuite {

  import Comparisons._

  test("apply a 2-sample boxcar filter with an SOSFilt") {
    val filter = SOSFilt(0.5, 0.5, 0.0, 0.0, 0.0)
    val result = filter(List(1., 2, 3, 4, 5, 6, 7, 8, 9, 10))
    val expected = List(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5)
    eqd(result, expected)
  }

  test("apply a 4-th order Butterworth filter with an SOSFilt stack") {
    // generate the filters using butterSOSEven
    val fStack = Butter.butterSOSEven(4, 0.2)
    // test data
    val x = List(1., 2, 3, 4, 5, 6, 7, 8, 9, 10)
    // expected output (generated using Octave)
    val yExpected = List(0.0048243, 0.0403774, 0.1665251, 0.4606177, 
			 0.9793515, 1.7315426, 2.6772461, 3.7467150,
			 4.8657874, 5.9763508)
    val y = SOSFilt.sosfilt(fStack, x)
    eqd(y, yExpected, 1e-7)
  }

  test("apply a 4th order low pass Butterworth filter to an ECG phantom") {
    // this is a comparison with Matlab data
    val filters = Butter.butterSOSEven(4, 0.08)
    val y = SOSFilt.sosfilt(filters, ECG.noisyecg)
    val gain = 1.8322e-4 // gain from Matlab
    val yExpected = ECG.butter4sosfilt.map(_ * 1.8322e-4)
    eqd(y, yExpected, 1e-4)
  }

  test("apply a 4-th order Butterworth filter specified using a Matrix") {
    // this is a comparison with Matlab data
    val filter = DenseMatrix(
      Array(1, 2, 1, 1, -1.5752, 0.6263),
      Array(1, 2, 1, 1, -1.7688, 0.8262)
    )
    val x = List(1., 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val y = SOSFilt.sosfilt(filter, x)
    val yExpected = List(0.0010, 0.0093, 0.0440, 0.1420, 0.3582, 0.7612,
			 1.4265, 2.4282, 3.8372, 5.7059).map(_ * 1000.0)
    eqd(y, yExpected, 1.0)
  }

  test("SOSFilt evaluation should be lazy where possible") {
    // create Iterable[Double] that keeps track of number of requested elements
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

    // create lazy evaluation scenario
    val inStream = Stream() ++ trackingIterable
    val filter = DenseMatrix(
      Array(1, 2, 1, 1, -1.5752, 0.6263),
      Array(1, 2, 1, 1, -1.7688, 0.8262)      
    )
    val y = SOSFilt.sosfilt(filter, inStream)
    assert(trackingIterable.pullCount === 1)
    assert(y.isInstanceOf[Stream[_]])
    y.drop(5)
    assert(trackingIterable.pullCount === 6)
    y.drop(6)
    assert(trackingIterable.pullCount === 7)
  }

}
