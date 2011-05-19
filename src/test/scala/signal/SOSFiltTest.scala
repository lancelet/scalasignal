package signal

import org.scalatest.FunSuite

class SOSFiltTest extends FunSuite {

  import FilterTest._

  test("apply a 2-sample boxcar filter with an SOSFilt") {
    val filter = SOSFilt(0.5, 0.5, 0.0, 0.0, 0.0)
    val result = filter(List(1., 2, 3, 4, 5, 6, 7, 8, 9, 10))
    val expected = List(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5)
    assert(doubleItAeq(result, expected))
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
    assert(doubleItAeq(y, yExpected, 1e-7))
  }

  test("apply a 4-th order Butterworth filter specified using a Matrix") (pending)

  test("SOSFilt evaluation should be lazy where possible") (pending)

}
