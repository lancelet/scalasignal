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

  test("apply a 4-th order Butterworth filter with an SOSFilt stack") (pending)

  test("SOSFilt evaluation should be lazy where possible") (pending)

}
