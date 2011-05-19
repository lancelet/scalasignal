package signal

import org.scalatest.FunSuite

class FiltFiltTest extends FunSuite {

  import FilterTest._

  test("filtfilt is called on data that is too short") {
    intercept[IllegalArgumentException] {
      FiltFilt.filtfilt(List(0.33, 0.33, 0.33), List(1), List(1., 2, 3))
    }
  }

  test("apply filtfilt with simple 2-sample boxcar filter (FIR)") {
    // filter coefficients
    val b = List(0.5, 0.5)
    // test data
    val x = List[Double](1, 3, 4, 4, 6, 1, 8, 13, 2, 5, 5000)
    // expected output (generated using Octave)
    val yExpected = List[Double](1, 2.75, 3.75, 4.5, 4.25, 4, 7.5, 9, 5.5, 1253, 5000)
    // apply filtfilt and check output
    val y = FiltFilt.filtfilt(b, List[Double](1), x)
    assert(doubleItAeq(y, yExpected))
  }

  test("apply filtfilt with simple 2nd order Butterworth coefficients (IIR)") {
    // externally generated 2nd order Butterworth filter coefficients
    val b = List(0.13111, 0.26221, 0.13111)
    val a = List(1.0, -0.74779, 0.27221)
    // test data
    val x = List[Double](10, 13, 12, 12, 13, 11, 15, 19, 15, 14, 13, 12, 11)
    // expected output (generated using Octave)
    val yExpected = List[Double](9.9912, 11.2425, 11.9709, 12.2597, 12.6335, 
				 13.5644, 14.9197, 15.8455, 15.6602, 14.5896,
				 13.2717, 12.0638, 10.9936)
    // apply filtfilt and check output
    val y = FiltFilt.filtfilt(b, a, x)
    assert(doubleItAeq(y, yExpected, 1.0E-4))  // check to 1.0E-4: precis of Octave output
  }

  test("apply a low-pass 2nd order Butterworth filtfilt to an ECG phantom signal") {
    // this is a comparison with Matlab data
    val a = List(1, -1.8227, 0.8372)
    val b = List(0.0036, 0.0072, 0.0036)
    val y = FiltFilt.filtfilt(b, a, ECG.noisyecg)
    val yExpected = ECG.butter2filtfilt
    assert(doubleItAeq(y, yExpected, 1e-6))
  }

  test("apply a low-pass 4th order Butterworth SOS filter to an ECG phantom signal") (pending)

}
