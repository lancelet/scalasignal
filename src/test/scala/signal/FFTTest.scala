package signal

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import breeze.math._

class FFTTest extends FunSuite with ShouldMatchers {

  import Comparisons._
  
  test("apply an FFT to a sequence of Doubles") {
    val x = List[Double](3, 5, 2, 8, 7, 9, 3, 1)
    val hExpected = List[Complex](  // from Octave
      38.0 + 0.0 * i,
      -11.7782 - 1.1213 * i,
      5.0 - 5.0 * i,
      3.7782 - 3.1213 * i,
      -8.0 + 0.0 * i,
      3.7782 + 3.1213 * i,
      5.0 + 5.0 * i,
      -11.7782 + 1.1213 * i
    )
    val h = FFT.fft(x)
    eqc(h, hExpected, 1e-4)
  }
  
  test("apply an inverse FFT") {
    val xOriginal = List[Double](3, 5, 2, 8, 7, 9, 3, 1)
    val h = FFT.fft(xOriginal)
    val x = FFT.ifft(h)
    eqc(x, xOriginal.map(Complex(_, 0.0)))
  }
  
  test("apply FFT to an even non-power-of-two length") {
    val x = List[Double](1, 2, 3, 4)
    val hExpected = List[Complex](10 + 0 * i, -2 + 2 * i, -2 + 0 * i, -2 - 2 * i)
    val h = FFT.fft(x)
    eqc(h, hExpected)
  }
  
  test("apply FFT to an odd non-power-of-two length") {
    val x = List[Double](1, 6, 7, 3, 4, 9, 6, -5, -1)
    val h = FFT.fft(x)
    val hExpected = List[Complex](  // from Octave
      30 + 0 * i,
      -11.53849 - 12.00903 * i,
      5.44743 - 16.80991 * i,
      8.66025 * i,
      -4.40895 + 2.99335 * i,
      -4.40895 - 2.99335 * i,
      -8.66025 * i,
      5.44743 + 16.80991 * i,
      -11.53849 + 12.00903 * i
    )
    eqc(h, hExpected, 1e-5)
  }
  
  test("apply FFT to an ECG phantom") {
    eqc(FFT.fft(ECG.noisyecg), ECG.fft)
  }
  
  test("nextPow2") {
    FFT.nextPow2(1) should be (2)
    FFT.nextPow2(2) should be (2)
    FFT.nextPow2(13) should be (16)
    FFT.nextPow2(16) should be (16)
  }
  
}
