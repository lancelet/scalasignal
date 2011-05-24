package signal

import org.scalatest.FunSuite
import scalala.scalar._

class FFTTest extends FunSuite {

  import Comparisons._
  
  test("apply an FFT to a sequence of Doubles") {
    val x = List[Double](3, 5, 2, 8, 7, 9, 3, 1)
    val hExpected = List[Complex](
      38, 
      -11.7782 - 1.1213 * i,
      5 - 5 * i,
      3.7782 - 3.1213 * i,
      -8,
      3.7782 + 3.1213 * i,
      5 + 5 * i,
      -11.7782 + 1.1213 * i
    )
    val h = FFT.fft(x)
    eqc(h, hExpected, 1e-4)
  }
  
  test("apply an inverse FFT") {
    val xOriginal = List[Double](3, 5, 2, 8, 7, 9, 3, 1)
    val h = FFT.fft(xOriginal)
    val x = FFT.ifft(h)
    eqc(x, xOriginal.map(implicitly[Complex](_)))
  }
  
  test("check that FFT complains about a non power-of-2 length") {
    val x = List[Double](1, 6, 7, 3, 4, 9, 6, -5, -1)
    intercept [IllegalArgumentException] {
      FFT.fft(x)
    }
  }
  
}
