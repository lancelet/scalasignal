package signal

import math.{ sin, Pi }
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class PSDTest extends FunSuite with ShouldMatchers {

  test("psd") (pending)
  
  test("bandwidth") {
    /** Here, we construct a signal with a 100 Hz and 200 Hz component.  The
     *  45% bandwidth should be 100 Hz, while the 95% bandwidth should be
     *  200 Hz. */
    val fs = 1024.0  // sample freq
    val t = (0 until fs.toInt).map(_ / fs)  // 1 s of signal
    
    // 100 Hz sine wave; magnitude 1
    val x100 = t.map((x: Double) => sin(2.0 * Pi * x * 100))
    // 200 Hz sine wave; magnitude 1
    val x200 = t.map((x: Double) => sin(2.0 * Pi * x * 200))
    // total signal
    val x = for ((x1, x2) <- x100 zip x200) yield (x1 + x2)

    // 45% bandwidth should be 100 Hz
    PSD.bandwidth(PSD.psd(x, fs), 0.45) should be (100.0)
    // 95% bandwidth should be 200 Hz
    PSD.bandwidth(PSD.psd(x, fs), 0.95) should be (200.0)    
  }
  
}
