package signal

import org.scalatest.FunSuite

class WindowTest extends FunSuite {
    
  import Comparisons._
  
  test("tukeywin") {
    // for a <= 0, tukeywin should be a rectangular window
    eq(Window.tukeywin(4, 0), List(1, 1, 1, 1))
    eq(Window.tukeywin(4, -1), List(1, 1, 1, 1))
    
    // try an intermediate tukey
    val tukey7 = List[Double](0, 0.22873, 0.70564, 0.99442, 1, 1, 0.99442,
      0.70564, 0.22873, 0) // from Octave
    eqd(Window.tukeywin(10, 0.7), tukey7, 1e-5)

    // try the default tukey window
    val tukey5 = List[Double](0, 0.14645, 0.5, 0.85355, 1, 1, 1, 1, 1, 1, 1, 
      1, 1, 0.85355, 0.5, 0.14645, 0) // from Octave
    eqd(Window.tukeywin(17), tukey5, 1e-5)
    
    // for tukey >= 1, the result should be a Hann window
    val tukey1 = List[Double](0, 0.25, 0.75, 1, 0.75, 0.25, 0)
    eqd(Window.tukeywin(7, 1), tukey1)
    eqd(Window.tukeywin(7, 2), tukey1)
    
    // check that tukey complains if n <= 0
    intercept[IllegalArgumentException] {
      Window.tukeywin(0, 0.5)
    }
  }
  
  test("hann") {
    // test an 8-sample window
    val hann8 = List[Double](0, 0.18826, 0.61126, 0.95048, 0.95048, 0.61126,
      0.18826, 0)
    eqd(Window.hann(8), hann8, 1e-5)
    
    // test a 1-sample window
    eqd(Window.hann(1), List[Double](1))
    
    // test a 9-sample periodic window
    val hann9p = List[Double](0, 0.11698, 0.41318, 0.75, 0.96985, 0.96985,
      0.75, 0.41318, 0.11698)
    eqd(Window.hann(9, true), hann9p, 1e-5)
    
    // check that hann complains if n <= 0
    intercept[IllegalArgumentException] {
      Window.hann(0)
    }
  }
  
}