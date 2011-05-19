package signal

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class ButterTest extends FunSuite with ShouldMatchers {

  test("butterSOSEven is called with an odd order (requires even)") {
    intercept[IllegalArgumentException] { Butter.butterSOSEven(5, 0.5) }
  }

  test("butterSOSEven is called with an out-of-range cutoff frequency") {
    intercept[IllegalArgumentException] { Butter.butterSOSEven(2, -1.0) }
    intercept[IllegalArgumentException] { Butter.butterSOSEven(2,  2.0) }
  }

  test("construct a second-order filter with butterSOSEven") {
    // cutoff frequency of 0.2 (checking using coefficients from Octave)
    val fl1 = Butter.butterSOSEven(2, 0.2)
    assert(fl1.size === 1)
    val f = fl1.head
    f.b0 should be (0.067455 plusOrMinus 1e-6)
    f.b1 should be (0.134911 plusOrMinus 1e-6)
    f.b2 should be (0.067455 plusOrMinus 1e-6)
    f.a1 should be (-1.14298 plusOrMinus 1e-5)
    f.a2 should be (0.4128 plusOrMinus 1e-4)

    // cutoff frequency of 0.5 (checking using coefficients from Octave)
    val fl2 = Butter.butterSOSEven(2, 0.5)
    assert(fl2.size === 1)
    val g = fl2.head
    g.b0 should be (0.29289 plusOrMinus 1e-5)
    g.b1 should be (0.58579 plusOrMinus 1e-5)
    g.b2 should be (0.29289 plusOrMinus 1e-5)
    g.a1 should be (0.0 plusOrMinus 1e-10)
    g.a2 should be (0.17157 plusOrMinus 1e-5)
  }

  test("construct a fourth-order filter with butterSOSEven") (pending)

}
