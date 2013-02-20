package signal

import org.scalatest.matchers.ShouldMatchers._
import breeze.math.Complex

/** Performs comparisons for testing purposes. */
object Comparisons {

  /** Default test epsilon value. */
  val Eps = 1.0e-10

  /** Tests for approximate equality between sequences of `Double`s. */
  def eqd(x: Seq[Double], y: Seq[Double], tol: Double = Eps) {
    x.length should equal (y.length)
    for ((x,y) <- x zip y) {
      x should be (y plusOrMinus tol)
    }
  }
  
  /** Tests for approximate equality between sequences of `Complex`s. */
  def eqc(x: Seq[Complex], y: Seq[Complex], tol: Double = Eps) {
    x.length should equal (y.length)
    (x zip y).foreach {
      case (x, y) => {
        x.real should be (y.real plusOrMinus tol)
        x.imag should be (y.imag plusOrMinus tol)
      }
    }
  }
  
}