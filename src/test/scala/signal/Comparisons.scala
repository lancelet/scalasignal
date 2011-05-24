package signal

import org.scalatest.matchers.ShouldMatchers._

/** Performs comparisons for testing purposes. */
object Comparisons {

  /** Default test epsilon value. */
  val Eps = 1.0e-10

  /** Tests for approximate equality between sequences of `Double`s. */
  def eq(x: Seq[Double], y: Seq[Double], tol: Double = Eps) {
    x.length should equal (y.length)
    (x zip y).foreach { case (x, y) => x should be (y plusOrMinus tol) }
  }
  
}