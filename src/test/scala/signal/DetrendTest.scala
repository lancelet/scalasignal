package signal

import scala.collection.immutable._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class DetrendTest extends FunSuite with ShouldMatchers {

  test("detrend") {
    // signal with no linear trend
    val sig1 = Vector[Double](0, 1, -2, 1, 0)
    // add a linear trend
    val sigTrend1 = sig1.zipWithIndex.map(i => i._1 + i._2)
    // now de-trend the signal
    Detrend.detrend(sigTrend1) should be (sig1)
    
    // signal that is pure linear trend
    val sig2 = Vector[Double](0, -1, -2, -3)
    Detrend.detrend(sig2) should be (Vector[Double](0, 0, 0, 0))
  }
  
}