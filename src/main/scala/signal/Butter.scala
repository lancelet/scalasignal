package signal

object Butter {

  /** Constructs a second-order-section Butterworth filter.
    *
    * For this function, the Butterworth filter must be an even order.
    *
    * The algorithm is described here:
    *   http://www.kwon3d.com/theory/filtering/sys.html
    * 
    * @param order order of the filter; must be an even number
    * @param normCutoff normalized cutoff frequency (a number between 0.0
    *   and 1.0, where 1.0 is the Nyquist frequency)
    * @return a `List` of second order section filters that implement the
    *   Butterworth filter */
  def butterSOSEven(order: Int, normCutoff: Double): List[SOSFilt[Double]] = {
    require(order % 2 == 0)
    require(normCutoff >= 0.0 && normCutoff <= 1.0)

    val omegac = math.tan(math.Pi * normCutoff / 2.0)
    val b0 = omegac * omegac
    val b1 = 2.0 * b0
    
    val idxSeq = for (k <- 0 until (order / 2)) yield {
      val cf = 2.0 * math.cos(math.Pi * (2 * k + 1) / (2 * order)) * omegac
      val a0 = 1.0 + cf + b0
      val a1 = 2.0 * (b0 - 1.0)
      val a2 = 1.0 - cf + b0
      SOSFilt(b0, b1, b0, a0, a1, a2)
    }
    idxSeq.toList
  }

}
