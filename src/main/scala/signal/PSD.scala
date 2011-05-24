package signal

import collection.immutable._

object PSD {

  /** Estimates the power spectral density of a signal.
   *  
   *  === Algorithm ===
   *  The PSD is estimated using a Fast Fourier Transform.  The sequence of
   *  steps is as follows:
   *  
   *   1. The signal in `x` is padded to the next closest power of 2,
   *      denoted by `nfft`.
   *      
   *   2. The padded signal is transformed using the FFT.
   *   
   *   3. The first half of the frequency domain is kept (the second half is
   *      a mirror image because the signal is real).
   *      
   *   4. The `Complex` absolute value of the frequency domain is computed.
   *   
   *   5. The frequency magnitudes are divided by the length of the
   *      original signal (`x.length`).
   *   
   *   6. The frequency magnitudes are squared.
   *   
   *   7. The frequency magnitudes are multiplied by 2, except the
   *      first (DC) and last (Nyquist).  Since only half of the original
   *      signal was kept, this recovers the total energy.  The DC and
   *      Nyquist frequencies are not multiplied by 2 because they are
   *      unique.
   * 
   * @param x the signal
   * @param fs the sampling frequency (used to calculate the frequency
   *   components)
   * @return a sequence of pairs, in which the first component of the pair
   *   is the frequency band, and the second component is the power contained
   *   in that frequency band */
  def psd(x: Seq[Double], fs: Double = 1.0): Seq[(Double, Double)] = {   
    // pad x to the next closest power of 2
    val nfft = FFT.nextPow2(x.length)
    val xx = x.padTo(nfft, 0.0)

    // 1. take FFT of the padded signal
    // 2. keep only the first half (the second is symmetric)
    // 3. take the absolute value
    // 4. divide by the length of the original signal
    // 5. square
    val h1 = FFT.fft(xx).take(nfft / 2).map(_.abs).map(_ / xx.length).
      map(x => x * x)
    
    // multiply all frequencies of the signal by 2, except the DC and
    //  Nyquist frequencies (which are unique).  we kept only the first half 
    //  of the signal, so this recovers the energy of the other half
    val midFreq = h1.tail.dropRight(1).map(_ * 2.0)
    val h = Vector(h1.head) ++ midFreq ++ Vector(h1.last)

    // generate frequency vector
    val freq = for {
      i <- 0 until nfft/2
      f = i.toDouble * fs / nfft
    } yield f

    // return frequencies zipped with magnitudes
    freq zip h
  }
  
  /** Signal bandwidth estimation.
   *  
   *  Estimates the frequency below which `fract` amount of the power of the 
   *  signal is contained.
   *  
   *  === Algorithm ===
   *  The PSD (power spectral density) of the signal is contained in `pd`.
   *  This is a list of frequency and power pairs.  The total power is first
   *  computed by summing the power components.  Then, `pd` is iterated until
   *  `fract` amount of the total power has been found.
   *  
   *  @param pd power spectral density (result of the `psd` method)
   *  @param fract fraction of the signal bandwidth to acquire
   *  @return frequency below which `fract` amount of the signal power is
   *    contained */
  def bandwidth(pd: Seq[(Double, Double)], fract: Double = 0.95): Double = {
    require(fract > 0.0 && fract < 1.0)
    
    // find the target power
    val pTarget = pd.map(_._2).sum * fract

    // accumulate to the target power
    var accum: Double = 0.0
    for (d <- pd) {
      accum = accum + d._2
      if (accum >= pTarget) {
        return d._1
      }
    }

    // if we reach here, all frequencies are required, so send the last
    return pd.last._1
  }
    
}
