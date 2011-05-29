package signal

import collection.immutable._

object PSD {

  /** Estimates the power spectral density of a signal.
   *  
   *  === Algorithm ===
   *  The PSD is estimated using a Fast Fourier Transform.  The sequence of
   *  steps is as follows:
   *  
   *  1. The signal is transformed using the FFT.
   *  
   *  2. The first half of the frequency domain is kept (the second half is
   *     a mirror image under complex conjugation because the signal is real).
   *     For a given number of samples `N = x.length`, if `N` is even then 
   *     `N/2 + 1` samples are kept, while if `N` is odd, then `(N+1)/2` are 
   *     kept.
   *   
   *  3. The `Complex` absolute values of the frequency domain are computed.
   *  
   *  4. The frequency magnitudes are divided by the length of the original
   *     signal, `N`.
   *     
   *  5. The frequency magnitudes are squared.
   *  
   *  6. The magnitudes are multiplied by 2, except the first (DC) and
   *     Nyquist components (the Nyquist component is only present if
   *     `N` is even).  This accounts for the complex-conjugate pairs which
   *     were discarded in step 2.
   * 
   * @todo add windowing and spectrogram options; also compare with Matlab
   *   output
   * 
   * @param x the signal
   * @param fs the sampling frequency (used to calculate the frequency
   *   components)
   * @return a sequence of pairs, in which the first component of the pair
   *   is the frequency band, and the second component is the power contained
   *   in that frequency band */
  def psd(x: Seq[Double], fs: Double = 1.0): Seq[(Double, Double)] = {
    // determine if the signal has an even number of samples
    val isEven = (x.length % 2 == 0)
    // compute number of unique samples in the transformed FFT
    val nUnique = if (isEven) x.length / 2 + 1 else (x.length + 1) / 2
    
    // 1. take FFT
    // 2. keep only first half
    // 3. take the Complex absolute value
    // 4. divide by the length of the original signal
    // 5. square
    val h1 = FFT.fft(x).take(nUnique).map(_.abs).map(_ / x.length).
      map(x => x * x)
    
    // multiply frequencies by 2, except for DC and Nyquist
    val h = if (isEven) {
      // if there were an even number of samples, the Nyquist freq is present
      val midFreq = h1.tail.dropRight(1).map(_ * 2.0)
      Vector(h1.head) ++ midFreq ++ Vector(h1.last)
    } else {
      // for an odd number of samples, there was no Nyquist freq
      val tailFreq = h1.tail.map(_ * 2.0)
      Vector(h1.head) ++ tailFreq
    }

    // compute vector of frequencies
    val freq = for {
      i <- 0 until nUnique
      f = i.toDouble * fs / x.length.toDouble
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
