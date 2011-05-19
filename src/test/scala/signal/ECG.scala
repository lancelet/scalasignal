package signal

import scala.io.Source

/** Some ECG phantom test signals. */ 
object ECG {

  private def loadSignal(source: Source): List[Double] = {
    val x = source.getLines.filterNot(_.trim.startsWith("#")).map(_.trim.toDouble).toList
    source.close
    x
  }

  private def ecgSignal(name: String): List[Double] = {
    val rs = getClass.getResourceAsStream("ecg/%s" format name)
    val x = loadSignal(Source.fromInputStream(rs))
    rs.close
    x
  }

  /** ECG test phantom with noise, sampled at 500 Hz. */
  lazy val noisyecg: List[Double] = ecgSignal("noisyecg.dat")
  /** Filtered with a 2nd order Butterworth low-pass filter, cutoff at 10 Hz. */
  lazy val butter2filter: List[Double] = ecgSignal("ecg-butter2-0.04-filter.dat")
  /** FiltFilt-ed with a 2nd order Butterworth low-pass filter, cutoff at 10 Hz. */
  lazy val butter2filtfilt: List[Double] = ecgSignal("ecg-butter2-0.04-filtfilt.dat")
  /** SOS filtered with a 4th order Butterworth low-pass filter, cutoff at 20 Hz.  (Gains not applied!) */
  lazy val butter4sosfilt: List[Double] = ecgSignal("ecg-butter4-0.08-sosfilt.dat")
  /** SOS filtfilt-ed with a 4th order Butterworth low-pass filter, cutoff at 20 Hz. */
  lazy val butter4sosfiltfilt: List[Double] = ecgSignal("ecg-butter4-0.08-sosfiltfilt.dat")
}
