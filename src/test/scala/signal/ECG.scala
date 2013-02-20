package signal

import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers
import breeze.math.Complex

/** Some ECG phantom test signals. */ 
object ECG {

  /** Parses `Complex` numbers. */
  private object complexParser extends JavaTokenParsers {
    def apply(s: String): Complex = parse(expr, s).getOrElse {
      throw new NumberFormatException(
        "Could not parse \"%s\" as a Complex number."
      )
    }
    def f = floatingPointNumber
    def fpi = f <~ "i" ^^ ((x: String) => Complex(0, x.toDouble))
    def fp = f ^^ ((x: String) => Complex(x.toDouble, 0))
    def expr = (fpi | fp) * (
      "+" ^^^ ((x: Complex, y: Complex) => x + y) |
      "-" ^^^ ((x: Complex, y: Complex) => x - y)
    )
  }
  
  /** Loads a signal from a source file.
   * 
   *  Signal source files are text files containing a 1D signal.  Each line of
   *  the file may contain a comment, which starts with a `#` symbol, or a
   *  double-precision number, specified in text form.
   *  
   *  For example, a valid file may look like this:
   *  {{{
   *  # This first line is a comment
   *  1.00
   *  2.00
   *  3.00
   *  # For some reason, another comment
   *  42.556
   *  }}}
   *  
   *  @param source source from which to load the signal
   *  @return the signal */
  private def loadSignal(source: Source): List[Double] = {
    val x = source.getLines.filterNot(_.trim.startsWith("#")).
    	map(_.trim.toDouble).toList
    source.close
    x
  }
  
  /** Loads a complex signal from a source file.
   *  
   *  A valid file may look like this:
   *  {{{
   *  # A comment
   *  # A real
   *  42.00
   *  # A complex
   *  42.00+52.00i
   *  }}}
   *  
   *  @param source source from which to load the signal
   *  @return the signal */
  private def loadComplexSignal(source: Source): List[Complex] = {
    val nonComments = source.getLines.filterNot(_.trim.startsWith("#"))
    nonComments.map(complexParser(_)).toList
  }

  /** Loads a signal from the `ecg` directory.
   *  
   *  This method loads a signal (specified as indicated in the
   *  comments for the `loadSignal` method), from the
   *  `src/test/resources/signal/ecg` directory of the project.
   *  
   *  @param name name of the file to load
   *  @return the signal */
  private def ecgSignal(name: String): List[Double] = {
    val rs = getClass.getResourceAsStream("ecg/%s" format name)
    val x = loadSignal(Source.fromInputStream(rs))
    rs.close
    x
  }

  /** Loads a `Complex` signal from the `ecg` directory.
   *  
   *  This method loads a signal (specified as indicated in the comments for
   *  the `loadComplexSignal` method), from the
   *  `src/test/resources/signal/ecg` directory of the project.
   *  
   *  @param name name of the file to load
   *  @return the `Complex` signal */
  private def ecgComplexSignal(name: String): List[Complex] = {
    val rs = getClass.getResourceAsStream("ecg/%s" format name)
    val x = loadComplexSignal(Source.fromInputStream(rs))
    rs.close
    x
  }
  
  /** ECG test phantom with noise, sampled at 500 Hz. */
  lazy val noisyecg: List[Double] = ecgSignal("noisyecg.dat")
  /** Filtered with a 2nd order Butterworth low-pass filter, cutoff at 
   *  10 Hz. */
  lazy val butter2filter: List[Double] = 
    ecgSignal("ecg-butter2-0.04-filter.dat")
  /** FiltFilt-ed with a 2nd order Butterworth low-pass filter, cutoff at 
   *  10 Hz. */
  lazy val butter2filtfilt: List[Double] = 
	ecgSignal("ecg-butter2-0.04-filtfilt.dat")
  /** SOS filtered with a 4th order Butterworth low-pass filter, cutoff at 
   *  20 Hz.  (Gains not applied!) */
  lazy val butter4sosfilt: List[Double] = 
	ecgSignal("ecg-butter4-0.08-sosfilt.dat")
  /** SOS filtfilt-ed with a 4th order Butterworth low-pass filter, cutoff at 
   *  20 Hz. */
  lazy val butter4sosfiltfilt: List[Double] = 
    ecgSignal("ecg-butter4-0.08-sosfiltfilt.dat")
  /** FFT of the ECG test phantom with noise. */
  lazy val fft: List[Complex] = ecgComplexSignal("fft.dat")
  
}
