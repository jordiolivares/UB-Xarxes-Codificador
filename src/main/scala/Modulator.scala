import scala.math.{cos, Pi}

abstract class SimpleContinuousEncoder extends Encoder {
  def one(x: Double): Double
  def zero(x: Double): Double
  private val oneSignal: ContinuousSignal = new ContinuousSignal(one)
  private val zeroSignal: ContinuousSignal = new ContinuousSignal(zero)
  
  override def encode(bits: String): String = {
    bits.zipWithIndex.map {
      case ('1', x) => oneSignal at x
      case ('0', x) => zeroSignal at x
    }.mkString("\n")
  }
}

class ASK(amp: Double, freq: Int) extends SimpleContinuousEncoder {
  override def one(x: Double) = amp * cos(2 * Pi * freq * x)
  override def zero(x: Double) = 0
}

class FSK(amp: Double, freq1: Int, freq0: Int) extends SimpleContinuousEncoder {
  override def one(x: Double) = amp * cos(2 * Pi * freq1 * x)
  override def zero(x: Double) = amp * cos(2 * Pi * freq0 * x)
}

class PSK(amp: Double, freq: Double) extends SimpleContinuousEncoder {
  override def one(x: Double) = amp * cos(2 * Pi * freq * x)
  override def zero(x: Double) = amp * cos(2 * Pi * freq * x + Pi)
}


