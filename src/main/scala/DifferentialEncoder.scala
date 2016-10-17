/**
  * Created by jordi on 16/10/16.
  */
abstract class DifferentialEncoder extends Encoder {
  def equal(bit1: Char, bit2: Char, pos: Int): String
  def switch(bit1: Char, bit2: Char, pos: Int): String

  def encode(bits: String): String = {
    Utils.chains(bits.toList).zipWithIndex.map {
      case ((x, y), index) => if (y == '1') switch(x, y, index) else equal(x, y, index)
    }.mkString("\n")
  }
}

class NonReturnToZeroInverted(firstBit: Char) extends DifferentialEncoder {
  private var previousSignal: Signal = _
  private var switchSignal: Signal = _

  if (firstBit == '0') {
    previousSignal = NonReturnToZero.zero
    switchSignal = NonReturnToZero.one
  } else {
    previousSignal = NonReturnToZero.one
    switchSignal = NonReturnToZero.zero
  }

  override def equal(bit1: Char, bit2: Char, pos: Int): String = {
    previousSignal.at(pos)
  }

  override def switch(bit1: Char, bit2: Char, pos: Int): String = {
    val tmp = previousSignal
    this.previousSignal = switchSignal
    this.switchSignal = tmp
    previousSignal.at(pos)
  }
}

class ManchesterDifferential(firstBit: Char) extends DifferentialEncoder {
  private var currentSignal: Signal = _
  private var nextSignal: Signal = _

  if (firstBit == '0') {
    currentSignal = Manchester.zero
    nextSignal = Manchester.one
  } else {
    currentSignal = Manchester.one
    nextSignal = Manchester.zero
  }

  override def equal(bit1: Char, bit2: Char, pos: Int): String = {
    val result = nextSignal.at(pos)
    val tmp = currentSignal
    currentSignal = nextSignal
    nextSignal = tmp
    result
  }

  override def switch(bit1: Char, bit2: Char, pos: Int): String = {
    currentSignal.at(pos)
  }
}
