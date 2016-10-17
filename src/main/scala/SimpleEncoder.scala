/**
  * Created by jordi on 17/10/16.
  */
abstract class SimpleEncoder(val zero: Signal, val one: Signal) extends Encoder {
  def encode(bits: String): String = {
    bits.zipWithIndex.map {
      case ('1', x) => one.at(x)
      case ('0', x) => zero.at(x)
    }.mkString("\n")
  }
}

object NonReturnToZero {
  val one =
    """1
      |1""".stripMargin
  val zero =
    """0
      |0""".stripMargin
}

class NonReturnToZero extends SimpleEncoder(NonReturnToZero.zero, NonReturnToZero.one)
class NonReturnToZeroLevel extends SimpleEncoder(NonReturnToZero.one, NonReturnToZero.zero)

object Manchester {
  // We define the signals for each bit
  val one =
    """-1
      |-1
      |1
      |1""".stripMargin
  val zero =
    """1
      |1
      |-1
      |-1""".stripMargin
}

class Manchester extends SimpleEncoder(Manchester.zero, Manchester.one)
