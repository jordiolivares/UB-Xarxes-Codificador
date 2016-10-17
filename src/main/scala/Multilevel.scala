class Bipolar extends Encoder {
  private var nextSignal: Signal = 1
  private val noSignal: Signal = 0

  override def encode(bits: String): String = {
    var result = ""
    bits.zipWithIndex.foreach {
      case ('1', index) => {
        result += nextSignal at index
        result += '\n'
        nextSignal = -nextSignal
      }
      case ('0', index) => {
        result += noSignal at index
        result += '\n'
      }
    }
    result
  }
}

class Pseudoternary extends Encoder {
  private var nextSignal: Signal = 1
  private val noSignal: Signal = 0

  override def encode(bits: String): String = {
    var result = ""
    bits.zipWithIndex.foreach {
      case ('0', index) => {
        result += nextSignal at index
        result += '\n'
        nextSignal = -nextSignal
      }
      case ('1', index) => {
        result += noSignal at index
        result += '\n'
      }
    }
    result
  }
}

