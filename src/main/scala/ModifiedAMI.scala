/**
  * Created by jordi on 9/11/16.
  */
abstract class ModifiedAMI extends Encoder {
  private var nextSignal: Signal = 1
  private val noSignal: Signal = 0
  private var violationSignal: Signal = 1

  override def encode(bits: String): String = {
    val preprocessedBits = preprocess(bits)
    var result = ""
    preprocessedBits.zipWithIndex.foreach {
      case ('1', index) =>
        result += nextSignal at index
        result += '\n'
        violationSignal = nextSignal
        nextSignal = -nextSignal
      case ('0', index) =>
        result += noSignal at index
        result += '\n'
      case ('B', index) =>
        result += nextSignal at index
        result += '\n'
        violationSignal = nextSignal
        nextSignal = -nextSignal
      case ('V', index) =>
        result += violationSignal at index
        result += '\n'
    }
    result
  }

  def preprocess(bits: String): String
}

class HDB3 extends ModifiedAMI {
  override def preprocess(bits: String): String = {
    var result = ""
    var numZeroes = 0
    var numBits = 0
    def isEven = numBits % 2 == 0
    bits.foreach {
      case '0' =>
        numZeroes += 1
        if (numZeroes == 4) {
          if (isEven) {
            result = result ++ "B00V"
          } else {
            result = result ++ "000V"
          }
          numZeroes = 0
          numBits = 0
        }
      case '1' =>
        result = result ++ "0" * numZeroes ++ "1"
        numZeroes = 0
        numBits += 1
    }
    result ++ "0" * numZeroes
  }
}

class B8ZS extends ModifiedAMI {
  override def preprocess(bits: String): String = {
    var result = ""
    var numZeroes = 0
    bits.foreach {
      case '0' =>
        numZeroes += 1
        if (numZeroes == 8) {
          result = result ++ "000VB0VB"
          numZeroes = 0
        }
      case '1' =>
        result = result ++ "0" * numZeroes ++ "1"
        numZeroes = 0
    }
    result ++ "0" * numZeroes
  }
}