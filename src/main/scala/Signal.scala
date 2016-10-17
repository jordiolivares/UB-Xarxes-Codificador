/**
  * Created by jordi on 16/10/16.
  */
import scala.language.implicitConversions

class Signal(val pattern: Seq[Double]) {
  private val numPoints = pattern.length
  private val step = 1.0 / (numPoints - 1)

  def at(s: Int) = {
    Utils.chains(pattern.toList).zipWithIndex.map {
      case ((point1, point2), index) =>
        if (point1 != point2)
          // Creates the edge point between the two transitions
          s"""${s + step * (3.0 / 2) * (index - 1)} $point1
             |${s + step * (3.0 / 2) * (index - 1)} $point2""".stripMargin
        else
          s"${s + step * index} $point2"
    }.mkString("\n")
  }

  def unary_- = new Signal(pattern.map(-_))
}

object Signal {
  implicit def seqInt2signal(seq: Seq[Int]): Signal = new Signal(seq.map(_.toDouble))
  // I don't want to convert stuff, so we create a signal converter
  implicit def seq2Signal(seq: Seq[Double]): Signal = new Signal(seq)

  implicit def constant2signal(const: Double): Signal = {
    val signal = Seq(const, const)
    new Signal(signal)
  }
}

class ContinuousSignal(f: Double => Double) {
  private val points = 1000

  def at(s: Int) = {
    val step = 1.0 / (points - 1)
    (0 to points).map {
      i => s"${s + step * i} ${f(s + step * i)}"
    }.mkString("\n")
  }
}
