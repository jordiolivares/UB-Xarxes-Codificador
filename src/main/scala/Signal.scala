/**
  * Created by jordi on 16/10/16.
  */
import scala.language.implicitConversions

class Signal(val pattern: Seq[Int]) {
  private val numPoints = pattern.length
  private val step = 1.0 / (numPoints - 1)

  def at(s: Int) = {
    Utils.chains(pattern.toList).zipWithIndex.map {
      case ((point1, point2), index) =>
        if (index == numPoints - 1)
          // To avoid the collision between two bits we put a small separation towards the end
          s"""${s + 0.999} $point2"""
        else if (point1 != point2)
          // Creates the edge point between the two transitions
          s"""${s + step * (3.0 / 2) * (index - 1) - 0.001} $point1
             |${s + step * (3.0 / 2) * (index - 1) + 0.001} $point2""".stripMargin
        else
          s"${s + step * index} $point2"
    }.mkString("\n")
  }

  def unary_- = new Signal(pattern.map(-_))
}

object Signal {
  // I don't want to convert stuff, so we create a signal converter
  implicit def seq2Signal(seq: Seq[Int]): Signal = new Signal(seq)

  implicit def constant2signal(int: Int): Signal = {
    val signal = Seq(int, int)
    new Signal(signal)
  }
}
