/**
  * Created by jordi on 16/10/16.
  */
class Signal(signal: String) {
  private val lines = signal.split("\n")
  private val numLines = lines.length
  private val step = 1.0 / (numLines - 1)

  def at(s: Int) = {
    Utils.chains(lines.toList).zipWithIndex.map {
      case ((line1, line2), index) =>
        if (index == numLines - 1)
          // To avoid the collision between two bits we put a small separation towards the end
          s"""${s + 0.999} $line2"""
        else if (line1 != line2)
          // Creates the edge point between the two transitions
          s"""${s + step * (3.0 / 2) * (index - 1) - 0.001} $line1
             |${s + step * (3.0 / 2) * (index - 1) + 0.001} $line2""".stripMargin
        else
          s"${s + step * index} $line2"
    }.mkString("\n")
  }
}

object Signal {
  // I don't want to convert stuff, so we create a signal converter
  implicit def string2signal(str: String): Signal = new Signal(str)

  implicit def constant2signal(int: Int): Signal = {
    val signal = s"""$int
                    |$int""".stripMargin
    new Signal(signal)
  }
}
