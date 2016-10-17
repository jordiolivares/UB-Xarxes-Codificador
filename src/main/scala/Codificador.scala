import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

/**
  * Created by jordi on 16/10/16.
  */
object Codificador {
  def main(args: Array[String]): Unit = {
    import scala.io.StdIn
    println("""Entra la seqüencia de bits a codificar:""")
    val bits = StdIn.readLine()
    println(
      """Selecciona el Codificador de la següent llista
        |    1. Manchester
        |    2. Manchester Differential
        |    3. NRZ
        |    4. NRZ-L
        |    5. NRZ-I
        |    6. Bipolar-Ami
        |    7. Pseudoternary
      """.stripMargin)
    var coder: Encoder = null
    StdIn.readInt() match {
      case 1 => coder = new Manchester
      case 2 => coder = new ManchesterDifferential(bits.charAt(0))
      case 3 => coder = new NonReturnToZero
      case 4 => coder = new NonReturnToZeroLevel
      case 5 => coder = new NonReturnToZeroInverted(bits.charAt(0))
      case 6 => coder = new Bipolar
      case 7 => coder = new Pseudoternary
    }
    val out =
      """set yrange [-2:2]
        |plot "-" with lines
        |
        |""".stripMargin ++
      coder.encode(bits) ++
      """
        |end
        |""".stripMargin
    print(out)
    import scala.sys.process._
    "gnuplot -p" #< new ByteArrayInputStream(out.getBytes(StandardCharsets.UTF_8)) !
  }
}
