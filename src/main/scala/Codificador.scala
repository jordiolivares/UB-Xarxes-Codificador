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
      """Selecciona el Codificador/Modulador de la següent llista
        |Codificadors:
        |    1. Manchester
        |    2. Manchester Differential
        |    3. NRZ
        |    4. NRZ-L
        |    5. NRZ-I
        |    6. Bipolar-Ami
        |    7. Pseudoternary
        |Moduladors:
        |    8. ASK
        |    9. FSK
        |   10. PSK
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
      case 8 => coder = new ASK(1, 1)
      case 9 => coder = new FSK(1, 1, 2)
      case 10 => coder = new PSK(1, 1)
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
