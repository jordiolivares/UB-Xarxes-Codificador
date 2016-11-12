import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import scala.util.Random

/**
  * Created by jordi on 16/10/16.
  */
object Codificador {
  def main(args: Array[String]): Unit = {
    import scala.io.StdIn
    println("""Entra la seqüencia de bits a codificar: (Prem ENTER si vols que sigui aleatoria)""")
    val bits = {
      val tmp = StdIn.readLine()
      if (tmp.length == 0)
        Seq.fill(20)(Random.nextInt(2).toString).mkString
      else
        tmp
    }
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
        |    8. B8ZS
        |    9. HDB3
        |Moduladors:
        |   10. ASK
        |   11. FSK
        |   12. PSK
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
      case 8 => coder = new B8ZS
      case 9 => coder = new HDB3
      case 10 => coder = new ASK(1, 1)
      case 11 => coder = new FSK(1, 2, 1)
      case 12 => coder = new PSK(1, 1)
    }
    val bitsAxis = bits.zipWithIndex.map {
      case (bit, index) => s"'$bit' ${index + 0.5}"
    }.mkString(", ")
    val vertLines = (1 to bits.length).map(i => s"set arrow from $i, graph 0 to $i, graph 1 nohead lc rgb 'grey'").mkString("\n")
    val out =
      s"""set yrange [-2:2]
        |set xtics ($bitsAxis)
        |$vertLines
        |plot "-" with lines
        |
        |""".stripMargin ++
      coder.encode(bits) ++
      """
        |end
        |""".stripMargin
    import scala.sys.process._
    "gnuplot -p" #< new ByteArrayInputStream(out.getBytes(StandardCharsets.UTF_8)) !
  }
}
