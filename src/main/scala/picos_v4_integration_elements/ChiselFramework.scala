package picos_v4_integration_elements

import java.io._
import chisel3._
import scala.sys.process._

object CompilePicosV4InstructionParser  {
  def main(args: Array[String]): Unit = {
    val pw = new PrintWriter(new File("output.firrtl"))
    pw.write(stage.ChiselStage.emitFirrtl(new picos_v4_integration_elements.InstructionParser))
    pw.close()
    "./generate_verilog_without_randomization.sh".!!
  }
}
