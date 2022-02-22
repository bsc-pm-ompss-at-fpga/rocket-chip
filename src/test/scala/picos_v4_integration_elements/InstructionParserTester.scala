package picos_v4_integration_elements
//package freechips.rocketchip.picos_v4_integration_elements

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/**
  * This is a trivial example of how to run this Specification
  * From within sbt use:
  * {{{
  * testOnly picos_v4_integration_elements
  * }}}
  *
  * For a reference on AnyFlatSpec constructs, please refer to:
  * https://www.scalatest.org/scaladoc/3.2.11/org/scalatest/flatspec/AnyFlatSpec.html
  *
  * For a more basic reference on Chisel testing, refer to:
  * https://github.com/ucb-bar/chiseltest
  */
class BasicTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "InstructionParser"
  it should "produce no resp output before the first instruction has been parsed" in {
    test(new InstructionParser) { c =>
      c.io.resp.valid.expect(false.B)
    }
  }
  
  it should "produce no resp output if an unknown funct value is passed to it" in {
    //test(new InstructionParser).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
    test(new InstructionParser) { c =>
      var a = 0;
      for(a <- 0 until 128){
        if (a < 8 || a > 13) {
            c.io.cmd.valid.poke(true.B)
            c.io.cmd.bits.inst.funct.poke(a.U)
            c.clock.step()
            c.io.resp.valid.expect(false.B)
            c.clock.step()
        }
      }
    }
  }

  it should "write some value to the rd indicated by the instruction in the first cycle after a known funct value is passed to it" in {
    //test(new InstructionParser).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
    test(new InstructionParser) { c =>
      var a = 0;
      var b = 0;
      for(a <- 8 until 14){
        for (b <- 0 until 33)
          c.io.cmd.valid.poke(true.B)
          c.io.cmd.bits.inst.funct.poke(a.U)
          c.io.cmd.bits.inst.rd.poke(b.U)
          c.clock.step()
          c.io.resp.valid.expect(true.B)
          c.io.resp.bits.rd.expect(b.U)
          c.clock.step()
        }
      }
    }
}
