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
      c.io.resp_valid.expect(false.B)
    }
  }
  
  it should "produce no resp output if an unknown funct value is passed to it" in {
    //test(new InstructionParser).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
    test(new InstructionParser) { c =>
      var a = 0;
      for(a <- 0 until 128){
        if (a < 8 || a > 13) {
            c.io.cmd_valid.poke(true.B)
            c.io.funct.poke(a.U)
            c.clock.step()
            c.io.resp_valid.expect(false.B)
            c.clock.step()
        }
      }
    }
  }

  it should "produce a resp output in the first cycle after a known funct value is passed to it" in {
    //test(new InstructionParser).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
    test(new InstructionParser) { c =>
      var a = 0;
      for(a <- 8 until 14){
        //println("Funct value being tested " + a)
        //println("Last output value :" + c.io.out.peek().litValue)
        c.io.cmd_valid.poke(true.B)
        c.io.funct.poke(a.U)
        c.clock.step()
        c.io.resp_valid.expect(true.B)
        c.clock.step()
      }
    }
  }
}
