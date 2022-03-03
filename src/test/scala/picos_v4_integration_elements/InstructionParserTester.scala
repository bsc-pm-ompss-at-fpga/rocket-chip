package picos_v4_integration_elements
//package freechips.rocketchip.picos_v4_integration_elements

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import Array._
import scala.collection.SortedSet

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
	val flag_success_hw									= 0.U
	val flag_failure_hw									= "h_ffff_ffff_ffff_ffff".U

  val funct_initiate_task             = 8 
  val funct_add_args_and_parent_info  = 9 
  val funct_send_in_dep               = 10
  val funct_send_in_deps              = 11
  val funct_send_out_dep              = 12
  val funct_send_out_deps             = 13

  val funct_fetch_nanos_id					  = 2
  val funct_retire									  = 3
  val funct_fetch_picos_id					  = 4
  val funct_work_request						  = 6

  val valid_funct_values = Array(
		funct_initiate_task,
		funct_add_args_and_parent_info,
		funct_send_in_dep,
		funct_send_in_deps,
		funct_send_out_dep,
		funct_send_out_deps,
		funct_fetch_nanos_id,
		funct_retire,
		funct_fetch_picos_id,
		funct_work_request
	).to[SortedSet]

  val funct_values_of_blocking_instructions = Array(
		funct_retire
	).to[SortedSet]

  val opcode_range = range(0, 128).to[SortedSet]
  val register_set = range(0, 32).to[SortedSet]

	val r = new scala.util.Random()
	r.setSeed(1000)
  
	behavior of "InstructionParser"
  it should "produce no resp output if an unknown funct value is passed to it" in {
    //test(new InstructionParser).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
    test(new InstructionParser) { c =>
      var a = 0;
      for(a <- (opcode_range -- valid_funct_values)){
          c.io.cmd.valid.poke(true.B)
          c.io.cmd.bits.inst.funct.poke(a.U)
          c.clock.step()
          c.io.resp.valid.expect(false.B)
          c.clock.step()
      }
    }
  }

	behavior of "All InstructionParser instructions"
  it should "produce no resp output before having been parsed" in {
    test(new InstructionParser) { c =>
      c.io.resp.valid.expect(false.B)
    }
  }

	behavior of "Non-blocking InstructionParser instructions"
  it should "write some value to their rd's in the first cycle after having been parsed" in {
    test(new InstructionParser) { c =>
      var a = 0;
      var b = 0;
      for(a <- (valid_funct_values -- funct_values_of_blocking_instructions)){
        for (b <- register_set) {
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

	behavior of "Instruction initiate_task"
  it should "route nanos_id and num_deps data to the taskInfo interface in the first cycle after having been parsed" in {
		val testValue0 = 2394870923L
		val testValue1 = 12
    test(new InstructionParser) { c =>
			c.io.cmd.valid.poke(true.B)
			c.io.cmd.bits.inst.funct.poke(funct_initiate_task.U)
			c.io.cmd.bits.rs1.poke(testValue0.U)
			c.io.cmd.bits.rs2.poke(testValue1.U)
			c.clock.step()
			c.io.resp.valid.expect(true.B)
			c.io.taskInfo.valid.expect(true.B)
			c.io.taskInfo.bits.task_id.expect(testValue0.U)
			c.io.taskInfo.bits.num_deps.expect(testValue1.U)
			c.clock.step()
    }
  }

	behavior of "Instruction add_args_and_parent_info"
  it should "route parent_id and num_args data to the argsAndParentInfo interface in the first cycle after having been parsed" in {
		val testValue0 = 7221378036354780807L
		val testValue1 = 10
    test(new InstructionParser) { c =>
			c.io.cmd.valid.poke(true.B)
			c.io.cmd.bits.inst.funct.poke(funct_add_args_and_parent_info.U)
			c.io.cmd.bits.rs1.poke(testValue0.U)
			c.io.cmd.bits.rs2.poke(testValue1.U)
			c.clock.step()
			c.io.resp.valid.expect(true.B)
			c.io.argsAndParentInfo.valid.expect(true.B)
			c.io.argsAndParentInfo.bits.parent_id.expect(testValue0.U)
			c.io.argsAndParentInfo.bits.num_args.expect(testValue1.U)
			c.clock.step()
    }
  }

	val invalid_dep_code = 0
	val in_dep_code = 1
	val out_dep_code = 2

	behavior of "Instruction send_in_dep"
  it should "route pointer data to depInfo, describe the first dep as of IN kind, and the second as of INVALID kind" in {
		val testValue0 = 72057594037927935L // The maximum 56-bit number
		val testValue1 = 14014398509481983L
    test(new InstructionParser) { c =>
			c.io.cmd.valid.poke(true.B)
			c.io.cmd.bits.inst.funct.poke(funct_send_in_dep.U)
			c.io.cmd.bits.rs1.poke(testValue0.U)
			c.io.cmd.bits.rs2.poke(testValue1.U)
			c.clock.step()
			c.io.resp.valid.expect(true.B)
			c.io.depInfo.valid.expect(true.B)
			c.io.depInfo.bits(0).pointer.expect(testValue0.U)
			c.io.depInfo.bits(0).kind.expect(in_dep_code.U)
			c.io.depInfo.bits(1).kind.expect(invalid_dep_code.U)
			c.clock.step()
    }
  }

	behavior of "Instruction send_in_deps"
  it should "route pointer data to depInfo and describe both deps as of IN kind" in {
		val testValue0 = 72057594037927935L // The maximum 56-bit number
		val testValue1 = 14014398509481983L
    test(new InstructionParser) { c =>
			c.io.cmd.valid.poke(true.B)
			c.io.cmd.bits.inst.funct.poke(funct_send_in_deps.U)
			c.io.cmd.bits.rs1.poke(testValue0.U)
			c.io.cmd.bits.rs2.poke(testValue1.U)
			c.clock.step()
			c.io.resp.valid.expect(true.B)
			c.io.depInfo.valid.expect(true.B)
			c.io.depInfo.bits(0).pointer.expect(testValue0.U)
			c.io.depInfo.bits(0).kind.expect(in_dep_code.U)
			c.io.depInfo.bits(1).pointer.expect(testValue1.U)
			c.io.depInfo.bits(1).kind.expect(in_dep_code.U)
			c.clock.step()
    }
  }

	behavior of "Instruction send_out_dep"
  it should "route pointer data to depInfo, describe the first dep as of OUT kind, and the second as of INVALID kind" in {
		val testValue0 = 72057594037927935L // The maximum 56-bit number
		val testValue1 = 14014398509481983L
    test(new InstructionParser) { c =>
			c.io.cmd.valid.poke(true.B)
			c.io.cmd.bits.inst.funct.poke(funct_send_out_dep.U)
			c.io.cmd.bits.rs1.poke(testValue0.U)
			c.io.cmd.bits.rs2.poke(testValue1.U)
			c.clock.step()
			c.io.resp.valid.expect(true.B)
			c.io.depInfo.valid.expect(true.B)
			c.io.depInfo.bits(0).pointer.expect(testValue0.U)
			c.io.depInfo.bits(0).kind.expect(out_dep_code.U)
			c.io.depInfo.bits(1).kind.expect(invalid_dep_code.U)
			c.clock.step()
    }
  }

	behavior of "Instruction send_out_deps"
  it should "route pointer data to depInfo and describe both deps as of OUT kind" in {
		val testValue0 = 72057594037927935L // The maximum 56-bit number
		val testValue1 = 14014398509481983L
    test(new InstructionParser) { c =>
			c.io.cmd.valid.poke(true.B)
			c.io.cmd.bits.inst.funct.poke(funct_send_out_deps.U)
			c.io.cmd.bits.rs1.poke(testValue0.U)
			c.io.cmd.bits.rs2.poke(testValue1.U)
			c.clock.step()
			c.io.resp.valid.expect(true.B)
			c.io.depInfo.valid.expect(true.B)
			c.io.depInfo.bits(0).pointer.expect(testValue0.U)
			c.io.depInfo.bits(0).kind.expect(out_dep_code.U)
			c.io.depInfo.bits(1).pointer.expect(testValue1.U)
			c.io.depInfo.bits(1).kind.expect(out_dep_code.U)
			c.clock.step()
    }
  }

	behavior of "Instruction retire_task"
  it should "not return until data encoded in rs1 could be sent to the retInterface" in {
		val testValue0 = 9223372036854775807L
		val testRd = 15
    test(new InstructionParser) { c =>
			c.io.cmd.valid.poke(true.B)
			c.io.cmd.bits.inst.rd.poke(testRd.U)
			c.io.cmd.bits.inst.funct.poke(funct_retire.U)
			c.io.cmd.bits.rs1.poke(testValue0.U)
			c.io.retInterface.ready.poke(false.B)

			for (i <- range(0, 1000)) {
				c.io.resp.valid.expect(false.B)
				c.clock.step()
			}

			c.io.retInterface.ready.poke(true.B)
			c.io.resp.valid.expect(true.B)
			c.io.resp.bits.rd.expect(testRd.U)
			c.io.retInterface.bits.expect(testValue0.U)
			c.io.resp.bits.data.expect(flag_success_hw)
			c.clock.step()
    }
  }

	behavior of "Instruction work_request"
  it should "return an increasing value every time a work-fetch request could be issued, and the same value as before when it couldn't" in {
		val testRd = 15
    var requestWillSucceed = false
    var numSuccessfulRequests = 0
    test(new InstructionParser) { c =>
			for (i <- range(0, 100)) {
        requestWillSucceed = r.nextBoolean()
        if (requestWillSucceed) {
          numSuccessfulRequests += 1
        }

				c.io.cmd.valid.poke(true.B)
				c.io.cmd.bits.inst.rd.poke(testRd.U)
				c.io.cmd.bits.inst.funct.poke(funct_work_request.U)
				c.clock.step()
				c.io.readyRequestInterface.ready.poke(requestWillSucceed.B)
				c.io.resp.valid.expect(true.B)
				c.io.resp.bits.rd.expect(testRd.U)
				c.io.resp.bits.data.expect(numSuccessfulRequests.U)
				c.clock.step()
			}
    }
  }

	behavior of "Instruction nanos_id_work_fetching"
  it should "never cause readQInterface to fire before picos_id_work_fetching is executed" in {
		val testRd = 15
		val testValue0 = 2394870923L
    var requestWillSucceed = false
    var numSuccessfulRequests = 0
    test(new InstructionParser) { c =>
      c.io.cmd.valid.poke(true.B)
      c.io.cmd.bits.inst.rd.poke(testRd.U)
      c.io.cmd.bits.inst.funct.poke(funct_fetch_nanos_id.U)
      c.clock.step()
      c.io.readyQInterface.valid.poke(true.B)
      c.io.readyQInterface.bits.nanosID.poke(testValue0.U)
      c.io.readyQInterface.ready.expect(false.B)
      c.clock.step()
      // Now in the cycle spent with initial instruction parsing
      c.clock.step()
      c.io.readyQInterface.valid.poke(true.B)
      c.io.readyQInterface.bits.nanosID.poke(testValue0.U)
      c.io.readyQInterface.ready.expect(false.B)
      c.clock.step()
    }
  }

  it should "only return valid data once after any given readyQInterface firing" in {
		val testRd = 15
		val testValue0 = 2394870923L
    var requestWillSucceed = false
    var numSuccessfulRequests = 0
    test(new InstructionParser) { c =>
      c.io.cmd.valid.poke(true.B)
      c.io.cmd.bits.inst.rd.poke(testRd.U)
      c.io.cmd.bits.inst.funct.poke(funct_fetch_nanos_id.U)
      c.clock.step()
      c.io.readyQInterface.valid.poke(true.B)
      c.io.readyQInterface.bits.nanosID.poke(testValue0.U)
      c.io.resp.valid.expect(true.B)
      c.io.resp.bits.rd.expect(testRd.U)
      c.io.resp.bits.data.expect(testValue0.U)
      c.clock.step()
      // Now in the cycle spent with initial instruction parsing
      c.clock.step()
      c.io.resp.valid.expect(true.B)
      c.io.resp.bits.rd.expect(testRd.U)
      c.io.resp.bits.data.expect(flag_failure_hw)
      c.clock.step()
    }
  }

	behavior of "Instruction picos_id_work_fetching"
  it should "only return valid data if nanos_id_work_fetching has previously returned valid data itself, even if there is valid data at readyQInterface" in {
		val testRd0 = 4
		val testRd1 = 11
		val testValuePicosID = 989870923L
		val testValueNanosID = 2394870923L
    var requestWillSucceed = false
    var numSuccessfulRequests = 0
    test(new InstructionParser) { c =>
      c.io.readyQInterface.valid.poke(true.B)
      c.io.readyQInterface.bits.nanosID.poke(testValueNanosID.U)
      c.io.readyQInterface.bits.picosID.poke(testValuePicosID.U)

      c.io.cmd.valid.poke(true.B)
      c.io.cmd.bits.inst.rd.poke(testRd0.U)
      c.io.cmd.bits.inst.funct.poke(funct_fetch_picos_id.U)

      c.clock.step()

      c.io.resp.valid.expect(true.B)
      c.io.resp.bits.rd.expect(testRd0.U)
      c.io.resp.bits.data.expect(flag_failure_hw)

      c.clock.step()

      c.io.cmd.bits.inst.rd.poke(testRd1.U)
      c.io.cmd.bits.inst.funct.poke(funct_fetch_nanos_id.U)

      c.clock.step()

      c.io.resp.valid.expect(true.B)
      c.io.resp.bits.rd.expect(testRd1.U)
      c.io.resp.bits.data.expect(testValueNanosID.U)
      
      c.clock.step()

      c.io.cmd.bits.inst.rd.poke(testRd0.U)
      c.io.cmd.bits.inst.funct.poke(funct_fetch_picos_id.U)

      c.clock.step()

      c.io.resp.valid.expect(true.B)
      c.io.resp.bits.rd.expect(testRd0.U)
      c.io.resp.bits.data.expect(testValuePicosID.U)
    }
  }
}
