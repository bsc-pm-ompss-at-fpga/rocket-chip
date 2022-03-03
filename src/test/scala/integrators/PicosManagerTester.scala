package freechips.rocketchip.integrators

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import Array._
import scala.util.Random._

import scala.collection.mutable._

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
class PicosManagerTest extends AnyFlatSpec with ChiselScalatestTester {
  val numTestedCores = 8

	val r = new scala.util.Random()
	r.setSeed(1000)

  val toHexString = (num : BigInt) => {
    f"${num}%X"
  }

	behavior of "ReadyPacketExtractor"
  it should "send ready task data coming from Picos to the requesting cores respecting the petition order, at least when there is never more than one routing request packet in queue" in {
    test(new ReadyPacketExtractor(numTestedCores)) { c =>
      def asUnsigned(unsignedLong: Long) =
        (BigInt(unsignedLong >>> 1) << 1) + (unsignedLong & 1)

      val generateRandomInput = () => {
        var inputList : ListBuffer[BigInt] = ListBuffer()
        val sequenceLength = 4
        
        for (i <- 0 until sequenceLength) {
          val newPacket = asUnsigned(nextLong().abs)
          //println("newPacket: " + toHexString(newPacket))
          inputList += newPacket
        }
        inputList
      }

      val sendReadyPacket = (testPacket : BigInt) => {
        var tryAgain = false

        do {
          tryAgain = false
          c.io.picosReadyInterface.valid.poke(true.B)
          c.io.picosReadyInterface.data.poke(testPacket.U)
          if (c.io.picosReadyInterface.empty.peek().litToBoolean) {
          } else {
            tryAgain = true
          }
          c.clock.step()
          c.io.picosReadyInterface.valid.poke(false.B)
          c.io.picosReadyInterface.data.poke(0.U)
        } while (tryAgain)
      }

      val sendRoutingPacket = (coreID : Int) => {
        var tryAgain = false

        do {
          tryAgain = false
          c.io.readyRequestInterface(coreID).valid.poke(true.B)
          if (c.io.readyRequestInterface(coreID).ready.peek().litToBoolean) {
          } else {
            tryAgain = true
          }
          c.clock.step()
          c.io.readyRequestInterface(coreID).valid.poke(false.B)
        } while (tryAgain)
      }

      val driveInputSequence = (inputSequence : ListBuffer[BigInt]) => {
        inputSequence.foreach{ case el => 
          sendReadyPacket(el)
        }
      }

      class ReadyTaskInfo {
        var numArgs: BigInt = 0
        var picosID: BigInt = 0
        var parentID: BigInt = 0
        var nanosID: BigInt = 0

        override def toString() : String = {
          val baseList = List(numArgs, picosID, parentID, nanosID)
          val hexList = baseList.map(toHexString(_))
          val innerString = hexList.foldRight("")(_ + ", " + _).dropRight(2)
          return "(numArgs, picosID, parentID, nanosID) = (" + innerString + ")"
        }

        override def equals(other: Any): Boolean = {
          other match {
            case other: ReadyTaskInfo => picosID == other.picosID && nanosID == other.nanosID
            case _ => false
          }
        }
      }

      val interpretInput = (input: ListBuffer[BigInt]) => {
        assert(input.size == 4)
        var readyTask = new ReadyTaskInfo
        readyTask.numArgs = input(0) & 0xff00
        readyTask.picosID = input(1) & 0xffffffffL
        readyTask.parentID = input(2)
        readyTask.nanosID = input(3) & 0xffffffffL

        readyTask
      }

      val fetchAnswerForCore = (coreID : Int) => {
        c.io.readyQInterface(coreID).ready.poke(true.B)
        while (!c.io.readyQInterface(coreID).valid.peek().litToBoolean) {
          c.clock.step()
        }

        var readyTask = new ReadyTaskInfo

        readyTask.nanosID = c.io.readyQInterface(coreID).bits.nanosID.peek().litValue
        readyTask.picosID = c.io.readyQInterface(coreID).bits.picosID.peek().litValue

        c.clock.step()
        c.io.readyQInterface(coreID).ready.poke(false.B)

        readyTask
      }

      it should "work"

      val numReadyTaskFetches = 10
      for (i <- 0 until numReadyTaskFetches) {
        val randomInputSequence = generateRandomInput()
        val interpretedInput = interpretInput(randomInputSequence)
        val targetCore = i % numTestedCores

        //println(interpretedInput)
        driveInputSequence(randomInputSequence)
        
        sendRoutingPacket(targetCore)
        val answer = fetchAnswerForCore(targetCore)
        //println(answer)
        assert(interpretedInput == answer)
      }
    }
  }


	behavior of "SubmissionHeaderGenerator"
  it should "generate four 64-bit header packets following Picos standards as well as a single routingInfo packet complying with Guided Arbiter at every loop " in {
    test(new SubmissionHeaderGenerator(numTestedCores)) { c =>
      val placeTaskInfo = (numDeps: Int, taskID: Long) => {
          c.io.taskInfo.valid.poke(true.B)
          c.io.taskInfo.bits.num_deps.poke(numDeps.U)
          c.io.taskInfo.bits.task_id.poke(taskID.U)
      }

      val unplaceTaskInfo = () => {
          c.io.taskInfo.valid.poke(false.B)
          c.io.taskInfo.bits.num_deps.poke(0.U)
          c.io.taskInfo.bits.task_id.poke(0.U)
      }

      val sendTaskInfo = (numDeps: Int, taskID: Int) => {
        var tryAgain = false

        do {
          tryAgain = false
          placeTaskInfo(numDeps, taskID)
          if (c.io.taskInfo.ready.peek().litToBoolean) {
          } else {
            tryAgain = true
          }
          c.clock.step()
          unplaceTaskInfo()
        } while (tryAgain)
      }

      /*
      val activateRoutingInfoConsumption = () => {
        c.io.routingInfo.ready.poke(true.B)
      }

      val deactivateRoutingInfoConsumptionIfFired = () => {
        if (c.io.routingInfo.ready.peek().litToBoolean) {
          c.io.routingInfo.ready.poke(false.B)
        }
      }

      val activateOuterRoutingInfoConsumption = () => {
        c.io.routingInfoOuter.ready.poke(true.B)
      }

      val deactivateOuterRoutingInfoConsumptionIfFired = () => {
        if (c.io.routingInfoOuter.ready.peek().litToBoolean) {
          c.io.routingInfoOuter.ready.poke(false.B)
        }
      }
      */

      val checkPacket0 = (numDeps: Int, taskNumber: Long) => {
        c.io.picosSubData.valid.expect(true.B)
        val picosSubPacket = c.io.picosSubData.bits.peek().litValue
        println(toHexString(picosSubPacket))
        assert(((picosSubPacket >> 16) & 0xf) == numDeps)
        assert(((picosSubPacket >> 32) & 0xffffffffL) == taskNumber)
      }

      // TODO: Implement once we start supporting nesting
      val checkPacket1 = (parentID: Long) => {
        c.io.picosSubData.valid.expect(true.B)
      }

      val checkPacket2 = (nanosID: Long) => {
        val FPGA_MASK = 0x1
        val RUN_ON_ANY_ACCELERATOR_MASK = 0xff

        c.io.picosSubData.valid.expect(true.B)
        val picosSubPacket = c.io.picosSubData.bits.peek().litValue
        println(toHexString(picosSubPacket))
        assert(((picosSubPacket >> 32) & 0x3) == FPGA_MASK)
        assert(((picosSubPacket >> 40) & 0xff) == RUN_ON_ANY_ACCELERATOR_MASK)
        assert((picosSubPacket & 0xffffffffL) == nanosID)
      }

      val checkHeaderInnerRoutingPacket = () => {
        c.io.routingInfo.valid.expect(true.B)
        c.io.routingInfo.bits.arbID.expect(0.U)
        c.io.routingInfo.bits.numPackets.expect(3.U)
      }

      val checkPayloadInnerRoutingPacket = (numDeps: Int, numArgs: Int) => {
        c.io.routingInfo.valid.expect(true.B)
        c.io.routingInfo.bits.arbID.expect(1.U)
        c.io.routingInfo.bits.numPackets.expect((numDeps + numArgs).U)
      }

      val checkOuterRoutingPacket = (coreID: Int, numDeps: Int, numArgs: Int) => {
        c.io.routingInfoOuter.valid.expect(true.B)
        c.io.routingInfoOuter.bits.arbID.expect(coreID.U)
        c.io.routingInfoOuter.bits.numPackets.expect((3 + numDeps + numArgs).U)
      }

      val waitRandomAmountOfCycles = () => {
        val numWaitCycles = r.nextInt(20)
        for (k <- 0 until numWaitCycles) {
          c.clock.step()
        }
      }

      val testedCoreID = (numTestedCores + 2) % numTestedCores 
      
      c.io.coreID.poke(testedCoreID.U)

      for (i <- 0 until 100) {
        println("Running iteration " + i)
        val testedTaskNumber = i + 1
        val testedNumDeps = r.nextInt(16)
        val testedParentID = r.nextLong() & 0xffffffffL
        val testedNanosID = r.nextLong() & 0xffffffffL
        val testedNumArgs = 0

        placeTaskInfo(testedNumDeps, testedNanosID)
        c.io.picosSubData.valid.expect(false.B)

          waitRandomAmountOfCycles()

        checkHeaderInnerRoutingPacket()
        checkOuterRoutingPacket(testedCoreID, testedNumDeps, testedNumArgs)

        c.io.routingInfo.ready.poke(true.B)

          waitRandomAmountOfCycles()

        c.io.picosSubData.valid.expect(false.B)

          waitRandomAmountOfCycles()

        c.io.routingInfoOuter.ready.poke(true.B)
        c.io.picosSubData.valid.expect(true.B)
        c.io.picosSubData.ready.poke(true.B)

        checkPacket0(testedNumDeps, testedTaskNumber)

          c.clock.step()

        c.io.routingInfo.ready.poke(false.B)
        c.io.routingInfoOuter.ready.poke(false.B)
        c.io.picosSubData.ready.poke(false.B)

          waitRandomAmountOfCycles()

        c.io.routingInfo.ready.poke(true.B)
        c.io.routingInfoOuter.ready.poke(true.B)
        c.io.picosSubData.ready.poke(true.B)

        checkPacket1(testedParentID)
        checkPayloadInnerRoutingPacket(testedNumDeps, testedNumArgs)

          c.clock.step()

        c.io.routingInfo.ready.poke(false.B)
        c.io.routingInfoOuter.ready.poke(false.B)
        c.io.picosSubData.ready.poke(false.B)

          waitRandomAmountOfCycles()

        c.io.routingInfo.ready.poke(true.B)
        c.io.routingInfoOuter.ready.poke(true.B)
        c.io.picosSubData.ready.poke(true.B)

        checkPacket2(testedNanosID)

          c.clock.step()

        unplaceTaskInfo()
        c.io.picosSubData.ready.poke(false.B)
        c.io.routingInfo.ready.poke(false.B)
        c.io.routingInfoOuter.ready.poke(false.B)

          waitRandomAmountOfCycles()
      }
    }
  }
}
