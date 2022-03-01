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

	behavior of "ReadyPacketExtractor"
  it should "be able to be instantiated within this tester" in {
    test(new ReadyPacketExtractor(4)) { c =>
    }
  }

  it should "send ready task data coming from Picos to the requesting cores respecting the petition order, at least when there is never more than one routing request packet in queue" in {
    test(new ReadyPacketExtractor(numTestedCores)) { c =>
      val toHexString = (num : BigInt) => {
        f"${num}%X"
      }

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
}
