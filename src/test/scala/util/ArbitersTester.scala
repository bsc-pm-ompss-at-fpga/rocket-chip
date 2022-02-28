package freechips.rocketchip.util
//package freechips.rocketchip.picos_v4_integration_elements

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import Array._
import scala.collection.SortedSet
import scala.collection.mutable.HashMap
import scala.util.Random._

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
class ArbiterTest extends AnyFlatSpec with ChiselScalatestTester {
  val numTestedCores = 8

	val r = new scala.util.Random()
	r.setSeed(1000)

	behavior of "ArbitraryPrefixerAndSuffixer"
  it should "produce a n-packet sequence for every input packet process, where the first and the last of these packets are the prefix and suffix described at the module instantiation, with the option to block prefixing" in {
    val testPrefix = 1111111
    val testSuffix = 2222222

    for (n <- 3 until 10) {
      test(new ArbitraryPrefixerAndSuffixer(UInt(64.W), n, testPrefix, testSuffix)) { c =>

        val checkThatPrefixingIsBlocked = () => {
          c.io.enablePrefixing.poke(false.B)
          c.io.in.valid.poke(true.B)
          c.io.out.valid.expect(false.B)

          c.io.in.valid.poke(false.B)
          c.io.enablePrefixing.poke(true.B)
          c.clock.step()
        }

        val checkWhetherOutputPacketIsAvailable = (value : Int, step : Boolean) => {
          c.io.out.bits.expect(value.U)
          c.io.out.valid.expect(true.B)
          c.io.out.ready.poke(true.B) // After this line, we can be sure a firing has happened
          if (step) {
            c.clock.step()
            c.io.out.ready.poke(false.B)
          }
        }

        val sendPacket = (testPacket : Int) => {
          var tryAgain = false

          do {
            tryAgain = false
            c.io.in.valid.poke(true.B)
            c.io.in.bits.poke(testPacket.U)
            c.io.out.ready.poke(true.B)
            if (c.io.in.ready.peek().litToBoolean) {
              checkWhetherOutputPacketIsAvailable(testPacket, false)
            } else {
              tryAgain = true
            }
            c.clock.step()
            c.io.in.valid.poke(false.B)
            c.io.in.bits.poke(0.U)
            c.io.out.ready.poke(false.B)
          } while (tryAgain)
        }

        val waitRandomAmountOfCycles = () => {
          val numWaitCycles = r.nextInt(20)
          for (k <- 0 until numWaitCycles) {
            c.clock.step()
          }
        }

        val testPacket = 123456789

        for (i <- 0 until 50) {
          checkThatPrefixingIsBlocked()

          checkWhetherOutputPacketIsAvailable(testPrefix, true)

          waitRandomAmountOfCycles()

          for (j <- 0 until n - 2) {
            sendPacket(testPacket)
            waitRandomAmountOfCycles()
          }

          checkWhetherOutputPacketIsAvailable(testSuffix, true)

          waitRandomAmountOfCycles()
        }
      }
    }
  }
  
	behavior of "RRArbiterWithPrefixAndSuffix"
  it should "should let data elements coming from independent input channels to be transmitted exactly once, while also being prefixed and suffixed" in {
    val inputType = UInt(64.W)
    val sequenceLength = 3
    val prefixValue = 3
    val suffixValue = 0
    val numTestLoops = 100

    test(new RRArbiterWithPrefixAndSuffix(inputType, numTestedCores, sequenceLength, prefixValue, suffixValue)) { c =>
      val generateInputMap = () => {                                                                                        
        var hashMap = new HashMap[Int, Int]()                                                                                 
        for (i <- 0 until numTestedCores) {
          if (nextBoolean()) {
            hashMap += (i -> i)
          }
        }
        hashMap                                                                                                             
      } 

      // TODO: Apply DRY to this function
      val checkWhetherOutputPacketIsAvailable = (value : Int, step : Boolean) => {
        c.io.out.bits.expect(value.U)
        c.io.out.valid.expect(true.B)
        c.io.out.ready.poke(true.B) // After this line, we can be sure a firing has happened
        if (step) {
          c.clock.step()
          c.io.out.ready.poke(false.B)
        }
      }

      val generateEmptyInputMap = () => {
        var hashMap = new HashMap[Int, Int]()                                                                                 
      }

      val driveEmptyInputMap = () => {
        for (i <- 0 until numTestedCores) {
          c.io.in(i).valid.poke(false.B)
        }
      }

      val driveInputMap = (inputMap : HashMap[Int, Int]) => {
        driveEmptyInputMap()
        inputMap.foreach{ case (idx, value) =>
          c.io.in(idx).valid.poke(true.B)
          c.io.in(idx).bits.poke(value.U)
        }
      }

      val ensureThatAllInputsAreTransmittedExactlyOnce = (inputMap : HashMap[Int, Int]) => {
        val numElements = inputMap.size
        driveInputMap(inputMap)
        for (i <- 0 until numElements) {
          checkWhetherOutputPacketIsAvailable(prefixValue, true)
          c.io.out.ready.poke(true.B)
          c.io.out.valid.expect(true.B)

          val fetchedValue = c.io.out.bits.peek().litValue.toInt

          assert(inputMap.contains(fetchedValue))
          inputMap.remove(fetchedValue)
          c.clock.step()
          c.io.out.ready.poke(false.B)
          checkWhetherOutputPacketIsAvailable(suffixValue, true)
          driveInputMap(inputMap)
        }
        assert(inputMap.size == 0)

        c.io.out.ready.poke(true.B)
        c.io.out.valid.expect(false.B)
        c.clock.step()
        c.io.out.ready.poke(false.B)
      }

      val ensureNothingIsBeingOutputted = () => {
        c.io.out.valid.expect(false.B)
      }

      for (i <- 0 until numTestLoops) {
        val inputMap = generateInputMap()

        ensureNothingIsBeingOutputted()
        c.clock.step()
        ensureThatAllInputsAreTransmittedExactlyOnce(inputMap)
      }
    }
  }
}
