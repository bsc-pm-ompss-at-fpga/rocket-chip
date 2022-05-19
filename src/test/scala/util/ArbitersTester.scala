package freechips.rocketchip.util
//package freechips.rocketchip.picos_v4_integration_elements

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import freechips.rocketchip.integrators.RetirementHandler

//import Array._
//import scala.collection.SortedSet
import scala.collection.mutable.HashMap
import scala.util.Random._
import scala.util.control.Breaks._

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
  it should "produce, from an (n-2) packet sequence, an n-packet sequence where the first and the last of these packets are the prefix and suffix described at the module instantiation, with the option to block prefixing" in {
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
  it should "let data elements coming from independent input channels to be transmitted exactly once, while also being prefixed and suffixed" in {
    val inputType = UInt(64.W)
    val sequenceLength = 3
    val prefixValue = 3
    val suffixValue = 0
    val numTestLoops = 100
    var triggerPacket = BigInt(0L)
    var currentCycle = 0

    test(new RRArbiterWithPrefixAndSuffix(inputType, numTestedCores, sequenceLength, prefixValue, suffixValue)) { c =>
      val waitRandomAmountOfCycles = () => {
        val numWaitCycles = r.nextInt(20)
        for (k <- 0 until numWaitCycles) {
          c.clock.step()
          currentCycle += 1
        }
      }

      val generateInputMap = () => {                                                                                        
        var hashMap = new HashMap[Int, (Int, Int)]()
        for (i <- 0 until numTestedCores) {
          if (nextBoolean()) {
            hashMap += (i -> (currentCycle + nextInt(10), i))
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
					currentCycle += 1
          c.io.out.ready.poke(false.B)
        }
      }

      val generateEmptyInputMap = () => {
        var hashMap = new HashMap[Int, (Int, Int)]()                                                                                 
      }

      val driveEmptyInputMap = () => {
        for (i <- 0 until numTestedCores) {
          c.io.in(i).valid.poke(false.B)
        }
      }

      val driveInputMap = (inputMap : HashMap[Int, (Int, Int)]) => {
        driveEmptyInputMap()
        inputMap.foreach{ case (idx, (minCycle, value)) =>
          val minCycleWasReached = minCycle <= currentCycle
          c.io.in(idx).valid.poke(minCycleWasReached.B)
          c.io.in(idx).bits.poke(value.U)
        }
      }

      val ensureThatAllInputsAreTransmittedExactlyOnce = (inputMap : HashMap[Int, (Int, Int)]) => {
        val numElements = inputMap.size
        driveInputMap(inputMap)
        for (i <- 0 until numElements) {
          //println(s"[ensureThatAllInputsAreTransmittedExactlyOnce]: Iteration #$i")
          while (c.io.arbiterMonitor.valid.peek().litValue == 0) {
            c.clock.step()
            currentCycle += 1
            driveInputMap(inputMap)
          }
            waitRandomAmountOfCycles()
            driveInputMap(inputMap)
          c.io.arbiterMonitor.valid.expect(true.B)
          //triggerPacket = c.io.arbiterMonitor.bits.peek().litValue

          //println(s"[ensureThatAllInputsAreTransmittedExactlyOnce]: c.io.triggerContent = ${c.io.triggerContent.peek().litValue}")

            waitRandomAmountOfCycles()
            driveInputMap(inputMap)
          c.io.last.expect(false.B)
            waitRandomAmountOfCycles()
            driveInputMap(inputMap)
          c.io.arbiterMonitor.valid.expect(true.B)
          triggerPacket = c.io.arbiterMonitor.bits.peek().litValue
          c.io.triggerContent.expect(triggerPacket.U)
          //println(s"[ensureThatAllInputsAreTransmittedExactlyOnce]: c.io.triggerContent = ${triggerPacket}")
          checkWhetherOutputPacketIsAvailable(prefixValue, true)
            driveInputMap(inputMap)
          c.io.out.ready.poke(false.B)
            waitRandomAmountOfCycles()
            driveInputMap(inputMap)
          c.io.out.ready.poke(true.B)
          c.io.out.valid.expect(true.B)

          c.io.last.expect(false.B)
          val fetchedValue = c.io.out.bits.peek().litValue.toInt


          //println(s"[ensureThatAllInputsAreTransmittedExactlyOnce:fetchedValue]: c.io.fetchedValue = ${fetchedValue}")
          c.io.out.bits.expect(triggerPacket.U)
          c.io.triggerContent.expect(triggerPacket.U)

          assert(inputMap.contains(fetchedValue))
          inputMap.remove(fetchedValue)
            c.clock.step()
            currentCycle += 1
            driveInputMap(inputMap)
          c.io.out.ready.poke(false.B)
            waitRandomAmountOfCycles()
            driveInputMap(inputMap)

          //println(s"[ensureThatAllInputsAreTransmittedExactlyOnce]: c.io.triggerContent = ${c.io.triggerContent.peek().litValue}")
          c.io.triggerContent.expect(triggerPacket.U)

          c.io.last.expect(true.B)
          checkWhetherOutputPacketIsAvailable(suffixValue, true)
            driveInputMap(inputMap)
        }
        assert(inputMap.size == 0)

        c.io.out.ready.poke(true.B)
        c.io.out.valid.expect(false.B)
          c.clock.step()
					currentCycle += 1
          driveInputMap(inputMap)
        c.io.out.ready.poke(false.B)

          waitRandomAmountOfCycles()
          driveInputMap(inputMap)
      }

      val ensureNothingIsBeingOutputted = () => {
        c.io.out.valid.expect(false.B)
      }

      c.io.enablePrefixing.poke(true.B)

      for (i <- 0 until numTestLoops) {
        //println(s"[RRArbiterWithPrefixAndSuffixTester:testEpoch]: $i")
        val inputMap = generateInputMap()

        ensureNothingIsBeingOutputted()
        c.clock.step()
        currentCycle += 1
        ensureThatAllInputsAreTransmittedExactlyOnce(inputMap)
      }
    }
  }

	behavior of "RetirementHandler"
  it should "Display about the same functionality as RRArbiterWithPrefixAndSuffix but with the ability to clean retirement packets and generate tID information" in {
    val inputType = UInt(64.W)
    val sequenceLength = 3
    val prefixValue = 3
    val suffixValue = 0
    val numTestLoops = 1000
    var triggerPacket = BigInt(0L)
    var currentCycle = 0

    test(new RetirementHandler(numTestedCores)) { c =>
      val waitRandomAmountOfCycles = () => {
        val numWaitCycles = r.nextInt(20)
        for (k <- 0 until numWaitCycles) {
          c.clock.step()
          currentCycle += 1
        }
      }

      val generateInputMap = () => {                                                                                        
        val hashMap = new HashMap[Int, (Int, BigInt)]()
        val reverseMap = new HashMap[BigInt, Int]
        
        for (i <- 0 until numTestedCores) {
          val testedTID = r.nextInt(1048576).toLong
          val testedPicosID = r.nextInt(10480576).toLong | (BigInt(r.nextInt(16)) << 60)
          val packet = testedPicosID | (testedTID << 32)
          if (nextBoolean() && !reverseMap.contains(packet)) {
            hashMap += (i -> (currentCycle + nextInt(10), packet))
            // TODO: ENSURE COLISIONS ARE IMPOSSIBLE!
            reverseMap += (testedPicosID -> i)
          }
        }
        (hashMap, reverseMap)
      } 

      // TODO: Apply DRY to this function
      val checkWhetherOutputPacketIsAvailable = (value : Int, step : Boolean) => {
        c.io.picosFinishedInterface.data.payload.expect(value.U)
        c.io.picosFinishedInterface.valid.expect(true.B)
        c.io.picosFinishedInterface.empty.poke(true.B) // After this line, we can be sure a firing has happened
        if (step) {
          c.clock.step()
					currentCycle += 1
          c.io.picosFinishedInterface.empty.poke(false.B)
        }
      }

      val generateEmptyInputMap = () => {
        var hashMap = new HashMap[Int, BigInt]()                                                                                 
      }

      val driveEmptyInputMap = () => {
        for (i <- 0 until numTestedCores) {
          c.io.retInterface(i).valid.poke(false.B)
        }
      }

      val driveInputMap = (inputMap : HashMap[Int, (Int, BigInt)]) => {
        driveEmptyInputMap()
        inputMap.foreach{ case (idx, (minCycle, value)) =>
          val minCycleWasReached = minCycle <= currentCycle
          c.io.retInterface(idx).valid.poke(minCycleWasReached.B)
          c.io.retInterface(idx).bits.poke(value.U)
        }
      }

      val ensureThatAllInputsAreTransmittedExactlyOnce = (inputMap: HashMap[Int, (Int, BigInt)], reverseMap: HashMap[BigInt, Int]) => {
        val numElements = inputMap.size
        driveInputMap(inputMap)
        for (i <- 0 until numElements) {
          //println(s"[ensureThatAllInputsAreTransmittedExactlyOnce]: Iteration #$i")
          while (c.io.picosFinishedInterface.valid.peek().litValue == 0) {
            c.clock.step()
            currentCycle += 1
            driveInputMap(inputMap)
          }
          //c.io.arbiterMonitor.valid.expect(true.B)
          //triggerPacket = c.io.arbiterMonitor.bits.peek().litValue

          //println(s"[ensureThatAllInputsAreTransmittedExactlyOnce]: c.io.triggerContent = ${c.io.triggerContent.peek().litValue}")
          //c.io.triggerContent.expect(triggerPacket.U)

          //c.io.last.expect(false.B)
          checkWhetherOutputPacketIsAvailable(prefixValue, true)
            driveInputMap(inputMap)
          c.io.picosFinishedInterface.empty.poke(true.B)
          c.io.picosFinishedInterface.valid.expect(true.B)

          //c.io.last.expect(false.B)
          val fetchedPicosID = c.io.picosFinishedInterface.data.payload.peek().litValue

          //println(s"[ensureThatAllInputsAreTransmittedExactlyOnce]: c.io.triggerContent = ${c.io.triggerContent.peek().litValue}")
          //c.io.triggerContent.expect(triggerPacket.U)

          assert(reverseMap.contains(fetchedPicosID))

          val idx = reverseMap(fetchedPicosID)
          val tidGroundTruth = (inputMap(idx)._2 >> 32) & 0xfffffffL
          reverseMap.remove(fetchedPicosID)
          inputMap.remove(idx)

          c.io.picosFinishedInterface.data.tid.expect(tidGroundTruth.U)
            c.clock.step()
            currentCycle += 1
            driveInputMap(inputMap)
          c.io.picosFinishedInterface.empty.poke(false.B)

            waitRandomAmountOfCycles()
            driveInputMap(inputMap)

          //println(s"[ensureThatAllInputsAreTransmittedExactlyOnce]: c.io.triggerContent = ${c.io.triggerContent.peek().litValue}")
          //c.io.triggerContent.expect(triggerPacket.U)

          //c.io.last.expect(true.B)
          checkWhetherOutputPacketIsAvailable(suffixValue, true)
          driveInputMap(inputMap)
            waitRandomAmountOfCycles()
            driveInputMap(inputMap)
        }
        assert(inputMap.size == 0)

        c.io.picosFinishedInterface.empty.poke(true.B)
        c.io.picosFinishedInterface.valid.expect(false.B)
          c.clock.step()
					currentCycle += 1
        c.io.picosFinishedInterface.empty.poke(false.B)
      }

      val ensureNothingIsBeingOutputted = () => {
        c.io.picosFinishedInterface.valid.expect(false.B)
      }

      for (i <- 0 until numTestLoops) {
        val (inputMap, reverseMap) = generateInputMap()

        ensureNothingIsBeingOutputted()
          c.clock.step()
					currentCycle += 1
        ensureThatAllInputsAreTransmittedExactlyOnce(inputMap, reverseMap)
      }
    }
  }

} // DO NOT COMMENT OUT
