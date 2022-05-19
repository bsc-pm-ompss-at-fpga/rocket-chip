package freechips.rocketchip.integrators

import chisel3._
import chisel3.util.{DecoupledIO}
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ListBuffer
import scala.util.Random._
import picos_v4_integration_elements.{ArgsAndParentInfo, DepInfo, TaskInfo}

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
  val maxTID = 255

	val r = new scala.util.Random()
	//r.setSeed(1000)
	r.setSeed(10)

  val toHexString = (num : BigInt) => {
    f"${num}%X"
  }

  val placeTaskInfo = (c: DecoupledIO[TaskInfo], numDeps: Int, nanosID: Long) => {
      c.valid.poke(true.B)
      c.bits.num_deps.poke(numDeps.U)
      c.bits.task_id.poke(nanosID.U)
  }

  val placeArgsAndParentInfo = (c: DecoupledIO[ArgsAndParentInfo], numArgs: Int, parentID: Long) => {
      c.valid.poke(true.B)
      c.bits.num_args.poke(numArgs.U)
      c.bits.parent_id.poke(parentID.U)
  }

  val unplaceTaskInfo = (c: DecoupledIO[TaskInfo]) => {
      c.valid.poke(false.B)
      c.bits.num_deps.poke(0.U)
      c.bits.task_id.poke(0.U)
  }

  val unplaceArgsAndParentInfo = (c: DecoupledIO[ArgsAndParentInfo]) => {
      c.valid.poke(false.B)
      c.bits.num_args.poke(0.U)
      c.bits.parent_id.poke(0.U)
  }

  val placeDepInfo = (c: DecoupledIO[Vec[DepInfo]], pointer0: Long, kind0: Int, pointer1: Long, kind1: Int) => {
      c.valid.poke(true.B)
      c.bits(0).pointer.poke(pointer0.U)
      c.bits(0).kind.poke(kind0.U)
      c.bits(1).pointer.poke(pointer1.U)
      c.bits(1).kind.poke(kind1.U)
  }

  val unplaceDepInfo = (c: DecoupledIO[Vec[DepInfo]]) => {
      c.valid.poke(false.B)
      c.bits(0).pointer.poke(0.U)
      c.bits(0).kind.poke(0.U)
      c.bits(1).pointer.poke(0.U)
      c.bits(1).kind.poke(0.U)
  }

  def checkPacket0 (c: DecoupledIO[UInt], numDeps: Int, taskNumber: Long, rawNumArgs: Int = 0, addIDArg: Boolean = true) {
    c.valid.expect(true.B)
    val picosSubPacket = c.bits.peek().litValue
    
    var numArgs = rawNumArgs
    if (addIDArg) {
      numArgs = rawNumArgs + 1
    }

    println(s"Required number of deps, arguments, taskNumber: $numDeps, $numArgs, $taskNumber")
    println(toHexString(picosSubPacket))

    assert(((picosSubPacket >> 8) & 0xf) == numArgs)
    assert(((picosSubPacket >> 16) & 0xf) == numDeps)
    assert(((picosSubPacket >> 32) & 0xffffffffL) == taskNumber)
  }

  // TODO: Implement once we start supporting nesting
  val checkPacket1 = (c: DecoupledIO[UInt], parentID: Long) => {
    c.valid.expect(true.B)
    c.bits.expect(parentID.U)
  }

  val checkPacket2 = (c: DecoupledIO[UInt]) => {
    val FPGA_MASK = 0x1
    val RUN_ON_ANY_ACCELERATOR_MASK = 0xff

    c.valid.expect(true.B)
    val picosSubPacket = c.bits.peek().litValue
    //println(toHexString(picosSubPacket))
    assert(((picosSubPacket >> 32) & 0x3) == FPGA_MASK)
    assert(((picosSubPacket >> 40) & 0xff) == RUN_ON_ANY_ACCELERATOR_MASK)
    assert((picosSubPacket & 0xffffffffL) == 0)
  }

  val checkSWIDPacket = (c: DecoupledIO[UInt], nanosID: Long) => {
    // THE SCALA COMPILER SEEMS TO BE ABLE TO
    // CHANGE THE ORDER OF EXPECT AND ASSERT
    // STATEMENTS! WATCH OUT!
    c.valid.expect(true.B)
    val picosSubPacket = c.bits.peek().litValue

    c.bits.expect(nanosID.U)
    //assert(picosSubPacket == nanosID)
  }

  val setAckSignal = (c: PicosFifoBundle[UInt]) => {
    c.valid.poke(true.B)
    c.data.poke(1.U)
  }

  val unsetAckSignal = (c: PicosFifoBundle[UInt]) => {
    c.valid.poke(false.B)
    c.data.poke(0.U)
  }

  val waitRandomAmountOfCycles = (c: Module) => {
    val numWaitCycles = r.nextInt(20)
    for (k <- 0 until numWaitCycles) {
      c.clock.step()
    }
  }

  val setTID = (c: PicosFifoBundle[PicosRawReadyBundle], tID: Int) => {
    c.valid.poke(true.B)
    c.data.tid.poke(tID.U)
  }

  val unsetTID = (c: PicosFifoBundle[PicosRawReadyBundle], tID: Int) => {
    c.valid.poke(false.B)
  }

  val checkOuterRoutingPacket = (c: CoreSpecificSubmissionHandler[UInt], coreID: Int, numDeps: Int, numArgs: Int) => {
    c.io.routingInfoOuter.valid.expect(true.B)
    c.io.routingInfoOuter.bits.arbID.expect(coreID.U)
    c.io.routingInfoOuter.bits.numPackets.expect((3 + numDeps + numArgs + 1).U)
  }

  def asUnsigned(unsignedLong: Long) =
    (BigInt(unsignedLong >>> 1) << 1) + (unsignedLong & 1)

  /*
	behavior of "DependenceInfoAdapter"
  it should "generate one Picos submission packet per depInfo packet whenever the second dependence is invalid" in {
      test(new DependenceInfoAdapter) { c =>
          val INVALID_TYPE = 0
          val IN_TYPE = 1
          val OUT_TYPE = 2
          val INOUT_TYPE = 3

          for (i <- 0 until 100) {
            val testPointer = r.nextLong() & 0xffffffffffffffL
            val testKind = r.nextInt(3) + 1

            c.io.picosSubData.ready.poke(true.B)
            placeDepInfo(c.io.depInfo, testPointer, testKind, 0, INVALID_TYPE)

            c.io.picosSubData.valid.expect(true.B)
            c.io.picosSubData.bits.expect((BigInt(testKind) << 58 | BigInt(testPointer)).U(64.W))
              c.clock.step()
          }
      }
  }

  it should "generate two Picos submission packets per depInfo packet whenever the second dependence is valid" in {
      test(new DependenceInfoAdapter) { c =>
          val INVALID_TYPE = 0
          val IN_TYPE = 1
          val OUT_TYPE = 2
          val INOUT_TYPE = 3

          for (i <- 0 until 100) {
            val testPointer0 = r.nextLong() & 0xffffffffffffffL
            val testKind0 = r.nextInt(3) + 1
            val testPointer1 = r.nextLong() & 0xffffffffffffffL
            val testKind1 = r.nextInt(3) + 1

            c.io.picosSubData.ready.poke(true.B)
            placeDepInfo(c.io.depInfo, testPointer0, testKind0, testPointer1, testKind1)

            c.io.picosSubData.valid.expect(true.B)
            c.io.picosSubData.bits.expect((BigInt(testKind0) << 58 | BigInt(testPointer0)).U(64.W))

              c.clock.step()

            c.io.picosSubData.valid.expect(true.B)
            c.io.picosSubData.bits.expect((BigInt(testKind1) << 58 | BigInt(testPointer1)).U(64.W))

              c.clock.step()
          }
      }
  }

  it should "be able handle depInfo sequences where some entries have two valid dependencies and others have only one" in {
      test(new DependenceInfoAdapter) { c =>
          val INVALID_TYPE = 0
          val IN_TYPE = 1
          val OUT_TYPE = 2
          val INOUT_TYPE = 3

          for (i <- 0 until 100) {
            val testPointer0 = r.nextLong() & 0xffffffffffffffL
            val testKind0 = r.nextInt(3) + 1
            val testPointer1 = r.nextLong() & 0xffffffffffffffL
            val testKind1 = r.nextInt(4)

            c.io.picosSubData.ready.poke(true.B)
            placeDepInfo(c.io.depInfo, testPointer0, testKind0, testPointer1, testKind1)

            c.io.picosSubData.valid.expect(true.B)
            c.io.picosSubData.bits.expect((BigInt(testKind0) << 58 | BigInt(testPointer0)).U(64.W))

              c.clock.step()

            if (testKind1 != 0) {
              c.io.picosSubData.valid.expect(true.B)
              c.io.picosSubData.bits.expect((BigInt(testKind1) << 58 | BigInt(testPointer1)).U(64.W))

                c.clock.step()
            }
          }
      }
  }

	behavior of "CoreSpecificSubmissionHandler"
  it should "produce a valid header sequence" in {
      test(new CoreSpecificSubmissionHandler(numTestedCores, 33)) { c =>
          val testCoreID = 7
          c.io.enableInternalNumbering.poke(true.B)

          for (i <- 0 until 100) {
            // TODO: Change this once we implement user-provided
            //       argument passing
            val testNumArgs = 0
            val testTaskNumber = i

            //val testNumDeps = r.nextInt(16)
            val testNumDeps = r.nextInt(16)
            val testParentID = r.nextLong() & 0xffffffffL
            val testNanosID = r.nextLong() & 0xffffffffL
            //val testNanosID = r.nextLong() & 0xffL
            //val testNanosID = 0xdeadbeefL

            val INVALID_TYPE = 0
            val IN_TYPE = 1
            val OUT_TYPE = 2
            val INOUT_TYPE = 3

            //println(f"Iteration: $i")

            c.io.coreID.poke(testCoreID.U)
            c.io.routingInfoOuter.valid.expect(false.B)

            placeTaskInfo(c.io.taskInfo, testNumDeps, testNanosID)
            c.io.taskInfo.ready.expect(true.B)
              c.clock.step()
            unplaceTaskInfo(c.io.taskInfo)

            placeArgsAndParentInfo(c.io.argsAndParentInfo, testNumArgs, testParentID)
            c.io.argsAndParentInfo.ready.expect(true.B)
              c.clock.step()
            unplaceArgsAndParentInfo(c.io.argsAndParentInfo)

            c.io.routingInfoOuter.valid.expect(true.B)
            checkOuterRoutingPacket(c, testCoreID, testNumDeps, testNumArgs)

            // The leaf GuidedArbiter has not processed the routing packet
            // that allows submission header packets to go through, so
            // the following interface should not hold any valid data
            c.io.picosSubData.valid.expect(false.B)

            c.io.routingInfoOuter.ready.poke(true.B)
              c.clock.step()
            c.io.routingInfoOuter.ready.poke(false.B)

              c.clock.step()

            checkPacket0(c.io.picosSubData, testNumDeps, testTaskNumber)

            c.io.picosSubData.ready.poke(true.B)
              c.clock.step()
            c.io.picosSubData.ready.poke(false.B)

            // TODO: For the following to work, we'll need to allow
            //       CoreSpecificSubmissionHandler to buffer at least
            //       one routing packet for the leaf GuidedArbiter
            checkPacket1(c.io.picosSubData, testParentID)

            c.io.picosSubData.ready.poke(true.B)
              c.clock.step()
            c.io.picosSubData.ready.poke(false.B)

            checkPacket2(c.io.picosSubData)

            c.io.picosSubData.ready.poke(true.B)
              c.clock.step()
            c.io.picosSubData.ready.poke(false.B)

              // One extra step is needed because the routing packet
              // for the payload section takes one cycle to be fetched
              c.clock.step()

              waitRandomAmountOfCycles(c)

            var j = 0
            var tries = 0
            var max_tries = 100
            //println(f"Num deps: $testNumDeps")
            while (j < testNumDeps) {
              val testDepPointer0 = r.nextLong() & 0xffffffffffffffL
              val testDepKind0 = r.nextInt(3) + 1
              val testDepPointer1 = r.nextLong() & 0xffffffffffffffL
              val testDepKind1 = if (j + 1 < testNumDeps) r.nextInt(4) else 0

              //println(f"Pushed dependencies: (${testDepPointer0.toHexString}, ${testDepPointer1.toHexString})")

              //println("Dep #" + j)
              j += 1

              placeDepInfo(c.io.depInfo, testDepPointer0, testDepKind0, testDepPointer1, testDepKind1)
              c.io.depInfo.ready.expect(true.B)
                c.clock.step()
              unplaceDepInfo(c.io.depInfo)

              tries = 0
              while (tries < max_tries) {
                if (c.io.picosSubData.valid.peek().litToBoolean) {
                  tries = max_tries + 1
                } else {
                  tries += 1
                  c.clock.step()
                }
              }
              assert(tries != max_tries)

              //println(f"Fetched packet: ${toHexString(c.io.picosSubData.bits.peek().litValue)}")
              c.io.picosSubData.bits.expect((BigInt(testDepKind0) << 58 | BigInt(testDepPointer0)).U(64.W))

              c.io.picosSubData.ready.poke(true.B)
                c.clock.step()
              c.io.picosSubData.ready.poke(false.B)

              if (testDepKind1 != 0) {
                //println("Dep #" + j)
                j += 1

                tries = 0
                while (tries < max_tries) {
                  if (c.io.picosSubData.valid.peek().litToBoolean) {
                    tries = max_tries + 1
                  } else {
                    tries += 1
                    c.clock.step()
                  }
                }
                assert(tries != max_tries)
                //println(f"Fetched packet: ${toHexString(c.io.picosSubData.bits.peek().litValue)}")
                c.io.picosSubData.bits.expect((BigInt(testDepKind1) << 58 | BigInt(testDepPointer1)).U(64.W))

                c.io.picosSubData.ready.poke(true.B)
                  c.clock.step()
                c.io.picosSubData.ready.poke(false.B)
              }

                waitRandomAmountOfCycles(c)
            }

              tries = 0
              while (tries < max_tries) {
                if (c.io.picosSubData.valid.peek().litToBoolean) {
                  tries = max_tries + 1
                } else {
                  tries += 1
                  c.clock.step()
                }
              }
              assert(tries != max_tries)
              checkSWIDPacket(c.io.picosSubData, testNanosID)
              //println(f"Fetched swID packet: ${toHexString(c.io.picosSubData.bits.peek().litValue)}")
              c.io.picosSubData.ready.poke(true.B)
                  c.clock.step()
              c.io.picosSubData.ready.poke(false.B)

              waitRandomAmountOfCycles(c)
          }
      }
  }

	behavior of "ReadyPacketExtractor"
  it should "send ready task data coming from Picos to the requesting cores respecting the petition order, at least when there is never more than one routing request packet in queue" in {
    test(new ReadyPacketExtractor(numTestedCores)) { c =>
      val generateRandomInput = () => {
        var inputList : ListBuffer[BigInt] = ListBuffer()
        val sequenceLength = 5
        
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
          c.io.picosReadyInterface.data.payload.poke(testPacket.U)
          if (c.io.picosReadyInterface.empty.peek().litToBoolean) {
          } else {
            tryAgain = true
          }
          c.clock.step()
          c.io.picosReadyInterface.valid.poke(false.B)
          c.io.picosReadyInterface.data.payload.poke(0.U)
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
        var tID: BigInt = 0

        override def toString() : String = {
          val baseList = List(numArgs, picosID, parentID, nanosID, tID)
          val hexList = baseList.map(toHexString(_))
          val innerString = hexList.foldRight("")(_ + ", " + _).dropRight(2)
          return "(numArgs, picosID, parentID, nanosID, tID) = (" + innerString + ")"
        }

        override def equals(other: Any): Boolean = {
          other match {
            case other: ReadyTaskInfo => picosID == other.picosID && nanosID == other.nanosID && tID == other.tID
            case _ => false
          }
        }
      }

      val interpretInput = (input: ListBuffer[BigInt]) => {
        assert(input.size == 5)
        var readyTask = new ReadyTaskInfo
        readyTask.numArgs  = input(0) & 0xff00
        readyTask.picosID  = input(1) & 0xffffffffL
        readyTask.parentID = input(2)
        readyTask.nanosID  = input(4)

        readyTask
      }

      val fetchAnswerForCore = (coreID : Int) => {
        c.io.readyQInterface(coreID).ready.poke(true.B)
        while (!c.io.readyQInterface(coreID).valid.peek().litToBoolean) {
          c.clock.step()
        }

        var readyTask = new ReadyTaskInfo

        val rawPicosID =  c.io.readyQInterface(coreID).bits.picosID.peek().litValue
        val cleanPicosID = rawPicosID & 0xffffffffL
        val tID = rawPicosID >> 32

        readyTask.nanosID = c.io.readyQInterface(coreID).bits.nanosID.peek().litValue
        readyTask.picosID = cleanPicosID
        readyTask.tID = tID

        c.clock.step()
        c.io.readyQInterface(coreID).ready.poke(false.B)

        readyTask
      }

      val numReadyTaskFetches = 10
      for (i <- 0 until numReadyTaskFetches) {
        val randomInputSequence = generateRandomInput()
        val targetCore = i % numTestedCores
        val interpretedInput = interpretInput(randomInputSequence)

        val testedTID = r.nextInt(maxTID + 1)

        //println(s"Iteration $i")

        interpretedInput.tID = testedTID
        //println("Ground truth: " + interpretedInput)

        //println(interpretedInput)
        setTID(c.io.picosReadyInterface, testedTID)
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
      val checkHeaderInnerRoutingPacket = () => {
        c.io.routingInfo.valid.expect(true.B)
        c.io.routingInfo.bits.arbID.expect(0.U)
        c.io.routingInfo.bits.numPackets.expect(3.U)
      }

      val checkPayloadInnerRoutingPacket = (numDeps: Int) => {
        c.io.routingInfo.valid.expect(true.B)
        c.io.routingInfo.bits.arbID.expect(1.U)
        c.io.routingInfo.bits.numPackets.expect((numDeps).U)
      }

      val checkPayloadArgsAddingInnerRoutingPacket = (numArgs: Int) => {
        c.io.routingInfo.valid.expect(true.B)
        c.io.routingInfo.bits.arbID.expect(2.U)
        c.io.routingInfo.bits.numPackets.expect((numArgs + 1).U)
      }

      val checkOuterRoutingPacket = (coreID: Int, numDeps: Int, numArgs: Int) => {
        c.io.routingInfoOuter.valid.expect(true.B)
        c.io.routingInfoOuter.bits.arbID.expect(coreID.U)
        c.io.routingInfoOuter.bits.numPackets.expect((3 + numDeps + numArgs + 1).U)
      }

      val testedCoreID = (numTestedCores + 2) % numTestedCores 
      
      c.io.coreID.poke(testedCoreID.U)

      for (i <- 0 until 100) {
        //println("Running iteration " + i)
        val testedTaskNumber = i
        val testedNumDeps = r.nextInt(16)
        val testedParentID = r.nextLong() & 0xffffffffL
        val testedNanosID = r.nextLong() & 0xffffffffL
        val testedNumArgs = r.nextInt(15)

        placeTaskInfo(c.io.taskInfo, testedNumDeps, testedNanosID)
        placeArgsAndParentInfo(c.io.argsAndParentInfo, testedNumArgs, testedParentID)
        c.io.picosSubData.valid.expect(false.B)

          waitRandomAmountOfCycles(c)

        checkHeaderInnerRoutingPacket()
        checkOuterRoutingPacket(testedCoreID, testedNumDeps, testedNumArgs)

        c.io.routingInfo.ready.poke(true.B)
        c.io.routingInfoOuter.ready.poke(false.B)
        c.io.picosSubData.valid.expect(false.B)

        c.io.routingInfo.ready.poke(false.B)
        c.io.routingInfoOuter.ready.poke(true.B)
        c.io.swID.ready.poke(false.B)
        c.io.picosSubData.valid.expect(false.B)

          waitRandomAmountOfCycles(c)

        c.io.routingInfo.ready.poke(true.B)
        c.io.routingInfoOuter.ready.poke(true.B)
        c.io.picosSubData.ready.poke(true.B)
        c.io.picosSubData.valid.expect(false.B)

        c.io.swID.ready.poke(true.B)
        c.io.picosSubData.valid.expect(true.B)

        checkPacket0(c.io.picosSubData, testedNumDeps, testedTaskNumber, testedNumArgs)

          c.clock.step()
        c.io.swID.ready.poke(false.B)

        c.io.routingInfo.ready.poke(false.B)
        c.io.routingInfoOuter.ready.poke(false.B)
        c.io.picosSubData.ready.poke(false.B)
        c.io.swID.ready.poke(false.B)

          waitRandomAmountOfCycles(c)

        c.io.routingInfo.ready.poke(false.B)
        c.io.picosSubData.valid.expect(false.B)

        c.io.routingInfo.ready.poke(true.B)
        c.io.routingInfoOuter.ready.poke(true.B)
        c.io.picosSubData.ready.poke(true.B)

        checkPacket1(c.io.picosSubData, testedParentID)
        checkPayloadInnerRoutingPacket(testedNumDeps)

          c.clock.step()

        c.io.routingInfo.ready.poke(false.B)
        c.io.routingInfoOuter.ready.poke(false.B)
        c.io.picosSubData.ready.poke(false.B)

          waitRandomAmountOfCycles(c)

        c.io.routingInfo.ready.poke(false.B)
        c.io.picosSubData.valid.expect(false.B)

        c.io.routingInfo.ready.poke(true.B)
        c.io.routingInfoOuter.ready.poke(true.B)
        c.io.picosSubData.ready.poke(true.B)

        checkPacket2(c.io.picosSubData)
        checkPayloadArgsAddingInnerRoutingPacket(testedNumArgs)
        
        // The following lines ensure that one packet is consumed
        // from each of these interfaces at the end of each header
        // generation cycle
        c.io.taskInfo.valid.expect(true.B)
        c.io.taskInfo.ready.expect(true.B)
        c.io.argsAndParentInfo.valid.expect(true.B)
        c.io.argsAndParentInfo.ready.expect(true.B)

          c.clock.step()

        unplaceTaskInfo(c.io.taskInfo)
        unplaceArgsAndParentInfo(c.io.argsAndParentInfo)
        c.io.picosSubData.ready.poke(false.B)
        c.io.routingInfo.ready.poke(false.B)
        c.io.routingInfoOuter.ready.poke(false.B)

          waitRandomAmountOfCycles(c)
      }
    }
  }

	behavior of "ResubmissionHandler"
  it should "Ensure submission sequences are re-submitted after being first sent to Picos if, and only if, Picos rejects that sequence" in {
    test(new ResubmissionHandler) { c =>
      class RawSubmissionSequence(val maxSequenceLength: Int = 33) {
        var inputList : ListBuffer[BigInt] = ListBuffer()

        val sequenceLength = 3 + r.nextInt(maxSequenceLength + 1 - 3)
        val dest = if (sequenceLength == 3) 3 else 2
        
        for (i <- 0 until sequenceLength) {
          val newPacket = asUnsigned(nextLong().abs)
          //println("newPacket: " + toHexString(newPacket))
          inputList += newPacket
        }

        def submitSinglePacket(io: DecoupledIO[UInt], module: Module, lastSignal: Bool, testPacket: BigInt, destSignal: UInt, testDest: Int, isLast: Boolean = false): Unit = {
          var tryAgain = false

          do {
            tryAgain = false
            io.valid.poke(true.B)
            io.bits.poke(testPacket.U)
            if (io.ready.peek().litToBoolean) {
              destSignal.poke(testDest.U)
              if (isLast) {
                lastSignal.poke(true.B)
              }
            } else {
              tryAgain = true
            }
            module.clock.step()
            io.valid.poke(false.B)
            lastSignal.poke(false.B)
            io.bits.poke(0.U)
          } while (tryAgain)
        }

        def checkSinglePacket(io: DecoupledIO[UInt], module: Module, lastSignal: Bool, testPacket: BigInt, destSignal: UInt, testDest: Int, isLast: Boolean = false): Unit = {
          var tryAgain = false

          do {
            tryAgain = false
            io.ready.poke(true.B)
            if (io.valid.peek().litToBoolean) {
              lastSignal.expect(isLast.B)
              destSignal.expect(testDest.U)
              io.bits.expect(testPacket.U)
            } else {
              tryAgain = true
            }
            module.clock.step()
            io.ready.poke(false.B)
          } while (tryAgain)
        }

        override def toString() : String = {
          val hexList = inputList.map(toHexString(_))
          val innerString = hexList.foldRight("")(_ + ", " + _).dropRight(2)
          return "(" + innerString + ")"
        }

        val submitSequence = (io: DecoupledIO[UInt], module: Module, lastSignal: Bool, destSignal: UInt) => {
          val testDest = if (inputList.size == 3) 3 else 2
          inputList.zipWithIndex.foreach { case (v, i) => 
            val isLast = (i == (inputList.size - 1))
            submitSinglePacket(io, module, lastSignal, v, destSignal, testDest, isLast)
          }
        }

        val checkIncomingSequence = (io: DecoupledIO[UInt], module: Module, lastSignal: Bool, destSignal: UInt) => {
          val testDest = if (inputList.size == 3) 3 else 2
          inputList.zipWithIndex.foreach { case (v, i) => 
            val isLast = (i == (inputList.size - 1))

            //println(s"Checking packet #$i from ${inputList.size}")
            checkSinglePacket(io, module, lastSignal, v, destSignal, testDest, isLast)
          }
        }
      }

      c.io.picosSubMaster.valid.expect(false.B)
      c.io.rawPackets.valid.poke(true.B)
      c.io.lastIn.poke(true.B)
      c.clock.step()

      c.io.rawPackets.valid.poke(false.B)
      c.io.lastIn.poke(false.B)
      c.io.picosSubMaster.valid.expect(true.B)
      c.io.lastOut.expect(true.B)
      c.io.picosSubMaster.ready.poke(true.B)
      c.clock.step()

      c.io.picosSubMaster.ready.poke(false.B)
      c.io.picosAckInterface.data.poke(1.U)
      c.io.picosAckInterface.valid.poke(true.B)
      c.clock.step()

      c.io.picosAckInterface.valid.poke(false.B)
      c.clock.step()
      c.clock.step()
      c.clock.step()

      for (i <- 0 until 1000) {
        //println(s"Submission #$i")

        val packetSeq = new RawSubmissionSequence
        //println(s"Random sequence: ${packetSeq}")
        packetSeq.submitSequence(c.io.rawPackets, c, c.io.lastIn, c.io.destIn)

        c.clock.step()

        c.io.rawPackets.ready.expect(false.B)
        packetSeq.checkIncomingSequence(c.io.picosSubMaster, c, c.io.lastOut, c.io.dest)
        c.io.rawPackets.ready.expect(false.B)

        while (r.nextBoolean()) {
          //println("Checking sequence re-submission")
          c.io.picosAckInterface.data.poke(0.U)
          c.io.picosAckInterface.valid.poke(true.B)
          c.clock.step()

          c.io.rawPackets.ready.expect(false.B)
          c.io.picosAckInterface.valid.poke(false.B)

          c.clock.step()

          packetSeq.checkIncomingSequence(c.io.picosSubMaster, c, c.io.lastOut, c.io.dest)
        }

        c.io.picosAckInterface.data.poke(1.U)
        c.io.picosAckInterface.valid.poke(true.B)
        c.clock.step()

        c.io.rawPackets.ready.expect(true.B)
        c.io.picosAckInterface.valid.poke(false.B)
      }
    }
  }
  */

	behavior of "SubmissionHandler"
  it should "Process submission sequences coming from different cores in an ack-dependent way" in {
    test(new SubmissionHandler(numTestedCores)) { c =>
      var firstSubmission = true

      c.io.enabled.poke(true.B)

      for (i <- 0 until 1000) {
        val testNumArgs = 0
        val testTaskID = 1332341342L
        val testTaskNumber = i

        val testNumDeps = r.nextInt(16)
        val testCoreID = r.nextInt(numTestedCores)
        val testParentID = r.nextLong() & 0xffffffffL
        val testNanosID = r.nextLong() & 0xffffffffL
        val testDest = if (testNumDeps == 0) 3 else 2

        val INVALID_TYPE = 0
        val IN_TYPE = 1
        val OUT_TYPE = 2
        val INOUT_TYPE = 3

        println(s"Test #$i")
        //println(s"Num deps: $testNumDeps")

        if (!firstSubmission) {
            c.clock.step()
            c.clock.step()
            c.clock.step()
          waitRandomAmountOfCycles(c)
          c.io.picosSubMaster.valid.expect(false.B)
          setAckSignal(c.io.picosAckInterface)
            c.clock.step()
          unsetAckSignal(c.io.picosAckInterface)
            c.clock.step()
            //c.clock.step()
        }

        c.io.picosSubMaster.valid.expect(false.B)
        placeTaskInfo(c.io.taskInfoVec(testCoreID), testNumDeps, testNanosID)
          c.clock.step()
        unplaceTaskInfo(c.io.taskInfoVec(testCoreID))
        placeArgsAndParentInfo(c.io.argsAndParentInfoVec(testCoreID), testNumArgs, testParentID)
          c.clock.step()
        unplaceArgsAndParentInfo(c.io.argsAndParentInfoVec(testCoreID))
          c.clock.step()
          c.clock.step()
          c.clock.step()
          //println(s"(picosSubMaster.bits) = ${c.io.picosSubMaster.bits.peek().litValue.toString(16)}")

        if (firstSubmission) {
          // The system has not completed any submission sequence yet,
          // so there is no need to wait for an acknowledgement
            c.clock.step()
            c.clock.step()
          c.io.picosSubMaster.valid.expect(true.B)
          firstSubmission = false
        }

          waitRandomAmountOfCycles(c)
        //println("Just crossed firstSubmission zone")

        checkPacket0(c.io.picosSubMaster, testNumDeps, testTaskNumber)
        c.io.dest.expect(testDest.U)

        c.io.picosSubMaster.ready.poke(true.B)
          c.clock.step()
        c.io.picosSubMaster.ready.poke(false.B)
          //println(s"(picosSubMaster.bits) = ${c.io.picosSubMaster.bits.peek().litValue.toString(16)}")

        checkPacket1(c.io.picosSubMaster, testParentID)
        c.io.dest.expect(testDest.U)

        c.io.picosSubMaster.ready.poke(true.B)
          c.clock.step()
        c.io.picosSubMaster.ready.poke(false.B)
          waitRandomAmountOfCycles(c)
          //println(s"(picosSubMaster.bits) = ${c.io.picosSubMaster.bits.peek().litValue.toString(16)}")

        checkPacket2(c.io.picosSubMaster)
        c.io.dest.expect(testDest.U)

        c.io.picosSubMaster.ready.poke(true.B)
        c.clock.step()
        c.io.picosSubMaster.ready.poke(false.B)

        var j = 0

          waitRandomAmountOfCycles(c)

        while (j < testNumDeps) {
          val testDepPointer0 = r.nextLong() & 0xffffffffffffffL
          val testDepKind0 = r.nextInt(3) + 1
          val testDepPointer1 = r.nextLong() & 0xffffffffffffffL
          val testDepKind1 = if (j + 1 < testNumDeps) r.nextInt(4) else 0

          //println("Dep #" + j)

          placeDepInfo(c.io.depInfoVec(testCoreID), testDepPointer0, testDepKind0, testDepPointer1, testDepKind1)
            c.clock.step()
          unplaceDepInfo(c.io.depInfoVec(testCoreID))

            c.clock.step()
          c.io.picosSubMaster.valid.expect(true.B)
          c.io.picosSubMaster.bits.expect((BigInt(testDepKind0) << 58 | BigInt(testDepPointer0)).U(64.W))
          c.io.dest.expect(testDest.U)
          j += 1

          //println(s"(picosSubMaster.bits) = ${c.io.picosSubMaster.bits.peek().litValue.toString(16)}")

          c.io.picosSubMaster.ready.poke(true.B)
            c.clock.step()
          c.io.picosSubMaster.ready.poke(false.B)

            waitRandomAmountOfCycles(c)

          if (testDepKind1 != 0) {
            //println("Dep #" + j)
            c.io.picosSubMaster.valid.expect(true.B)
            c.io.picosSubMaster.bits.expect((BigInt(testDepKind1) << 58 | BigInt(testDepPointer1)).U(64.W))
            c.io.dest.expect(testDest.U)
            j += 1

            //println(s"(picosSubMaster.bits) = ${c.io.picosSubMaster.bits.peek().litValue.toString(16)}")

              c.io.picosSubMaster.ready.poke(true.B)
              c.clock.step()
              c.io.picosSubMaster.ready.poke(false.B)
              waitRandomAmountOfCycles(c)
          }

            waitRandomAmountOfCycles(c)
        }

        c.clock.step()
        c.clock.step()
        c.clock.step()
        checkSWIDPacket(c.io.picosSubMaster, testNanosID)
        c.io.picosSubMaster.ready.poke(true.B)
        c.io.last.expect(true.B)
          c.clock.step()
        c.io.picosSubMaster.ready.poke(false.B)
      }
    }
  }
}
