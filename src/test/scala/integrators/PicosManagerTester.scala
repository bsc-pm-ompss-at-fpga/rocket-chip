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
	r.setSeed(1000)

  val toHexString = (num : BigInt) => {
    f"${num}%X"
  }

  val placeTaskInfo = (c: DecoupledIO[TaskInfo], numDeps: Int, nanosID: Long) => {
      c.valid.poke(true.B)
      c.bits.num_deps.poke(numDeps.U)
      c.bits.task_id.poke(nanosID.U)
  }

  val unplaceTaskInfo = (c: DecoupledIO[TaskInfo]) => {
      c.valid.poke(false.B)
      c.bits.num_deps.poke(0.U)
      c.bits.task_id.poke(0.U)
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

  val checkPacket0 = (c: DecoupledIO[UInt], numDeps: Int, taskNumber: Long) => {
    c.valid.expect(true.B)
    val picosSubPacket = c.bits.peek().litValue
    //println(toHexString(picosSubPacket))
    assert(((picosSubPacket >> 16) & 0xf) == numDeps)
    assert(((picosSubPacket >> 32) & 0xffffffffL) == taskNumber)
  }

  // TODO: Implement once we start supporting nesting
  val checkPacket1 = (c: DecoupledIO[UInt], parentID: Long) => {
    c.valid.expect(true.B)
  }

  val checkPacket2 = (c: DecoupledIO[UInt], nanosID: Long) => {
    val FPGA_MASK = 0x1
    val RUN_ON_ANY_ACCELERATOR_MASK = 0xff

    c.valid.expect(true.B)
    val picosSubPacket = c.bits.peek().litValue
    //println(toHexString(picosSubPacket))
    assert(((picosSubPacket >> 32) & 0x3) == FPGA_MASK)
    assert(((picosSubPacket >> 40) & 0xff) == RUN_ON_ANY_ACCELERATOR_MASK)
    assert((picosSubPacket & 0xffffffffL) == nanosID)
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
    c.io.routingInfoOuter.bits.numPackets.expect((3 + numDeps + numArgs).U)
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
        assert(input.size == 4)
        var readyTask = new ReadyTaskInfo
        readyTask.numArgs  = input(0) & 0xff00
        readyTask.picosID  = input(1) & 0xffffffffL
        readyTask.parentID = input(2)
        readyTask.nanosID  = input(3) & 0xffffffffL

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
        interpretedInput.tID = testedTID

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

      val testedCoreID = (numTestedCores + 2) % numTestedCores 
      
      c.io.coreID.poke(testedCoreID.U)

      for (i <- 0 until 100) {
        //println("Running iteration " + i)
        val testedTaskNumber = i + 1
        val testedNumDeps = r.nextInt(16)
        val testedParentID = r.nextLong() & 0xffffffffL
        val testedNanosID = r.nextLong() & 0xffffffffL
        val testedNumArgs = 0

        placeTaskInfo(c.io.taskInfo, testedNumDeps, testedNanosID)
        c.io.picosSubData.valid.expect(false.B)

          waitRandomAmountOfCycles(c)

        checkHeaderInnerRoutingPacket()
        checkOuterRoutingPacket(testedCoreID, testedNumDeps, testedNumArgs)

        c.io.routingInfo.ready.poke(true.B)

          waitRandomAmountOfCycles(c)

        c.io.picosSubData.valid.expect(false.B)

          waitRandomAmountOfCycles(c)

        c.io.routingInfoOuter.ready.poke(true.B)
        c.io.picosSubData.valid.expect(true.B)
        c.io.picosSubData.ready.poke(true.B)

        checkPacket0(c.io.picosSubData, testedNumDeps, testedTaskNumber)

          c.clock.step()

        c.io.routingInfo.ready.poke(false.B)
        c.io.routingInfoOuter.ready.poke(false.B)
        c.io.picosSubData.ready.poke(false.B)

          waitRandomAmountOfCycles(c)

        c.io.routingInfo.ready.poke(true.B)
        c.io.routingInfoOuter.ready.poke(true.B)
        c.io.picosSubData.ready.poke(true.B)

        checkPacket1(c.io.picosSubData, testedParentID)
        checkPayloadInnerRoutingPacket(testedNumDeps, testedNumArgs)

          c.clock.step()

        c.io.routingInfo.ready.poke(false.B)
        c.io.routingInfoOuter.ready.poke(false.B)
        c.io.picosSubData.ready.poke(false.B)

          waitRandomAmountOfCycles(c)

        c.io.routingInfo.ready.poke(true.B)
        c.io.routingInfoOuter.ready.poke(true.B)
        c.io.picosSubData.ready.poke(true.B)

        checkPacket2(c.io.picosSubData, testedNanosID)

          c.clock.step()

        unplaceTaskInfo(c.io.taskInfo)
        c.io.picosSubData.ready.poke(false.B)
        c.io.routingInfo.ready.poke(false.B)
        c.io.routingInfoOuter.ready.poke(false.B)

          waitRandomAmountOfCycles(c)
      }
    }
  }
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

          for (i <- 0 until 100) {
            val testNumArgs = 0
            val testTaskID = 1332341342L
            val testTaskNumber = i + 1

            val testNumDeps = r.nextInt(16)
            val testParentID = r.nextLong() & 0xffffffffL
            val testNanosID = r.nextLong() & 0xffffffffL

            val INVALID_TYPE = 0
            val IN_TYPE = 1
            val OUT_TYPE = 2
            val INOUT_TYPE = 3

            //println(f"Iteration: $i")

            c.io.coreID.poke(testCoreID.U)
            c.io.routingInfoOuter.valid.expect(false.B)

            c.io.routingInfoOuter.ready.poke(true.B)
            c.io.picosSubData.ready.poke(true.B)

            placeTaskInfo(c.io.taskInfo, testNumDeps, testNanosID)
              c.clock.step()
            unplaceTaskInfo(c.io.taskInfo)
            c.io.routingInfoOuter.valid.expect(true.B)
            
            checkOuterRoutingPacket(c, testCoreID, testNumDeps, testNumArgs)

            // The leaf GuidedArbiter has not processed the routing packet
            // that allows submission header packets to go through, so
            // the following interface should not hold any valid data
            c.io.picosSubData.valid.expect(false.B)

              c.clock.step()
              c.clock.step()

            checkPacket0(c.io.picosSubData, testNumDeps, testTaskNumber)

              c.clock.step()

            // TODO: For the following to work, we'll need to allow
            //       CoreSpecificSubmissionHandler to buffer at least
            //       one routing packet for the leaf GuidedArbiter
            checkPacket1(c.io.picosSubData, testParentID)

              c.clock.step()

            checkPacket2(c.io.picosSubData, testNanosID)

              c.clock.step()

              // One extra step is needed because the routing packet
              // for the payload section takes one cycle to be fetched
              c.clock.step()

              waitRandomAmountOfCycles(c)

            var j = 0
            var tries = 0
            var max_tries = 100
            while (j < testNumDeps) {
              val testDepPointer0 = r.nextLong() & 0xffffffffffffffL
              val testDepKind0 = r.nextInt(3) + 1
              val testDepPointer1 = r.nextLong() & 0xffffffffffffffL
              val testDepKind1 = if (j + 1 < testNumDeps) r.nextInt(4) else 0

              //println("Dep #" + j)
              j += 1

              placeDepInfo(c.io.depInfo, testDepPointer0, testDepKind0, testDepPointer1, testDepKind1)
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

              //c.io.picosSubData.valid.expect(true.B)
              c.io.picosSubData.bits.expect((BigInt(testDepKind0) << 58 | BigInt(testDepPointer0)).U(64.W))

                c.clock.step()

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
                c.io.picosSubData.bits.expect((BigInt(testDepKind1) << 58 | BigInt(testDepPointer1)).U(64.W))

                  c.clock.step()
              }


                waitRandomAmountOfCycles(c)
            }
            unplaceDepInfo(c.io.depInfo)
            c.io.routingInfoOuter.ready.poke(false.B)
            c.io.picosSubData.ready.poke(false.B)

              waitRandomAmountOfCycles(c)
          }
      }
  }

	behavior of "SubmissionHandler"
  it should "Process submission sequences coming from different cores in an ack-dependent way" in {
    test(new SubmissionHandler(numTestedCores)) { c =>
      var firstSubmission = true

      c.io.enabled.poke(true.B)

      for (i <- 0 until 100) {
        val testNumArgs = 0
        val testTaskID = 1332341342L
        val testTaskNumber = i + 1

        val testNumDeps = r.nextInt(16)
        val testCoreID = r.nextInt(numTestedCores)
        val testParentID = r.nextLong() & 0xffffffffL
        val testNanosID = r.nextLong() & 0xffffffffL
        val testDest = if (testNumDeps == 0) 3 else 2

        val INVALID_TYPE = 0
        val IN_TYPE = 1
        val OUT_TYPE = 2
        val INOUT_TYPE = 3

        //println(s"Test #$i")

        c.io.picosSubMaster.valid.expect(false.B)
        placeTaskInfo(c.io.taskInfoVec(testCoreID), testNumDeps, testNanosID)
          c.clock.step()
        unplaceTaskInfo(c.io.taskInfoVec(testCoreID))
          c.clock.step()
          //println(s"(picosSubMaster.bits) = ${c.io.picosSubMaster.bits.peek().litValue.toString(16)}")


        if (firstSubmission) {
          // The system has not completed any submission sequence yet,
          // so there is no need to wait for an acknowledgement
            c.clock.step()
          c.io.picosSubMaster.valid.expect(true.B)
          firstSubmission = false
        } else {
          waitRandomAmountOfCycles(c)
          c.io.picosSubMaster.valid.expect(false.B)
          setAckSignal(c.io.picosAckInterface)
            c.clock.step()
          unsetAckSignal(c.io.picosAckInterface)
            c.clock.step()
        }

          waitRandomAmountOfCycles(c)
        //println("Just crossed firstSubmission zone")

        checkPacket0(c.io.picosSubMaster, testNumDeps, testTaskNumber)
        c.io.dest.expect(testDest.U)
        c.io.picosSubMaster.ready.poke(true.B)

          c.clock.step()
          //println(s"(picosSubMaster.bits) = ${c.io.picosSubMaster.bits.peek().litValue.toString(16)}")

        checkPacket1(c.io.picosSubMaster, testParentID)
        c.io.dest.expect(testDest.U)

          c.clock.step()
          c.io.picosSubMaster.ready.poke(false.B)
          waitRandomAmountOfCycles(c)
          c.io.picosSubMaster.ready.poke(true.B)
          //println(s"(picosSubMaster.bits) = ${c.io.picosSubMaster.bits.peek().litValue.toString(16)}")

        checkPacket2(c.io.picosSubMaster, testNanosID)
        c.io.dest.expect(testDest.U)

        var j = 0

        if (j == testNumDeps) {
          c.io.last.expect(true.B)
          //println("This was the last dep!")
        } else {
          c.io.last.expect(false.B)
        }

          c.clock.step()
          c.io.picosSubMaster.ready.poke(false.B)
          waitRandomAmountOfCycles(c)
          c.io.picosSubMaster.ready.poke(true.B)
          //println(s"(picosSubMaster.bits) = ${c.io.picosSubMaster.bits.peek().litValue.toString(16)}")

          c.clock.step()
          c.io.picosSubMaster.ready.poke(false.B)
          waitRandomAmountOfCycles(c)
          c.io.picosSubMaster.ready.poke(true.B)

        while (j < testNumDeps) {
          val testDepPointer0 = r.nextLong() & 0xffffffffffffffL
          val testDepKind0 = r.nextInt(3) + 1
          val testDepPointer1 = r.nextLong() & 0xffffffffffffffL
          val testDepKind1 = if (j + 1 < testNumDeps) r.nextInt(4) else 0

          //println("Dep #" + j)

          placeDepInfo(c.io.depInfoVec(testCoreID), testDepPointer0, testDepKind0, testDepPointer1, testDepKind1)
            c.clock.step()
          unplaceDepInfo(c.io.depInfoVec(testCoreID))

          c.io.picosSubMaster.valid.expect(true.B)
          c.io.picosSubMaster.bits.expect((BigInt(testDepKind0) << 58 | BigInt(testDepPointer0)).U(64.W))
          c.io.dest.expect(testDest.U)
          j += 1

          if (j == testNumDeps) {
            c.io.last.expect(true.B)
            //println("This was the last dep!")
          } else {
            c.io.last.expect(false.B)
          }

            c.clock.step()
          c.io.picosSubMaster.ready.poke(false.B)
            waitRandomAmountOfCycles(c)
          c.io.picosSubMaster.ready.poke(true.B)

          if (testDepKind1 != 0) {
            //println("Dep #" + j)
            c.io.picosSubMaster.valid.expect(true.B)
            c.io.picosSubMaster.bits.expect((BigInt(testDepKind1) << 58 | BigInt(testDepPointer1)).U(64.W))
            c.io.dest.expect(testDest.U)
            j += 1

            if (j == testNumDeps) {
              c.io.last.expect(true.B)
              //println("This was the last dep!")
            } else {
              c.io.last.expect(false.B)
            }
            //println(s"(picosSubMaster.bits) = ${c.io.picosSubMaster.bits.peek().litValue.toString(16)}")

              c.clock.step()
              c.io.picosSubMaster.ready.poke(false.B)
              waitRandomAmountOfCycles(c)
              c.io.picosSubMaster.ready.poke(true.B)
          }

            waitRandomAmountOfCycles(c)
        }
        unplaceDepInfo(c.io.depInfoVec(testCoreID))
        c.io.picosSubMaster.ready.poke(false.B)
      }
    }
  }
}
