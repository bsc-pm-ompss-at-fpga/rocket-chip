// See LICENSE.Berkeley for license details.

package freechips.rocketchip.util

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.integrators.{PicosFifoBundle}

/** A generalized locking RR arbiter that addresses the limitations of the
 *  version in the Chisel standard library */
abstract class HellaLockingArbiter[T <: Data](typ: T, arbN: Int, rr: Boolean = false)
    extends Module {

  val io = new Bundle {
    val in = Vec(arbN, Decoupled(typ.cloneType)).flip
    val out = Decoupled(typ.cloneType)
  }

  def rotateLeft[T <: Data](norm: Vec[T], rot: UInt): Vec[T] = {
    val n = norm.size
    Vec.tabulate(n) { i =>
      Mux(rot < UInt(n - i), norm(UInt(i) + rot), norm(rot - UInt(n - i)))
    }
  }

  val lockIdx = Reg(init = UInt(0, log2Up(arbN)))
  val locked = Reg(init = Bool(false))

  val choice = if (rr) {
    PriorityMux(
      rotateLeft(Vec(io.in.map(_.valid)), lockIdx + UInt(1)),
      rotateLeft(Vec((0 until arbN).map(UInt(_))), lockIdx + UInt(1)))
  } else {
    PriorityEncoder(io.in.map(_.valid))
  }

  val chosen = Mux(locked, lockIdx, choice)

  for (i <- 0 until arbN) {
    io.in(i).ready := io.out.ready && chosen === UInt(i)
  }

  io.out.valid := io.in(chosen).valid
  io.out.bits := io.in(chosen).bits
}

/** This locking arbiter determines when it is safe to unlock
 *  by peeking at the data */
class HellaPeekingArbiter[T <: Data](
      typ: T, arbN: Int,
      canUnlock: T => Bool,
      needsLock: Option[T => Bool] = None,
      rr: Boolean = false)
    extends HellaLockingArbiter(typ, arbN, rr) {

  def realNeedsLock(data: T): Bool =
    needsLock.map(_(data)).getOrElse(Bool(true))

  when (io.out.fire()) {
    when (!locked && realNeedsLock(io.out.bits)) {
      lockIdx := choice
      locked := Bool(true)
    }
    // the unlock statement takes precedent
    when (canUnlock(io.out.bits)) {
      locked := Bool(false)
    }
  }
}

/** This arbiter determines when it is safe to unlock by counting transactions */
class HellaCountingArbiter[T <: Data](
      typ: T, arbN: Int, count: Int,
      val needsLock: Option[T => Bool] = None,
      rr: Boolean = false)
    extends HellaLockingArbiter(typ, arbN, rr) {

  def realNeedsLock(data: T): Bool =
    needsLock.map(_(data)).getOrElse(Bool(true))

  // if count is 1, you should use a non-locking arbiter
  require(count > 1, "CountingArbiter cannot have count <= 1")

  val lock_ctr = Counter(count)

  when (io.out.fire()) {
    when (!locked && realNeedsLock(io.out.bits)) {
      lockIdx := choice
      locked := Bool(true)
      lock_ctr.inc()
    }

    when (locked) {
      when (lock_ctr.inc()) { locked := Bool(false) }
    }
  }
}

/** This arbiter preserves the order of responses */
class InOrderArbiter[T <: Data, U <: Data](reqTyp: T, respTyp: U, n: Int)
    (implicit p: Parameters) extends Module {
  val io = new Bundle {
    val in_req = Vec(n, Decoupled(reqTyp)).flip
    val in_resp = Vec(n, Decoupled(respTyp))
    val out_req = Decoupled(reqTyp)
    val out_resp = Decoupled(respTyp).flip
  }

  if (n > 1) {
    val route_q = Module(new Queue(UInt(width = log2Up(n)), 2))
    val req_arb = Module(new RRArbiter(reqTyp, n))
    req_arb.io.in <> io.in_req

    val req_helper = DecoupledHelper(
      req_arb.io.out.valid,
      route_q.io.enq.ready,
      io.out_req.ready)

    io.out_req.bits := req_arb.io.out.bits
    io.out_req.valid := req_helper.fire(io.out_req.ready)

    route_q.io.enq.bits := req_arb.io.chosen
    route_q.io.enq.valid := req_helper.fire(route_q.io.enq.ready)

    req_arb.io.out.ready := req_helper.fire(req_arb.io.out.valid)

    val resp_sel = route_q.io.deq.bits
    val resp_ready = io.in_resp(resp_sel).ready
    val resp_helper = DecoupledHelper(
      resp_ready,
      route_q.io.deq.valid,
      io.out_resp.valid)

    val resp_valid = resp_helper.fire(resp_ready)
    for (i <- 0 until n) {
      io.in_resp(i).bits := io.out_resp.bits
      io.in_resp(i).valid := resp_valid && resp_sel === UInt(i)
    }

    route_q.io.deq.ready := resp_helper.fire(route_q.io.deq.valid)
    io.out_resp.ready := resp_helper.fire(io.out_resp.valid)
  } else {
    io.out_req <> io.in_req.head
    io.in_resp.head <> io.out_resp
  }
}

class RoutingInfo(arbN: Int, maxBurstLength: Int) extends Bundle {
  val arbID = UInt(log2Ceil(arbN).W)
  val numPackets = UInt(log2Ceil(maxBurstLength + 1).W)

  override def cloneType: this.type = new RoutingInfo(arbN, maxBurstLength).asInstanceOf[this.type]
}

object RoutingInfo {
  def apply(arbN: Int, maxBurstLength: Int): RoutingInfo = new RoutingInfo(arbN, maxBurstLength)
}

class GuidedArbiter[T <: Data](typ: T, arbN: Int, maxBurstLength: Int, numDownCycles: Int) extends Module {
  val io = IO(new Bundle {
    val in = Vec(arbN, Decoupled(typ.cloneType)).flip
    val out = Decoupled(typ.cloneType)
    val routingInfo = Decoupled(RoutingInfo(arbN, maxBurstLength)).flip
    val enabled = Bool().asInput
    val last = Bool().asOutput
  })

  val chosen = RegEnable(io.routingInfo.bits.arbID, io.routingInfo.fire())
  val burstCounter = RegInit(0.U(log2Ceil(maxBurstLength).W))
  val remainingDownCycles = RegInit(0.U(10.W))

  io.out.valid := (burstCounter > 0.U) && io.in(chosen).valid
  io.out.bits := io.in(chosen).bits

  io.last := burstCounter === 1.U
  when (io.out.fire()) {
    when (burstCounter === 1.U) {
      remainingDownCycles := numDownCycles.U
    }
    burstCounter := burstCounter - 1.U
  }

  when (io.routingInfo.fire()) {
    burstCounter := io.routingInfo.bits.numPackets
  }

  when (remainingDownCycles > 0.U) {
      remainingDownCycles := remainingDownCycles - 1.U
  }

  io.routingInfo.ready := (burstCounter === 0.U) && (remainingDownCycles === 0.U) && io.enabled

  for (i <- 0 until arbN) {
    io.in(i).ready := io.out.ready && chosen === i.U && burstCounter > 0.U
  }
}

class DelayerAdapter[T <: Data](typ: T) extends Module {
  val io = IO(new Bundle {
    val in = Decoupled(typ.cloneType).flip
    val out = Decoupled(typ.cloneType)
  })

  val counter = RegInit(t = UInt(1.W), init = 0.U)
  val numCyclesSinceReady = RegInit(t = UInt(1.W), init = 0.U)
  io.out.bits := io.in.bits

  // Counter == 0: Def: waiting for a new transaction to happen
  //               R1: hides firing from producer
  //               R2: lets the consumer acknowledge the firing
  // Counter == 1: Def: keeping data steady for one cycle more
  //               after a transaction happened
  //               R1: lets the producer acknowledge the firing
  //               R2: makes the consumer see a low valid signal
  
  // Writable signals
  // in.ready
  // out.valid
  // out.bits
  
  when (!io.out.ready) {
    numCyclesSinceReady := 0.U 
  } .otherwise {
    numCyclesSinceReady := 1.U 
  }

  when (counter === 1.U) {
    io.in.ready := true.B
    io.out.valid := false.B

    counter := 0.U
  } .otherwise {
    when (numCyclesSinceReady === 1.U) {
      io.in.ready := false.B
      io.out.valid := io.in.valid && io.out.ready

      when (io.out.fire()) {
        counter := 1.U
      }
    } .otherwise {
      io.in.ready := false.B
      io.out.valid := false.B
    }
  }
}


class TaggedDecoupledIO[T <: Data](typ: T, maxNumElements: Int) extends Bundle {
  val tag = UInt(log2Up(maxNumElements).W).asOutput
  val ready = Bool().asInput
  val valid = Bool().asOutput
  val bits  = typ.chiselCloneType.asOutput

  override def cloneType: this.type = new TaggedDecoupledIO(typ, maxNumElements).asInstanceOf[this.type]
}

object TaggedDecoupledIO {
  def apply[T <: Data](typ: T, maxNumElements: Int): TaggedDecoupledIO[T] = new TaggedDecoupledIO(typ, maxNumElements)
}

class RevTaggedDecoupledIO[T <: Data](typ: T, maxNumElements: Int) extends Bundle {
  val tag = UInt(log2Up(maxNumElements).W).asInput
  val ready = Bool().asInput
  val valid = Bool().asOutput
  val bits  = typ.chiselCloneType.asOutput

  override def cloneType: this.type = new RevTaggedDecoupledIO(typ, maxNumElements).asInstanceOf[this.type]
}

object RevTaggedDecoupledIO {
  def apply[T <: Data](typ: T, maxNumElements: Int): RevTaggedDecoupledIO[T] = new RevTaggedDecoupledIO(typ, maxNumElements)
}

class TDIOMuxCP[T <: Data](typ: T, n: Int) extends Module {
  val io = new Bundle {
    // These "in" signals feed this module with tags and readies, expecting bits and valids back
    val in = Vec(n, RevTaggedDecoupledIO(typ.cloneType, n))

    // These are the consumer signals
    val out = Vec(n, Decoupled(typ.cloneType)).flip

    // Mon Jan 28 17:04:36 -02 2019: TODO: CONTINUE
  }

  val tagWidth = log2Up(n)
  /*
   * Things to verify:
   * - For each producer
   *   - Whether its tag matched anyone
   * - For each consumer
   *   - Whether it was matched by any producer
   */

  for (i <- 0 until n) {
    val consumer = io.in(i)

    var matchesByTheConsumer = new Array[Bool](n)

    for (j <- 0 until n) {
      val producer = io.out(j)

      val tagMatch = consumer.tag === j.asUInt(tagWidth.W)

      when(tagMatch) {
        consumer.bits := producer.bits
        consumer.valid := producer.valid
        when (consumer.ready) {
          producer.ready := true.B
        }
      }

      matchesByTheConsumer(j) = tagMatch
    }

    val consumerMatchedSomeone = matchesByTheConsumer.reduce(_ || _)
    when (!consumerMatchedSomeone) {
      consumer.valid := false.B
    }
  }

  for (i <- 0 until n) {
    val producer = io.out(i)
    var matchesToTheProducer = new Array[Bool](n)

    for (j <- 0 until n) {
      val consumer = io.in(j)
      val tagMatch = (consumer.tag === i.asUInt(tagWidth.W)) && consumer.ready
      matchesToTheProducer(j) = tagMatch
    }

    val producerWasMatchedBySomeone = matchesToTheProducer.reduce(_ || _)
    when (!producerWasMatchedBySomeone) {
      producer.ready := false.B
    }
  }
}

class TDIOMuxPC[T <: Data](typ: T, n: Int) extends Module {
  val io = new Bundle {
    // These "in" signals feed this module with tags, bits and valids, expecting readies back.
    val in = Vec(n, TaggedDecoupledIO(typ.cloneType, n)).flip

    // These are the producer signals
    val out = Vec(n, Decoupled(typ.cloneType))
  }

  val tagWidth = log2Up(n)
  /*
   * Things to verify:
   * - For each producer
   *   - Whether its tag matched anyone
   * - For each consumer
   *   - Whether it was matched by any producer
   */

  for (i <- 0 until n) {
    val producer = io.in(i)

    var matchesByTheProducer = new Array[Bool](n)

    for (j <- 0 until n) {
      val consumer = io.out(j)

      val tagMatch = producer.tag === j.asUInt(tagWidth.W)

      when(tagMatch) {
        consumer.bits := producer.bits
        producer.ready := consumer.ready
        when(producer.valid) {
          consumer.valid := true.B
        }
      }

      matchesByTheProducer(j) = tagMatch
    }

    val producerMatchedSomeone = matchesByTheProducer.reduce(_ || _)
    when (!producerMatchedSomeone) {
      producer.ready := false.B
    }
  }

  for (i <- 0 until n) {
    val consumer = io.out(i)
    var matchesToTheConsumer = new Array[Bool](n)

    for (j <- 0 until n) {
      val producer = io.in(j)
      val tagMatch = (producer.tag === i.asUInt(tagWidth.W)) && producer.valid
      matchesToTheConsumer(j) = tagMatch
    }

    val consumerWasMatchedBySomeone = matchesToTheConsumer.reduce(_ || _)
    when (!consumerWasMatchedBySomeone) {
      consumer.valid := false.B
    }
  }
}

class ZeroPadder[T <: Data](typ: T, burstSizeWithZeros: Int) extends Module {
  val io = IO(new Bundle {
    val in = Decoupled(typ.cloneType).flip
    val out = Decoupled(typ.cloneType)
    val startPadding = Bool().asInput
  })

  val counter = RegInit(UInt(burstSizeWithZeros - 1))
  val zeroLocked = RegEnable(true.B, false.B, io.startPadding)
  val sendZero = Wire(Bool())
  sendZero := zeroLocked || io.startPadding

  when (sendZero) {
    io.in.ready := false.B
    io.out.valid := true.B
    io.out.bits := 0.U
  } .otherwise {
    io.out <> io.in
  }

  when (io.out.fire()) {
    when (counter > 0.U) {
      counter := counter - 1.U
    } .otherwise {
      counter := UInt(burstSizeWithZeros - 1)
      zeroLocked := false.B
    }
  }
}

class ArbitrarySuffixer[T <: Data](typ: T, burstSizeWithSuffix: Int, suffixValue: Int) extends Module {
  val io = IO(new Bundle {
    val in = Decoupled(typ.cloneType).flip
    val out = Decoupled(typ.cloneType)
    val suffixing = Output(Wire(Bool()))
  })

  val counter = RegInit(UInt(burstSizeWithSuffix - 1))
  val suffixNow = counter === 0.U
  io.suffixing := suffixNow

  when (suffixNow) {
    io.in.ready := false.B
    io.out.valid := true.B
    io.out.bits := suffixValue.U
  } .otherwise {
    io.out <> io.in
  }

  when (io.out.fire()) {
    when (counter > 0.U) {
      counter := counter - 1.U
    } .otherwise {
      counter := UInt(burstSizeWithSuffix - 1)
    }
  }
}

class ArbitraryPrefixer[T <: Data](typ: T, burstSizeWithPrefix: Int, prefixValue: Int) extends Module {
  val io = IO(new Bundle {
    val in = Decoupled(typ.cloneType).flip
    val out = Decoupled(typ.cloneType)
    val enablePrefixing = Input(Wire(Bool()))
    val prefixing = Output(Wire(Bool()))
  })

  val counter = RegInit(0.U(log2Ceil(burstSizeWithPrefix).W))

  val prefixNow = counter === 0.U
  val wrapCounter = counter === (burstSizeWithPrefix - 1).U
  io.prefixing := prefixNow

  when (prefixNow) {
    io.in.ready := false.B
    io.out.valid := io.enablePrefixing
    io.out.bits := prefixValue.U
  } .otherwise {
    io.out <> io.in
  }

  when (io.out.fire()) {
    when (wrapCounter) {
      counter := 0.U
    } .otherwise {
      counter := counter + 1.U
    }
  }
}

class ArbitraryPrefixerAndSuffixer[T <: Data](typ: T, burstSizeWithPrefixAndSuffix: Int, prefixValue: Int, suffixValue: Int) extends Module {
  val io = IO(new Bundle {
    val in = Decoupled(typ.cloneType).flip
    val out = Decoupled(typ.cloneType)
    val enablePrefixing = Input(Wire(Bool()))
    val suffixing = Output(Wire(Bool()))
    val prefixing = Output(Wire(Bool()))
  })

  val prefixer = Module(new ArbitraryPrefixer(typ, burstSizeWithPrefixAndSuffix - 1, prefixValue))
  val suffixer = Module(new ArbitrarySuffixer(typ, burstSizeWithPrefixAndSuffix, suffixValue))

  prefixer.io.enablePrefixing := io.enablePrefixing
  prefixer.io.in <> io.in
  suffixer.io.in <> prefixer.io.out
  io.out <> suffixer.io.out
  io.suffixing := suffixer.io.suffixing
  io.prefixing := prefixer.io.prefixing
}

class RRArbiterWithPrefixAndSuffix[T <: Data](typ: T, numProducers: Int, burstSizeWithPrefixAndSuffix: Int, prefixValue: Int, suffixValue: Int) extends Module {
  val io = IO(new Bundle {
    val in = Vec(numProducers, Decoupled(typ.cloneType)).flip
    val out = Decoupled(typ.cloneType)
    val last = Output(Wire(Bool()))
    val enablePrefixing = Input(Wire(Bool()))
    val suffixing = Output(Wire(Bool()))
    val prefixing = Output(Wire(Bool()))
    val triggerContent = Output(typ.cloneType)
    val arbiterMonitor = Decoupled(typ.cloneType)
  })

  val roundRobinArbiter = Module(new RRArbiter(typ, numProducers))
  val arbitraryPrefixerAndSuffixer = Module(new ArbitraryPrefixerAndSuffixer(typ, burstSizeWithPrefixAndSuffix, prefixValue, suffixValue))

  val suffixing = arbitraryPrefixerAndSuffixer.io.suffixing
  val prefixing = arbitraryPrefixerAndSuffixer.io.prefixing

  io.suffixing := suffixing
  io.prefixing := prefixing

  val updateTriggerContent = io.out.fire() && prefixing && !suffixing
  val triggerContentReg = RegEnable(roundRobinArbiter.io.out.bits, updateTriggerContent)

  roundRobinArbiter.io.in <> io.in
  arbitraryPrefixerAndSuffixer.io.in <> roundRobinArbiter.io.out
  arbitraryPrefixerAndSuffixer.io.enablePrefixing := roundRobinArbiter.io.out.valid && io.enablePrefixing
  io.out <> arbitraryPrefixerAndSuffixer.io.out
  io.arbiterMonitor.bits := roundRobinArbiter.io.out.bits
  io.arbiterMonitor.valid := roundRobinArbiter.io.out.valid
  io.last := suffixing

  // The following condition ensures `io.triggerContent` only acts as a passthrough
  // for `roundRobinArbiter.io.out.bits` when this module is actually outputting
  // the prefix
  when (prefixing && !suffixing && !arbitraryPrefixerAndSuffixer.io.in.ready) {
    io.triggerContent := roundRobinArbiter.io.out.bits
  } .otherwise {
    io.triggerContent := triggerContentReg
  }
}

class GenerateAndSubmitPacketSequenceDependingOnAcknowledgement[T <: Data](typ: T, numProducers: Int, burstSizeWithPrefixAndSuffix: Int, prefixValue: Int, suffixValue: Int) extends Module {
  val io = IO(new Bundle {
    val in = Vec(numProducers, Decoupled(typ.cloneType)).flip
    val out = Decoupled(typ.cloneType)
    val picosAckInterface = Flipped(new PicosFifoBundle(UInt(2.W)))
  })

  val arbiter = Module(new RRArbiterWithPrefixAndSuffix(typ, numProducers, burstSizeWithPrefixAndSuffix, prefixValue, suffixValue))

  val awaitingAcknowledgement = RegInit(false.B)

  arbiter.io.in <> io.in
  arbiter.io.enablePrefixing := !awaitingAcknowledgement
  io.out <> arbiter.io.out

  io.picosAckInterface.empty := awaitingAcknowledgement
  when (awaitingAcknowledgement) {
    awaitingAcknowledgement := !(io.picosAckInterface.valid && (io.picosAckInterface.data === 1.U(2.W)))
  } .otherwise {
    awaitingAcknowledgement := arbiter.io.last && arbiter.io.out.fire()
  }
}
