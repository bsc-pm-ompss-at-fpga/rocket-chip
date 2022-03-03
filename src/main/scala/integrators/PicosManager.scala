package freechips.rocketchip.integrators

import Chisel._
//import chisel3.iotesters._
//import org.scalatest._
import freechips.rocketchip.util.{InOrderArbiter, GuidedArbiter, RoutingInfo, ZeroPadder, DelayerAdapter, TDIOMuxPC}
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import chisel3.dontTouch

import picos_v4_integration_elements.{ArgsAndParentInfo, DepInfo, TaskInfo}
import freechips.rocketchip.tile.{BlankConfig}

class ReadyPacket extends Bundle {
				val nanosID                     = UInt(32.W)
				val picosID                     = UInt(32.W)
}

object ReadyPacket {
				def apply() = new ReadyPacket
}

trait HasDest extends Bundle{
				val dest                     		= UInt(3.W)
}

class PicosFifoBundle[T <: Data](typ: T = UInt(64.W)) extends Bundle {
				val data                        = typ.cloneType.asOutput
				val valid                       = Bool().asOutput
				val empty                       = Bool().asInput
				val last												= Bool().asOutput

				override def cloneType = new PicosFifoBundle(typ.cloneType).asInstanceOf[this.type]
}

class ReadyTaskIO(nCores: Int) extends Bundle {
				val readyRequestInterface   = Flipped(Vec(nCores, Decoupled(UInt(0.W))))
				val readyQInterface         = Vec(nCores, Decoupled(new ReadyPacket))
				val picosReadyInterface     = Flipped(new PicosFifoBundle(UInt(64.W)))
}

class RealManagerIO[T <: Data](nCores: Int, typ: T = UInt(64.W), burstSizeWithZeros: Int = 48)(implicit p: Parameters) extends Bundle {
        // TODO: Clear these deprecated signals
				// val tag                     = Flipped(Vec(nCores, UInt(64.W).asOutput))
				// val subQInterface           = Flipped(Vec(nCores, Decoupled(UInt(64.W))))
				// val subRequest              = Flipped(Vec(nCores, Decoupled(RoutingInfo(64, 48))))

				val retInterface            = Flipped(Vec(nCores, Decoupled(UInt(64.W))))
				val readyRequestInterface   = Flipped(Vec(nCores, Decoupled(UInt(0.W))))
				val readyQInterface         = Vec(nCores, Decoupled(new ReadyPacket))

				//TODO: Remove the dontTouch to make sure that these signals are being used
				val argsAndParentInfoVec      = dontTouch( Flipped(Vec(nCores, Decoupled(new ArgsAndParentInfo))) )
				val taskInfoVec            		= dontTouch( Flipped(Vec(nCores, Decoupled(new TaskInfo))) )
				val depInfoVec   							= dontTouch( Flipped(Vec(nCores, Decoupled(Vec(2, new DepInfo)))) )

				val picosFinishedInterface  = new PicosFifoBundle(typ.cloneType)
				val picosReadyInterface     = Flipped(new PicosFifoBundle(typ.cloneType))
				// TODO: Remove the dontTouch
				val picosSubInterface       = dontTouch(new PicosFifoBundle(typ.cloneType) with HasDest)
				val picosAckInterface       = Flipped(new PicosFifoBundle(UInt(2.W)))

				val pmException               = UInt(4.W).asOutput
}

class SubmissionHeaderGeneratorIO(nCores: Int) extends Bundle {
				val argsAndParentInfo      		= Flipped(Decoupled(new ArgsAndParentInfo))
				val taskInfo 		           		= Flipped(Decoupled(new TaskInfo))

				// This is used for feeding the 2-entry GuidedArbiter
				// that is instantiated for each core, which builds
				// complete submission sequences from the output of
				// SubmissionHeaderGenerator and the DependencyAdapter
				val routingInfo								= Decoupled((new RoutingInfo(2, 3 + 15 + 15)))

				// This is used for feeding the nCores-entry GuidedArbiter
				// that is fed by the core-specific GuidedArbiter building
				// submission sequences out of elementary data
				val routingInfoOuter					= Decoupled((new RoutingInfo(nCores, 3 + 15 + 15)))

				val picosSubData							= Decoupled(UInt(64.W))
				val coreID										= Input(UInt(log2Ceil(nCores).W))
}

class SubmissionHeaderGenerator(nCores: Int) extends Module {
				val io = IO(new SubmissionHeaderGeneratorIO(nCores))
				val cycleLength = 3

				val counter = RegInit(0.U(log2Ceil(cycleLength).W))
				// TODO: Check whether this should be initialized to 0 or 1
				val taskNumberCounter = RegInit(1.U(32.W))

				val wrapCounter = counter === (cycleLength - 1).U
				val headerInnerRoutingPacketWasSent = RegInit(false.B)
				val payloadInnerRoutingPacketWasSent = RegInit(false.B)
				val outerRoutingPacketWasSent = RegInit(false.B)

				io.argsAndParentInfo.ready := false.B
				io.taskInfo.ready := false.B
				io.routingInfo.valid := false.B
				io.picosSubData.valid := false.B

				val numDeps = io.taskInfo.bits.num_deps
				val numArgs = 0.U(4.W)
				val nanosID = io.taskInfo.bits.task_id
				val sixZeros = 0.U(6.W)
				val twelveZeros = 0.U(12.W)
				val sixteenZeros = 0.U(16.W)

				// TODO: This will need to be modified when we start supporting nested tasks
				switch (counter) {
					is (0.U) {
						// TODO: We still need to produce the routing packets for the dependence sequence
						io.routingInfo.valid := io.taskInfo.valid && !headerInnerRoutingPacketWasSent
						io.routingInfo.bits.arbID := 0.U
						io.routingInfo.bits.numPackets := 3.U

						io.routingInfoOuter.valid := io.taskInfo.valid && !outerRoutingPacketWasSent
						io.routingInfoOuter.bits.arbID := io.coreID
						// The following operation must be carried out with at least 6-bit precision
						// because we need to be able to describe up to 33 packets per sequence
						io.routingInfoOuter.bits.numPackets := 3.U(6.W) + numDeps + numArgs

						io.picosSubData.bits := Cat(Seq(taskNumberCounter, twelveZeros, numDeps, sixteenZeros))

						// The following ensures we do not leave this state before
						// the header inner routing packet is sent to the leaf
						// core-specific submission GuidedArbiter and the outer
						// routing packet is sent to the master submission GuidedArbiter
						io.picosSubData.valid := io.taskInfo.valid && (io.routingInfo.fire() || headerInnerRoutingPacketWasSent) && (io.routingInfoOuter.fire() || outerRoutingPacketWasSent)

						when (io.routingInfo.fire()) {
							headerInnerRoutingPacketWasSent := true.B
						}

						when (io.routingInfoOuter.fire()) {
							outerRoutingPacketWasSent := true.B
						}
					}
					is (1.U) {
						io.picosSubData.bits := 0.U

						io.routingInfo.valid := io.taskInfo.valid && !payloadInnerRoutingPacketWasSent
						io.routingInfo.bits.arbID := 1.U
						// TODO: Check if this is actually synthesized with 6-bit precision
						io.routingInfo.bits.numPackets := 0.U(6.W) + numDeps + numArgs

						// The following ensures we do not leave this state before
						// the payload inner routing packet is sent to the leaf
						// core-specific submission GuidedArbiter
						io.picosSubData.valid := io.routingInfo.fire() || payloadInnerRoutingPacketWasSent

						when (io.routingInfo.fire()) {
							payloadInnerRoutingPacketWasSent := true.B
						}
					}
					is (2.U) {
						io.picosSubData.bits := Cat(Seq(sixteenZeros, "hff".U(8.W), sixZeros, 1.U(2.W), nanosID))
						io.picosSubData.valid := io.taskInfo.valid

						// The following will ensure data at taskInfo is consumed 
						// when an out transaction is performed.
						// We here assume that valid inputs will never change
						// since they are first asserted and the transaction
						// is finally completed.
						io.taskInfo.ready := io.picosSubData.ready
					}
				}

				when (io.picosSubData.fire()) {
					when (wrapCounter) {
						counter := 0.U
						headerInnerRoutingPacketWasSent := false.B
						payloadInnerRoutingPacketWasSent := false.B
						outerRoutingPacketWasSent := false.B
						taskNumberCounter := taskNumberCounter + 1.U
					} .otherwise {
						counter := counter + 1.U
					}
				}
}

class SubmissionHandler[T <: Data](nCores: Int, typ: T, burstSizeWithZeros: Int = 48, routingQDepth: Int = 48, numDownCycles: Int = 200) extends Module {
				val io = IO(new Bundle {
								val tag                   = Flipped(Vec(nCores, UInt(64.W).asOutput))
								val subQInterface         = Flipped(Vec(nCores, Decoupled(typ.cloneType)))
								val subRequest            = Flipped(Vec(nCores, Decoupled(RoutingInfo(nCores, burstSizeWithZeros))))
								val picosSubMaster        = Decoupled(typ.cloneType)
								val numStartedSubmissions = UInt(64.W).asOutput
								val enabled               = Bool().asInput
				})

				val taggedMuxPC = Module(new TDIOMuxPC(typ.cloneType, nCores))
				val routingRequestArbiter = Module(new RRArbiter(RoutingInfo(nCores, burstSizeWithZeros), nCores))
				val guidedArbiter = Module(new GuidedArbiter(typ.cloneType, nCores, burstSizeWithZeros, numDownCycles))
				guidedArbiter.io.enabled := io.enabled

				val routingQ = Module(new Queue(RoutingInfo(nCores, burstSizeWithZeros), routingQDepth))
				dontTouch(routingQ.io.count)

				val routingQEnqCounter = dontTouch(RegInit(0.U(64.W)))
				routingQEnqCounter := Mux(routingQ.io.enq.fire(), routingQEnqCounter + 1.U, routingQEnqCounter)
				val routingQDeqCounter = dontTouch(RegInit(0.U(64.W)))
				routingQDeqCounter := Mux(routingQ.io.deq.fire(), routingQDeqCounter + 1.U, routingQDeqCounter)

				val zeroPadder = Module (new ZeroPadder(typ.cloneType, burstSizeWithZeros))

				//val finalBuffer = Module (new Queue(typ.cloneType, burstSizeWithZeros))
				val finalBuffer = Module (new Queue(typ.cloneType, 1))

				val waitingForRoutingPacket = guidedArbiter.io.routingInfo.ready
				val waitingForNewBurst = RegEnable(!routingQ.io.deq.fire(), true.B, waitingForRoutingPacket)

				when (routingQ.io.deq.fire()) {
								waitingForNewBurst := false.B
				}

				/*
				val remaining_packets_this_burst = RegInit(UInt(burstSizeWithZeros))

				when (finalBuffer.io.enq.fire()) {
								remaining_packets_this_burst := remaining_packets_this_burst - 1.U
				}

				when (remaining_packets_this_burst === 0.U) {
								when (finalBuffer.io.enq.fire()) {
												remaining_packets_this_burst := UInt(burstSizeWithZeros - 1)
												} .otherwise {
																remaining_packets_this_burst := UInt(burstSizeWithZeros)
												}
				}

				zeroPadder.io.in <> guidedArbiter.io.out
				zeroPadder.io.startPadding := waitingForRoutingPacket && !waitingForNewBurst && remaining_packets_this_burst > 0.U
				finalBuffer.io.enq <> zeroPadder.io.out
				*/

				io.numStartedSubmissions := routingQDeqCounter

				routingRequestArbiter.io.in <> io.subRequest
				routingQ.io.enq <> routingRequestArbiter.io.out
				guidedArbiter.io.routingInfo <> routingQ.io.deq

				val coreSpecificQueues = Vec(Seq.fill(nCores) { Module(new Queue(typ.cloneType, 4 * burstSizeWithZeros)).io })

				for (i <- 0 until nCores) {
								 coreSpecificQueues(i).enq <> io.subQInterface(i)
								 guidedArbiter.io.in(i) <> coreSpecificQueues(i).deq
				}
        
				val delayerAdapter = Module(new DelayerAdapter(typ.cloneType))
				val delayerAdapterPushCounter = dontTouch(RegInit(0.U(64.W)))
				delayerAdapterPushCounter := Mux(delayerAdapter.io.in.fire(), delayerAdapterPushCounter + 1.U, delayerAdapterPushCounter)
				val delayerAdapterPopCounter = dontTouch(RegInit(0.U(64.W)))
				delayerAdapterPopCounter := Mux(delayerAdapter.io.out.fire(), delayerAdapterPopCounter + 1.U, delayerAdapterPopCounter)

				//delayerAdapter.io.in <> finalBuffer.io.deq
				delayerAdapter.io.in <> guidedArbiter.io.out
				io.picosSubMaster <> delayerAdapter.io.out
}

trait HasRPEDebugSignals extends Bundle {
				val readyPacketWireMonitor   		= Output(new ReadyPacket)
				val roccReadyQueueMonitor    		= Decoupled(new ReadyPacket)
}

class ReadyPacketExtractor(nCores: Int, qDepth: Int = 4) extends Module {
	//val io = IO(new ReadyTaskIO(nCores) with HasRPEDebugSignals)
	val io = IO(new ReadyTaskIO(nCores) with HasRPEDebugSignals)

	implicit val p: Parameters = new BlankConfig

	val wf_arbiter = Module(new InOrderArbiter(UInt(0.W), new ReadyPacket, nCores))
	wf_arbiter.io.in_req <> io.readyRequestInterface

	val roccReadyQueue = Module( new Queue(new ReadyPacket, 64) )
	dontTouch(roccReadyQueue.io.count)
	val roccReadyQueueEnqCounter = dontTouch(RegInit(0.U(64.W)))
	roccReadyQueueEnqCounter := Mux(roccReadyQueue.io.enq.fire(), roccReadyQueueEnqCounter + 1.U, roccReadyQueueEnqCounter)
	val roccReadyQueueDeqCounter = dontTouch(RegInit(0.U(64.W)))
	roccReadyQueueDeqCounter := Mux(roccReadyQueue.io.deq.fire(), roccReadyQueueDeqCounter + 1.U, roccReadyQueueDeqCounter)

	val coreSpecificReadyQueues = Vec(Seq.fill(nCores) { Module(new Queue(new ReadyPacket, qDepth)).io })

	// This sends ready tasks back to the cores
	for (i <- 0 until nCores) {
					coreSpecificReadyQueues(i).enq <> wf_arbiter.io.in_resp(i)
					io.readyQInterface(i) <> coreSpecificReadyQueues(i).deq
	}

	// This makes ready tasks be fetched from a queue that is fed by the single Ready Queue of Picos
	wf_arbiter.io.out_resp <> roccReadyQueue.io.deq

	// This makes sure that the data requests are acknowledged by the arbiter
	// TODO: THIS SEEMS VERY DANGEROUS!
	wf_arbiter.io.out_req.ready := true.B
	val wf_counter = Reg(t = UInt(3.W), init = 0.U)

	val picosReadyInterfaceNotEmpty = io.picosReadyInterface.valid
	val picosReadyInterfacePop = io.picosReadyInterface.empty
	val picosReadyInterfaceBits = io.picosReadyInterface.data

	val readyPacket = Wire(new ReadyPacket)

	val num_args = RegEnable(picosReadyInterfaceBits(15, 8), wf_counter === 0.U)
	val wf_picosID = RegEnable(picosReadyInterfaceBits(31, 0), wf_counter === 1.U)
	val wf_parentID = RegEnable(picosReadyInterfaceBits, wf_counter === 2.U)

	val canProceed = picosReadyInterfaceNotEmpty && (wf_counter < 3.U || roccReadyQueue.io.enq.fire())

	readyPacket.nanosID := picosReadyInterfaceBits(31, 0)
	readyPacket.picosID := wf_picosID

	roccReadyQueue.io.enq.valid := (wf_counter === 3.U) && picosReadyInterfacePop && picosReadyInterfaceNotEmpty
	// TODO: Check whether the following line makes sense
	roccReadyQueue.io.enq.bits := readyPacket
	picosReadyInterfacePop := picosReadyInterfaceNotEmpty && roccReadyQueue.io.enq.ready

	when (canProceed) {
		when (wf_counter < 3.U) {
			wf_counter := wf_counter + 1.U 
		} .otherwise {
			wf_counter := 0.U
		}
	}
}

class CoreRealPicosManager(nCores: Int, qDepth: Int = 16)(implicit p: Parameters) extends Module {
	val io = IO(new RealManagerIO(nCores))
	val ret_arbiter = Module(new RRArbiter(UInt(64.W), nCores))

	val retQ = Module( new Queue(UInt(64.W), qDepth) )
	dontTouch(retQ.io.count)
	val retQEnqCounter = dontTouch(RegInit(0.U(64.W)))
	retQEnqCounter := Mux(retQ.io.enq.fire(), retQEnqCounter + 1.U, retQEnqCounter)
	val retQDeqCounter = dontTouch(RegInit(0.U(64.W)))
	retQDeqCounter := Mux(retQ.io.deq.fire(), retQDeqCounter + 1.U, retQDeqCounter)

	// Control access to Retirement Queue with RRArbiter
	ret_arbiter.io.in <> io.retInterface

	// Implements writes to the Picos FiFo Retirement Interface
	val delayerAdapter = Module(new DelayerAdapter(UInt(64.W)))
	retQ.io.enq <> ret_arbiter.io.out
	delayerAdapter.io.in <> retQ.io.deq

	io.picosFinishedInterface.data := delayerAdapter.io.out.bits
	io.picosFinishedInterface.valid := delayerAdapter.io.out.valid
	delayerAdapter.io.out.ready := io.picosFinishedInterface.empty

	// Implement submission interactions
	// FIXME: Re-activate the following code with adaptations to
	//        let it interact with the new interface
	/*
	val submissionHandler = Module(new SubmissionHandler(nCores, UInt(32.W), 48, 200))

	submissionHandler.io.tag <> io.tag
	submissionHandler.io.subQInterface <> io.subQInterface
	submissionHandler.io.subRequest <> io.subRequest
	io.picosSubInterface.data := submissionHandler.io.picosSubMaster.bits
	io.picosSubInterface.valid := submissionHandler.io.picosSubMaster.valid
	submissionHandler.io.picosSubMaster.ready := io.picosSubInterface.empty

	val submissionCounter = Wire(UInt(64.W))
	val retirementCounter = Wire(UInt(64.W))
	val numInflightTasks  = Wire(UInt(64.W))

	retirementCounter := retQDeqCounter
	submissionCounter := submissionHandler.io.numStartedSubmissions
	numInflightTasks := submissionCounter - retirementCounter

	//submissionHandler.io.enabled := numInflightTasks <= 250.U(64.W)
	submissionHandler.io.enabled := true.B
	*/
}
