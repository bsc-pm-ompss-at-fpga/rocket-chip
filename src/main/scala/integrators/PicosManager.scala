package freechips.rocketchip.integrators

import Chisel._
//import chisel3.iotesters._
//import org.scalatest._
import freechips.rocketchip.util.{InOrderArbiter, GuidedArbiter, RoutingInfo, DelayerAdapter}
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import chisel3.dontTouch

import picos_v4_integration_elements.{ArgsAndParentInfo, DepInfo, TaskInfo}
import freechips.rocketchip.tile.{BlankConfig}

class ReadyPacket extends Bundle {
				val nanosID                     = UInt(32.W)
				val picosID                     = UInt(64.W)
}

object ReadyPacket {
				def apply() = new ReadyPacket
}

trait HasDest extends Bundle{
				val dest                     		= UInt(3.W).asOutput
}

class PicosFifoBundle[T <: Data](typ: T = UInt(64.W)) extends Bundle {
				val data                        = typ.cloneType.asOutput
				val valid                       = Bool().asOutput
				val empty                       = Bool().asInput
				val last												= Bool().asOutput

				def fire() = empty && valid

				override def cloneType = new PicosFifoBundle(typ.cloneType).asInstanceOf[this.type]
}

class PicosRawRetirementBundle extends Bundle {
				val payload                     = UInt(64.W)
				val tid                         = UInt(32.W)

				override def cloneType = new PicosRawRetirementBundle().asInstanceOf[this.type]
}

class PicosRawReadyBundle extends Bundle {
				val payload                     = UInt(64.W)
				val tid                         = UInt(32.W)

				override def cloneType = new PicosRawReadyBundle().asInstanceOf[this.type]
}

class ReadyTaskIO(nCores: Int) extends Bundle {
				val readyRequestInterface   = Flipped(Vec(nCores, Decoupled(UInt(0.W))))
				val readyQInterface         = Vec(nCores, Decoupled(new ReadyPacket))
				val picosReadyInterface     = Flipped(new PicosFifoBundle(new PicosRawReadyBundle))
}

class RealManagerIO[T <: Data](nCores: Int, typ: T = UInt(64.W), burstSizeWithZeros: Int = 48)(implicit p: Parameters) extends Bundle with HasDest {
				val retInterface            = Flipped(Vec(nCores, Decoupled(UInt(64.W))))

				val readyRequestInterface   = Flipped(Vec(nCores, Decoupled(UInt(0.W))))
				val readyQInterface         = Vec(nCores, Decoupled(new ReadyPacket))

				val argsAndParentInfoVec      = Flipped(Vec(nCores, Decoupled(new ArgsAndParentInfo)))
				val taskInfoVec            		= Flipped(Vec(nCores, Decoupled(new TaskInfo)))
				val depInfoVec   							= Flipped(Vec(nCores, Decoupled(Vec(2, new DepInfo))))

				val picosFinishedInterface  = new PicosFifoBundle(new PicosRawRetirementBundle)
				val picosReadyInterface     = Flipped(new PicosFifoBundle(new PicosRawReadyBundle))
				val picosSubInterface       = new PicosFifoBundle(typ.cloneType)
				val picosAckInterface       = Flipped(new PicosFifoBundle(UInt(2.W)))

				val pmException               = UInt(4.W).asOutput
}

trait HasDepInfo extends Bundle{
				val depInfo   							  = Flipped(Decoupled(Vec(2, new DepInfo)))
}

class DependenceInfoAdapterIO[T <: Data](typ: T = UInt(64.W)) extends Bundle {
				val depInfo   							  = Flipped(Decoupled(Vec(2, new DepInfo)))
				val picosSubData							= Decoupled(typ.cloneType)

				override def cloneType = new DependenceInfoAdapterIO(typ.cloneType).asInstanceOf[this.type]
}

class DependenceInfoAdapter[T <: Data](typ: T = UInt(64.W)) extends Module {
				val io = IO(new DependenceInfoAdapterIO(typ.cloneType))

				val VOID_TYPE = 0.U
				val IN_TYPE = 1.U
				val OUT_TYPE = 2.U
				val IOUT_TYPE = 3.U

				val secondSubmissionPending = RegInit(false.B)
				val activeDep = RegInit(0.U(1.W))
				val twoZeros = 0.U(2.W)

				// ASSUMPTION: If only one of the dependencies described
				//             depInfo is valid, that is always going to
				//             be depInfo(0), not depInfo(1)
				when (io.depInfo.bits(1).kind === VOID_TYPE) {
					io.depInfo.ready := io.picosSubData.ready
					io.picosSubData.bits := Cat(Seq(io.depInfo.bits(0).kind, twoZeros, io.depInfo.bits(0).pointer))
					io.picosSubData.valid := io.depInfo.valid
				} .otherwise {
					io.depInfo.ready := io.picosSubData.ready && (activeDep === 1.U)
					io.picosSubData.bits := Cat(Seq(io.depInfo.bits(activeDep).kind, twoZeros, io.depInfo.bits(activeDep).pointer))
					io.picosSubData.valid := io.depInfo.valid
					when (io.picosSubData.fire()) {
						activeDep := activeDep + 1.U
					}
				}
}

trait HasInnerRoutingInfo extends Bundle {
				// This is used for feeding the 2-entry GuidedArbiter
				// that is instantiated for each core, which builds
				// complete submission sequences from the output of
				// SubmissionHeaderGenerator and the DependencyAdapter
				val routingInfo								= Decoupled((new RoutingInfo(2, 3 + 15 + 15)))
}

class SubmissionHeaderGeneratorIO[T <: Data](nCores: Int, typ: T = UInt(64.W)) extends Bundle with HasDest {
				val argsAndParentInfo      		= Flipped(Decoupled(new ArgsAndParentInfo))
				val taskInfo 		           		= Flipped(Decoupled(new TaskInfo))

				// This is used for feeding the nCores-entry GuidedArbiter
				// that is fed by the core-specific GuidedArbiter building
				// submission sequences out of elementary data
				val routingInfoOuter					= Decoupled((new RoutingInfo(nCores, 3 + 15 + 15)))

				val picosSubData							= Decoupled(typ.cloneType)
				val coreID										= Input(UInt(log2Ceil(nCores).W))
				val suggestedTaskNumber				= Input(UInt(32.W))
}

class SubmissionHeaderGenerator[T <: Data](nCores: Int, typ: T = UInt(64.W)) extends Module {
				val io = IO(new SubmissionHeaderGeneratorIO(nCores, typ) with HasInnerRoutingInfo)
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

					when (io.suggestedTaskNumber =/= 0.U) {
						io.picosSubData.bits := Cat(Seq(io.suggestedTaskNumber, twelveZeros, numDeps, sixteenZeros))
					} .otherwise {
						io.picosSubData.bits := Cat(Seq(taskNumberCounter, twelveZeros, numDeps, sixteenZeros))
					}

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

class CoreSpecificSubmissionHandler[T <: Data](nCores: Int, maxSequenceLength: Int = 33, typ: T = UInt(64.W)) extends Module {
				val io = IO(new SubmissionHeaderGeneratorIO(nCores, typ.cloneType) with HasDepInfo)

				val downtimeBetweenSubmissions = 0

				val headerGenerator = Module(new SubmissionHeaderGenerator(nCores))
				val dependenceInfoAdapter = Module(new DependenceInfoAdapter)
				val leafGuidedArbiter = Module(new GuidedArbiter(typ.cloneType, 2, maxSequenceLength, downtimeBetweenSubmissions))

				val leafRoutingPacketsQueue = Module(new Queue(new RoutingInfo(2, 3 + 15 + 15), 2))

				headerGenerator.io.coreID := io.coreID
				headerGenerator.io.argsAndParentInfo := io.argsAndParentInfo
				headerGenerator.io.taskInfo := io.taskInfo
				headerGenerator.io.suggestedTaskNumber := io.suggestedTaskNumber
				io.routingInfoOuter := headerGenerator.io.routingInfoOuter

				dependenceInfoAdapter.io.depInfo := io.depInfo

				// TODO: Check whether it would make sense
				//			 to disable it at any moment
				leafGuidedArbiter.io.enabled := true.B

				leafRoutingPacketsQueue.io.enq <> headerGenerator.io.routingInfo
				leafGuidedArbiter.io.routingInfo <> leafRoutingPacketsQueue.io.deq
				leafGuidedArbiter.io.in(0) <> headerGenerator.io.picosSubData
				leafGuidedArbiter.io.in(1) <> dependenceInfoAdapter.io.picosSubData
				io.picosSubData <> leafGuidedArbiter.io.out
}

class SubmissionHandlerIO[T <: Data](nCores: Int, typ: T = UInt(64.W)) extends Bundle with HasDest {
				val argsAndParentInfoVec  = Flipped(Vec(nCores, Decoupled(new ArgsAndParentInfo)))
				val taskInfoVec 		      = Flipped(Vec(nCores, Decoupled(new TaskInfo)))
				val depInfoVec  				  = Flipped(Vec(nCores, Decoupled(Vec(2, new DepInfo))))
				val picosAckInterface 		= Flipped(new PicosFifoBundle(UInt(2.W)))

				val picosSubMaster        = Decoupled(typ.cloneType)
				val numStartedSubmissions = UInt(64.W).asOutput
				val enabled               = Bool().asInput

				override def cloneType = new SubmissionHandlerIO(nCores, typ.cloneType).asInstanceOf[this.type]
}

class SubmissionHandler[T <: Data](nCores: Int, typ: T = UInt(64.W), maxSequenceLength: Int = 33, routingQDepth: Int = 0, numDownCycles: Int = 0) extends Module {
				val io = IO(new SubmissionHandlerIO(nCores, typ))

				val coreSubmissionHandlers = Vec.fill(nCores) {Module(new CoreSpecificSubmissionHandler(nCores, maxSequenceLength, typ)).io}
				val masterGuidedArbiter = Module(new GuidedArbiter(typ, nCores, maxSequenceLength, numDownCycles))
				val routingArbiter = Module(new RRArbiter(new RoutingInfo(nCores, maxSequenceLength), nCores))
				val taskNumberCounter = RegInit(0.U(32.W))
				val destReg = RegInit(0.U(3.W))

				for (i <- 0 until nCores) {
					coreSubmissionHandlers(i).taskInfo <> io.taskInfoVec(i)
					coreSubmissionHandlers(i).argsAndParentInfo <> io.argsAndParentInfoVec(i)
					coreSubmissionHandlers(i).depInfo <> io.depInfoVec(i)
					coreSubmissionHandlers(i).coreID := i.U(log2Ceil(nCores).W)
					coreSubmissionHandlers(i).suggestedTaskNumber := taskNumberCounter
					masterGuidedArbiter.io.in(i) <> coreSubmissionHandlers(i).picosSubData
					routingArbiter.io.in(i) <> coreSubmissionHandlers(i).routingInfoOuter
				}

				val awaitingAcknowledgement = RegInit(false.B)

				masterGuidedArbiter.io.enabled := !awaitingAcknowledgement

				io.picosAckInterface.empty := awaitingAcknowledgement
				when (awaitingAcknowledgement) {
					awaitingAcknowledgement := !(io.picosAckInterface.valid && (io.picosAckInterface.data === 1.U(2.W)))
				} .otherwise {
					awaitingAcknowledgement := masterGuidedArbiter.io.last
				}

				when (routingArbiter.io.out.fire()) {
					taskNumberCounter := taskNumberCounter + 1.U

					val numPacketsToRoute = routingArbiter.io.out.bits.numPackets
					destReg := Mux(numPacketsToRoute === 3.U, 3.U, 2.U)
				}

				masterGuidedArbiter.io.routingInfo <> routingArbiter.io.out
				io.picosSubMaster <> masterGuidedArbiter.io.out
				io.dest := destReg
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
	val picosReadyInterfaceBits = io.picosReadyInterface.data.payload
	val picosReadyInterfaceTID = io.picosReadyInterface.data.tid

	val readyPacket = Wire(new ReadyPacket)

	val num_args = RegEnable(picosReadyInterfaceBits(15, 8), wf_counter === 0.U)
	val wf_picosID = RegEnable(Cat(picosReadyInterfaceTID, picosReadyInterfaceBits(31, 0)), wf_counter === 1.U)
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

class RetirementHandlerIO (nCores: Int) extends Bundle {
	val retInterface            = Flipped(Vec(nCores, Decoupled(UInt(64.W))))
	val picosFinishedInterface  = new PicosFifoBundle(new PicosRawRetirementBundle)
}

class RetirementHandler (nCores: Int) extends Module {
	val io = IO(new RetirementHandlerIO(nCores))

	// TODO: ADD IMPLEMENTATION
}

class CoreRealPicosManager(nCores: Int, qDepth: Int = 16)(implicit p: Parameters) extends Module {
	val io = IO(new RealManagerIO(nCores))

	val workFetchController = Module(new ReadyPacketExtractor(nCores))
	val submissionController = Module(new SubmissionHandler(nCores))
	val retirementController = Module(new RetirementHandler(nCores))

	for (i <- 0 until nCores) {
		retirementController.io.retInterface(i) <> io.retInterface(i)

		workFetchController.io.readyRequestInterface(i) <> io.readyRequestInterface(i)
		io.readyQInterface(i) <> workFetchController.io.readyQInterface(i)

		submissionController.io.argsAndParentInfoVec(i) <> io.argsAndParentInfoVec(i)
		submissionController.io.taskInfoVec(i) <> io.taskInfoVec(i)
		submissionController.io.depInfoVec(i) <> io.depInfoVec(i)
	}
	io.picosFinishedInterface <> retirementController.io.picosFinishedInterface

	workFetchController.io.picosReadyInterface <> io.picosReadyInterface

	submissionController.io.picosAckInterface <> io.picosAckInterface
	io.picosSubInterface <> submissionController.io.picosSubMaster
	io.dest := submissionController.io.dest
}
