package freechips.rocketchip.integrators

import Chisel._
//import chisel3.iotesters._
//import org.scalatest._
import freechips.rocketchip.util.{InOrderArbiter, GuidedArbiter, RoutingInfo, ZeroPadder, DelayerAdapter, TDIOMuxPC}
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import chisel3.dontTouch

import picos_v4_integration_elements.{ArgsAndParentInfo, DepInfo, TaskInfo}

class ReadyPacket extends Bundle {
				val nanosID                     = UInt(64.W)
				val picosID                     = UInt(32.W)
}

object ReadyPacket {
				def apply() = new ReadyPacket
}

class PicosReadyBundle[T <: Data](typ: T = UInt(64.W)) extends Bundle {
				val data                        = typ.cloneType.asInput
				val readyEquivalentPop          = Bool().asOutput
				val validEquivalentIsNotEmpty   = Bool().asInput

				override def cloneType = new PicosReadyBundle(typ.cloneType).asInstanceOf[this.type]
}

class PicosFifoBundle[T <: Data](typ: T = UInt(64.W)) extends Bundle {
				val data                        = typ.cloneType.asOutput
				val valid                       = Bool().asOutput
				val empty                       = Bool().asInput

				override def cloneType = new PicosFifoBundle(typ.cloneType).asInstanceOf[this.type]
}

class RealManagerIO[T <: Data](nCores: Int, typ: T = UInt(64.W), burstSizeWithZeros: Int = 48)(implicit p: Parameters) extends Bundle {
				val tag                     = Flipped(Vec(nCores, UInt(64.W).asOutput))
				val subQInterface           = Flipped(Vec(nCores, Decoupled(UInt(64.W))))
				val subRequest              = Flipped(Vec(nCores, Decoupled(RoutingInfo(64, 48))))
				val retInterface            = Flipped(Vec(nCores, Decoupled(UInt(64.W))))
				val readyRequestInterface   = Flipped(Vec(nCores, Decoupled(UInt(0.W))))
				val readyQInterface         = Vec(nCores, Decoupled(new ReadyPacket))

				//TODO: Remove the dontTouch to make sure that these signals are being used
				val argsAndParentInfoVec      = dontTouch( Flipped(Vec(nCores, Decoupled(new ArgsAndParentInfo))) )
				val taskInfoVec            		= dontTouch( Flipped(Vec(nCores, Decoupled(new TaskInfo))) )
				val depInfoVec   							= dontTouch( Flipped(Vec(nCores, Decoupled(Vec(2, new DepInfo)))) )

				val picosFinishedInterface  = new PicosFifoBundle(typ.cloneType)
				val picosReadyInterface     = new PicosReadyBundle(typ.cloneType)
				val picosSubInterface       = new PicosFifoBundle(typ.cloneType)

				val pmException               = UInt(4.W).asOutput
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

class CoreRealPicosManager(nCores: Int, qDepth: Int = 16)(implicit p: Parameters) extends Module {
	val io = IO(new RealManagerIO(nCores))
	val ret_arbiter = Module(new RRArbiter(UInt(64.W), nCores))

	val roccReadyQueue = Module( new Queue(new ReadyPacket, 257 * 3) )
	dontTouch(roccReadyQueue.io.count)
	val roccReadyQueueEnqCounter = dontTouch(RegInit(0.U(64.W)))
	roccReadyQueueEnqCounter := Mux(roccReadyQueue.io.enq.fire(), roccReadyQueueEnqCounter + 1.U, roccReadyQueueEnqCounter)
	val roccReadyQueueDeqCounter = dontTouch(RegInit(0.U(64.W)))
	roccReadyQueueDeqCounter := Mux(roccReadyQueue.io.deq.fire(), roccReadyQueueDeqCounter + 1.U, roccReadyQueueDeqCounter)

	val retQ = Module( new Queue(UInt(64.W), qDepth) )
	dontTouch(retQ.io.count)
	val retQEnqCounter = dontTouch(RegInit(0.U(64.W)))
	retQEnqCounter := Mux(retQ.io.enq.fire(), retQEnqCounter + 1.U, retQEnqCounter)
	val retQDeqCounter = dontTouch(RegInit(0.U(64.W)))
	retQDeqCounter := Mux(retQ.io.deq.fire(), retQDeqCounter + 1.U, retQDeqCounter)

	val wf_arbiter = Module(new InOrderArbiter(UInt(0.W), new ReadyPacket, nCores))
	val coreSpecificReadyQueues = Vec(Seq.fill(nCores) { Module(new Queue(new ReadyPacket, qDepth)).io })

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

	// Implement fetching of ready packets
	// Implement arbitration of the consumption of the single readyQueue
	// This sends ready task requests to the arbiter

	wf_arbiter.io.in_req <> io.readyRequestInterface

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
	val exceptionReg = Reg(t = UInt(4.W), init = 0.U)

	val picosReadyInterfaceNotEmpty = io.picosReadyInterface.validEquivalentIsNotEmpty
	val picosReadyInterfacePop = io.picosReadyInterface.readyEquivalentPop
	val picosReadyInterfaceBits = io.picosReadyInterface.data
	val readyPacket = Wire(new ReadyPacket)

	val wf_taskID_high = RegEnable(picosReadyInterfaceBits, wf_counter === 1.U)
	val wf_taskID_low = RegEnable(picosReadyInterfaceBits, wf_counter === 3.U)
	val wf_xor = Reg(t = UInt(32.W), init = 0.U)

	val canProceed = picosReadyInterfacePop ||
	(wf_counter === 1.U || wf_counter === 3.U || wf_counter === 7.U) || 
	(wf_counter === 5.U && roccReadyQueue.io.enq.fire())

	readyPacket.nanosID := Cat(wf_taskID_high, wf_taskID_low)
	readyPacket.picosID := picosReadyInterfaceBits

	io.pmException := exceptionReg

	roccReadyQueue.io.enq.valid := (wf_counter === 5.U)
	roccReadyQueue.io.enq.bits := readyPacket
	picosReadyInterfacePop := picosReadyInterfaceNotEmpty && (wf_counter === 0.U || wf_counter === 2.U || wf_counter === 4.U || wf_counter === 6.U)

	when (canProceed) {
		when (wf_counter === 1.U || wf_counter === 3.U || wf_counter === 5.U) {
			wf_xor := wf_xor ^ picosReadyInterfaceBits
		} .otherwise {
			when (wf_counter === 7.U) {
				when (wf_xor =/= picosReadyInterfaceBits) {
								exceptionReg := exceptionReg | 2.U 
				}
				wf_xor := 0.U
			}
		}

		// The following change makes the system compatible with a 3-packet ready-task description
		// wf_counter := wf_counter + 1.U 
		when (wf_counter < 5.U) {
			wf_counter := wf_counter + 1.U 
		} .otherwise {
			wf_counter := 0.U
		}
	}

	when (wf_counter > 0.U && !picosReadyInterfaceNotEmpty) {
		exceptionReg := exceptionReg | 1.U 
	}
}
