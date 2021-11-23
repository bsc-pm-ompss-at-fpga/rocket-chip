package freechips.rocketchip.integrators

import Chisel._
//import chisel3.iotesters._
//import org.scalatest._
import freechips.rocketchip.util.{InOrderArbiter, GuidedArbiter, RoutingInfo, ZeroPadder, DelayerAdapter, TDIOMuxPC}
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

class ReadyPacket extends Bundle {
				val nanosID                     = UInt(64.W)
				val picosID                     = UInt(32.W)
}

object ReadyPacket {
				def apply() = new ReadyPacket
}

class PicosReadyBundle[T <: Data](typ: T = UInt(32.W)) extends Bundle {
				val data                        = typ.cloneType.asInput
				val readyEquivalentPop          = Bool().asOutput
				val validEquivalentIsNotEmpty   = Bool().asInput

				override def cloneType = new PicosReadyBundle(typ.cloneType).asInstanceOf[this.type]
}

class PicosFifoBundle[T <: Data](typ: T = UInt(32.W)) extends Bundle {
				val data                        = typ.cloneType.asOutput
				val valid                       = Bool().asOutput
				val empty                       = Bool().asInput

				override def cloneType = new PicosFifoBundle(typ.cloneType).asInstanceOf[this.type]
}

class RealManagerIO[T <: Data](nCores: Int, typ: T = UInt(32.W), burstSizeWithZeros: Int = 48)(implicit p: Parameters) extends Bundle {
				val tag                     = Flipped(Vec(nCores, UInt(64.W).asOutput))
				val subQInterface           = Flipped(Vec(nCores, Decoupled(UInt(32.W))))
				val subRequest              = Flipped(Vec(nCores, Decoupled(RoutingInfo(64, 48))))
				val retInterface            = Flipped(Vec(nCores, Decoupled(UInt(32.W))))
				val readyRequestInterface   = Flipped(Vec(nCores, Decoupled(UInt(0.W))))
				val readyQInterface         = Vec(nCores, Decoupled(new ReadyPacket))

				val picosFinishedInterface  = new PicosFifoBundle(typ.cloneType)
				val picosReadyInterface     = new PicosReadyBundle(typ.cloneType)
				val picosSubInterface       = new PicosFifoBundle(typ.cloneType)

				val pmException               = UInt(4.W).asOutput
}

class SubmissionHandler[T <: Data](nCores: Int, typ: T, burstSizeWithZeros: Int = 48, routingQDepth: Int = 48) extends Module {
				val io = IO(new Bundle {
								val tag                   = Flipped(Vec(nCores, UInt(64.W).asOutput))
								val subQInterface         = Flipped(Vec(nCores, Decoupled(typ.cloneType)))
								val subRequest            = Flipped(Vec(nCores, Decoupled(RoutingInfo(nCores, burstSizeWithZeros))))
								val picosSubMaster        = Decoupled(typ.cloneType)
				})

				val taggedMuxPC = Module(new TDIOMuxPC(typ.cloneType, nCores))
				val routingRequestArbiter = Module(new RRArbiter(RoutingInfo(nCores, burstSizeWithZeros), nCores))
				val guidedArbiter = Module(new GuidedArbiter(typ.cloneType, nCores, burstSizeWithZeros))

				val routingQ = Module(new Queue(RoutingInfo(nCores, burstSizeWithZeros), routingQDepth))
				val zeroPadder = Module (new ZeroPadder(typ.cloneType, burstSizeWithZeros))

				val finalBuffer = Module (new Queue(typ.cloneType, burstSizeWithZeros))

				val waitingForRoutingPacket = guidedArbiter.io.routingInfo.ready
				val waitingForNewBurst = RegEnable(!routingQ.io.deq.fire(), true.B, waitingForRoutingPacket)

				when (routingQ.io.deq.fire()) {
								waitingForNewBurst := false.B
				}

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

				routingRequestArbiter.io.in <> io.subRequest
				routingQ.io.enq <> routingRequestArbiter.io.out
				guidedArbiter.io.routingInfo <> routingQ.io.deq

				val coreSpecificQueues = Vec(Seq.fill(nCores) { Module(new Queue(typ.cloneType, 4 * burstSizeWithZeros)).io })

				for (i <- 0 until nCores) {
								coreSpecificQueues(i).enq <> io.subQInterface(i)
								guidedArbiter.io.in(i) <> coreSpecificQueues(i).deq
				}

				val delayerAdapter = Module(new DelayerAdapter(typ.cloneType))
				delayerAdapter.io.in <> finalBuffer.io.deq
				io.picosSubMaster <> delayerAdapter.io.out

				//io.picosSubMaster.valid := finalBuffer.io.deq.valid && io.picosSubMaster.ready
				//finalBuffer.io.deq.ready := io.picosSubMaster.ready
				//io.picosSubMaster.bits := finalBuffer.io.deq.bits
}

class CoreRealPicosManager(nCores: Int, qDepth: Int = 16)(implicit p: Parameters) extends Module {
	val io = IO(new RealManagerIO(nCores))
	val ret_arbiter = Module(new RRArbiter(UInt(32.W), nCores))
	val roccReadyQueue = Module( new Queue(new ReadyPacket, 260) )
	val retQ = Module( new Queue(UInt(32.W), qDepth) )
	val wf_arbiter = Module(new InOrderArbiter(UInt(0.W), new ReadyPacket, nCores))
	val coreSpecificReadyQueues = Vec(Seq.fill(nCores) { Module(new Queue(new ReadyPacket, qDepth)).io })

	val n_inflight_counter = Reg(t = UInt(9.W), init = 0.U)
	val n_unfulfilled_wf_requests_counter = Reg(t = UInt(4.W), init = 0.U)
	val n_being_executed_counter = Reg(t = UInt(4.W), init = 0.U)

	val n_inflight_try_increase = roccReadyQueue.io.enq.fire()
	val n_inflight_try_decrease = retQ.io.deq.fire()

	val n_unfulfilled_wf_requests_try_increase = io.readyRequestInterface.map(_.fire()).reduce(_ || _)
	val n_unfulfilled_wf_requests_try_decrease = roccReadyQueue.io.deq.fire()

	val n_being_executed_try_increase = roccReadyQueue.io.deq.fire()
	val n_being_executed_try_decrease = retQ.io.deq.fire()

	when (n_inflight_try_increase && !n_inflight_try_decrease) {
					n_inflight_counter := n_inflight_counter + 1.U
	} .otherwise {
					when (!n_inflight_try_increase && n_inflight_try_decrease) {
									n_inflight_counter := n_inflight_counter - 1.U
					}
	}

	when (n_unfulfilled_wf_requests_try_increase && !n_unfulfilled_wf_requests_try_decrease) {
					n_unfulfilled_wf_requests_counter := n_unfulfilled_wf_requests_counter + 1.U
	} .otherwise {
					when (!n_unfulfilled_wf_requests_try_increase && n_unfulfilled_wf_requests_try_decrease) {
									n_unfulfilled_wf_requests_counter := n_unfulfilled_wf_requests_counter - 1.U
					}
	}

	when (n_being_executed_try_increase && !n_being_executed_try_decrease) {
					n_being_executed_counter := n_being_executed_counter + 1.U
	} .otherwise {
					when (!n_being_executed_try_increase && n_being_executed_try_decrease) {
									n_being_executed_counter := n_being_executed_counter - 1.U
					}
	}

	// Control access to Retirement Queue with RRArbiter
	ret_arbiter.io.in <> io.retInterface

	// Implements writes to the Picos FiFo Retirement Interface
	val delayerAdapter = Module(new DelayerAdapter(UInt(32.W)))
	retQ.io.enq <> ret_arbiter.io.out
	delayerAdapter.io.in <> retQ.io.deq
	//io.picosFinishedInterface <> delayerAdapter.io.out

	io.picosFinishedInterface.data := delayerAdapter.io.out.bits
	io.picosFinishedInterface.valid := delayerAdapter.io.out.valid
	delayerAdapter.io.out.ready := io.picosFinishedInterface.empty

	//io.picosFinishedInterface.data := ret_arbiter.io.out.bits    
	//io.picosFinishedInterface.valid := ret_arbiter.io.out.valid && io.picosFinishedInterface.empty
	//ret_arbiter.io.out.ready := io.picosFinishedInterface.empty

	// Implement fetching of ready packets
	// Implement arbitration of the consumption of the single readyQueue
	// This sends ready task requests to the arbiter

	//wf_arbiter.io.in_req <> io.readyRequestInterface

	// This sends ready tasks back to the cores
	for (i <- 0 until nCores) {
					coreSpecificReadyQueues(i).enq <> wf_arbiter.io.in_resp(i)
					io.readyQInterface(i) <> coreSpecificReadyQueues(i).deq

					// This implements H1 and H2
					wf_arbiter.io.in_req(i).valid := io.readyRequestInterface(i).valid && (i.asUInt(9.W) < n_inflight_counter) && (n_unfulfilled_wf_requests_counter < (n_inflight_counter - n_being_executed_counter))
					io.readyRequestInterface(i).ready := wf_arbiter.io.in_req(i).ready && (i.asUInt(9.W) < n_inflight_counter) && (n_unfulfilled_wf_requests_counter < (n_inflight_counter - n_being_executed_counter))
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
	//picosReadyInterfacePop := (wf_counter === 0.U || wf_counter === 2.U || wf_counter === 4.U || wf_counter === 6.U)
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

									wf_counter := wf_counter + 1.U 
	}

	when (wf_counter > 0.U && !picosReadyInterfaceNotEmpty) {
					exceptionReg := exceptionReg | 1.U 
	}

	// Implement submission interactions
	val submissionHandler = Module(new SubmissionHandler(nCores, UInt(32.W), 48))

	submissionHandler.io.tag <> io.tag
	submissionHandler.io.subQInterface <> io.subQInterface
	submissionHandler.io.subRequest <> io.subRequest
	io.picosSubInterface.data := submissionHandler.io.picosSubMaster.bits
	io.picosSubInterface.valid := submissionHandler.io.picosSubMaster.valid
	submissionHandler.io.picosSubMaster.ready := io.picosSubInterface.empty
}

class RealPicosManager(nCores: Int, qDepth: Int = 16)(implicit p: Parameters) extends LazyModule {
				lazy val module = new LazyModuleImp(this) {
								val io = IO(new RealManagerIO(nCores))
								val ret_arbiter = Module(new RRArbiter(UInt(32.W), nCores))
								val roccReadyQueue = Module( new Queue(new ReadyPacket, 260) )
								val retQ = Module( new Queue(UInt(32.W), qDepth) )
								val wf_arbiter = Module(new InOrderArbiter(UInt(0.W), new ReadyPacket, nCores))
								val coreSpecificReadyQueues = Vec(Seq.fill(nCores) { Module(new Queue(new ReadyPacket, qDepth)).io })

								val n_inflight_counter = Reg(t = UInt(9.W), init = 0.U)
								val n_unfulfilled_wf_requests_counter = Reg(t = UInt(4.W), init = 0.U)
								val n_being_executed_counter = Reg(t = UInt(4.W), init = 0.U)

								val n_inflight_try_increase = roccReadyQueue.io.enq.fire()
								val n_inflight_try_decrease = retQ.io.deq.fire()

								val n_unfulfilled_wf_requests_try_increase = io.readyRequestInterface.map(_.fire()).reduce(_ || _)
								val n_unfulfilled_wf_requests_try_decrease = roccReadyQueue.io.deq.fire()

								val n_being_executed_try_increase = roccReadyQueue.io.deq.fire()
								val n_being_executed_try_decrease = retQ.io.deq.fire()

								when (n_inflight_try_increase && !n_inflight_try_decrease) {
												n_inflight_counter := n_inflight_counter + 1.U
								} .otherwise {
												when (!n_inflight_try_increase && n_inflight_try_decrease) {
																n_inflight_counter := n_inflight_counter - 1.U
												}
								}

								when (n_unfulfilled_wf_requests_try_increase && !n_unfulfilled_wf_requests_try_decrease) {
												n_unfulfilled_wf_requests_counter := n_unfulfilled_wf_requests_counter + 1.U
								} .otherwise {
												when (!n_unfulfilled_wf_requests_try_increase && n_unfulfilled_wf_requests_try_decrease) {
																n_unfulfilled_wf_requests_counter := n_unfulfilled_wf_requests_counter - 1.U
												}
								}

								when (n_being_executed_try_increase && !n_being_executed_try_decrease) {
												n_being_executed_counter := n_being_executed_counter + 1.U
								} .otherwise {
												when (!n_being_executed_try_increase && n_being_executed_try_decrease) {
																n_being_executed_counter := n_being_executed_counter - 1.U
												}
								}

								// Control access to Retirement Queue with RRArbiter
								ret_arbiter.io.in <> io.retInterface

								// Implements writes to the Picos FiFo Retirement Interface
								val delayerAdapter = Module(new DelayerAdapter(UInt(32.W)))
								retQ.io.enq <> ret_arbiter.io.out
								delayerAdapter.io.in <> retQ.io.deq
								//io.picosFinishedInterface <> delayerAdapter.io.out

								io.picosFinishedInterface.data := delayerAdapter.io.out.bits
								io.picosFinishedInterface.valid := delayerAdapter.io.out.valid
								delayerAdapter.io.out.ready := io.picosFinishedInterface.empty

								//io.picosFinishedInterface.data := ret_arbiter.io.out.bits    
								//io.picosFinishedInterface.valid := ret_arbiter.io.out.valid && io.picosFinishedInterface.empty
								//ret_arbiter.io.out.ready := io.picosFinishedInterface.empty

								// Implement fetching of ready packets
								// Implement arbitration of the consumption of the single readyQueue
								// This sends ready task requests to the arbiter

								//wf_arbiter.io.in_req <> io.readyRequestInterface

								// This sends ready tasks back to the cores
								for (i <- 0 until nCores) {
												coreSpecificReadyQueues(i).enq <> wf_arbiter.io.in_resp(i)
												io.readyQInterface(i) <> coreSpecificReadyQueues(i).deq

												// This implements H1 and H2
												wf_arbiter.io.in_req(i).valid := io.readyRequestInterface(i).valid && (i.asUInt(9.W) < n_inflight_counter) && (n_unfulfilled_wf_requests_counter < (n_inflight_counter - n_being_executed_counter))
												io.readyRequestInterface(i).ready := wf_arbiter.io.in_req(i).ready && (i.asUInt(9.W) < n_inflight_counter) && (n_unfulfilled_wf_requests_counter < (n_inflight_counter - n_being_executed_counter))
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
								//picosReadyInterfacePop := (wf_counter === 0.U || wf_counter === 2.U || wf_counter === 4.U || wf_counter === 6.U)
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

																wf_counter := wf_counter + 1.U 
								}

								when (wf_counter > 0.U && !picosReadyInterfaceNotEmpty) {
												exceptionReg := exceptionReg | 1.U 
								}

								// Implement submission interactions
								val submissionHandler = Module(new SubmissionHandler(nCores, UInt(32.W), 48))

								submissionHandler.io.tag <> io.tag
								submissionHandler.io.subQInterface <> io.subQInterface
								submissionHandler.io.subRequest <> io.subRequest
								io.picosSubInterface.data := submissionHandler.io.picosSubMaster.bits
								io.picosSubInterface.valid := submissionHandler.io.picosSubMaster.valid
								submissionHandler.io.picosSubMaster.ready := io.picosSubInterface.empty
				}
}
//
//case object BlankField extends Field[Boolean]
//
//class BlankConfig extends Config((site, here, up) => {
//				case BlankField => false
//})
//
//object PicosManagerDriver extends App {
//				implicit val p: Parameters = new BlankConfig
//				//chisel3.Driver.execute(args, () => new TestBench(2))
//				//chisel3.Driver.execute(args, () => new SubmissionHandler(2, UInt(32.W)))
//				chisel3.Driver.execute(args, () => LazyModule(new RealPicosManager(4)(p)).module)
//}
