package picos_v4_integration_elements

import chisel3._
import chisel3.util._

import freechips.rocketchip.tile.{EssentialRoCCCoreIO, PicosRelatedSignals}

class ArgsAndParentInfo extends Bundle {
  val parent_id = Bits(64.W)
  val num_args = Bits(4.W)
}

class DepInfo extends Bundle {
  val pointer = Bits(56.W)
  val kind = Bits(2.W)
}

class TaskInfo extends Bundle {
  val task_id = Bits(32.W)
  val num_deps = Bits(4.W)
}

class InstructionParserIO extends Bundle with EssentialRoCCCoreIO with PicosRelatedSignals {
}

/*
 * Kind options:
 *  0.U: To be discarded
 *  1.U: In
 *  2.U: Out
 *  3.U: Inout
 */

class InstructionParser extends Module {

  val io = IO(new InstructionParserIO)

  /*
   * TODO
   *  -- Remove as much of these 0 initializations when the module is fully implemented
   *  -- Ensure that work-fetching and retirement routing can occur even if this module
   *     only writes zeros to io.tag
   */
  io.pmException := 0.U
  io.readyRequestInterface.bits := 0.U
  io.retInterface.bits := 0.U
  io.readyQInterface.ready := 0.U
  io.readyRequestInterface.valid := 0.U

  // TODO: Clear these deprecated signals
  //io.subRequest.valid := 0.U
  //io.subRequest.bits.numPackets := 0.U
  //io.subRequest.bits.arbID := 0.U
  //io.subQInterface.bits := 0.U
  //io.subQInterface.valid := 0.U
  
  io.interrupt := 0.U
  io.tag := 0.U
  io.retInterface.valid := 0.U


  val s_idle :: s_work_request :: s_nanosid_fetch_work :: s_picosid_fetch_work :: s_retiring :: s_initiate_task :: s_add_args_and_parent_info :: s_send_in_dep :: s_send_in_deps :: s_send_out_dep :: s_send_out_deps :: Nil = Enum(11)
  val state = RegInit(s_idle)

  // Picos v4
  val do_v4_initiate_task            = io.cmd.bits.inst.funct === 8.U 
  val do_v4_add_args_and_parent_info = io.cmd.bits.inst.funct === 9.U 
  val do_v4_send_in_dep              = io.cmd.bits.inst.funct === 10.U
  val do_v4_send_in_deps             = io.cmd.bits.inst.funct === 11.U
  val do_v4_send_out_dep             = io.cmd.bits.inst.funct === 12.U
  val do_v4_send_out_deps            = io.cmd.bits.inst.funct === 13.U

  // Picos v1
  val doNFetchWork  = io.cmd.bits.inst.funct === 2.U
  val doRetire      = io.cmd.bits.inst.funct === 3.U
  val doPFetchWork  = io.cmd.bits.inst.funct === 4.U
  val doWorkRequest = io.cmd.bits.inst.funct === 6.U

  val flag_success = 0.U
  val flag_unimplemented = 1.U
  val flag_error = Fill(64, 1.U)

  io.cmd.ready := state === s_idle
  io.busy := state =/= s_idle

  io.resp.bits.data := flag_error
  io.resp.valid := false.B

  val registers = RegInit(VecInit(Seq.fill(2)(0.U(64.W))))
  val req_rd = RegInit(0.U(5.W))
  io.resp.bits.rd := req_rd

  when (io.cmd.fire()) {
    req_rd := io.cmd.bits.inst.rd
    registers(0) := io.cmd.bits.rs1
    registers(1) := io.cmd.bits.rs2

    when (do_v4_initiate_task) {
      state := s_initiate_task
    } .elsewhen (do_v4_add_args_and_parent_info) {
      state := s_add_args_and_parent_info
    } .elsewhen (do_v4_send_in_dep) {
      state := s_send_in_dep
    } .elsewhen (do_v4_send_in_deps) {
      state := s_send_in_deps
    } .elsewhen (do_v4_send_out_dep) {
      state := s_send_out_dep
    } .elsewhen (do_v4_send_out_deps) {
      state := s_send_out_deps
    } .elsewhen (doNFetchWork) {
      state := s_nanosid_fetch_work
    } .elsewhen (doRetire) {
      state := s_retiring
    } .elsewhen (doPFetchWork) {
      state := s_picosid_fetch_work
    } .elsewhen (doWorkRequest) {
      state := s_work_request
    }
  }

  io.argsAndParentInfo.valid := false.B
  io.argsAndParentInfo.bits.parent_id := 0.U
  io.argsAndParentInfo.bits.num_args := 0.U

  io.taskInfo.valid := false.B
  io.taskInfo.bits.task_id := 0.U
  io.taskInfo.bits.num_deps := 0.U

  io.depInfo.valid := false.B
  io.depInfo.bits.map{case (dep) =>
      dep.pointer := 0.U
      dep.kind := 0.U
  }

  when (state === s_initiate_task) {
      io.resp.valid := true.B
      state := s_idle

      io.taskInfo.bits.task_id := registers(0)
      io.taskInfo.bits.num_deps := registers(1)
      io.taskInfo.valid := true.B

      io.resp.bits.data := Mux(io.taskInfo.fire(), flag_success, flag_error)
  }

  when (state === s_add_args_and_parent_info) {
      io.resp.valid := true.B
      state := s_idle

      io.argsAndParentInfo.bits.parent_id := registers(0)
      io.argsAndParentInfo.bits.num_args := registers(1)
      io.argsAndParentInfo.valid := true.B

      io.resp.bits.data := Mux(io.argsAndParentInfo.fire(), flag_success, flag_error)
  }

  when (state === s_send_in_dep) {
      io.resp.valid := true.B
      state := s_idle

      io.depInfo.bits(0).pointer := registers(0)
      io.depInfo.bits(0).kind := 1.U
      io.depInfo.bits(1).kind := 0.U
      io.depInfo.valid := true.B

      io.resp.bits.data := Mux(io.depInfo.fire(), flag_success, flag_error)
  }

  when (state === s_send_in_deps) {
      io.resp.valid := true.B
      state := s_idle

      io.depInfo.bits(0).pointer := registers(0)
      io.depInfo.bits(0).kind := 1.U
      io.depInfo.bits(1).pointer := registers(1)
      io.depInfo.bits(1).kind := 1.U
      io.depInfo.valid := true.B

      io.resp.bits.data := Mux(io.depInfo.fire(), flag_success, flag_error)
  }

  when (state === s_send_out_dep) {
      io.resp.valid := true.B
      state := s_idle

      io.depInfo.bits(0).pointer := registers(0)
      io.depInfo.bits(0).kind := 2.U
      io.depInfo.bits(1).kind := 0.U
      io.depInfo.valid := true.B

      io.resp.bits.data := Mux(io.depInfo.fire(), flag_success, flag_error)
  }

  when (state === s_send_out_deps) {
      io.resp.valid := true.B
      state := s_idle

      io.depInfo.bits(0).pointer := registers(0)
      io.depInfo.bits(0).kind := 2.U
      io.depInfo.bits(1).pointer := registers(1)
      io.depInfo.bits(1).kind := 2.U
      io.depInfo.valid := true.B

      io.resp.bits.data := Mux(io.depInfo.fire(), flag_success, flag_error)
  }

  val retirementCount = RegInit(0.U(64.W))

  // Implements retirement
  io.retInterface.bits  := registers(0)

  when (state === s_retiring) {
    io.retInterface.valid := true.B
  }

  when (io.retInterface.fire()) {
    state := s_idle
    io.resp.valid := true.B
    io.resp.bits.data := flag_success
    retirementCount := retirementCount + 1.U(64.W)
  }
  
  // Implements work request
  
  val pendingRequestsCounter  = RegInit(0.U(64.W))
  val userReadNanosID         = RegInit(false.B)
  val workFetchingError       = Fill(64, 1.U)
  val thereIsReadyPacket      = io.readyQInterface.valid
  val increasedCounterVal     = pendingRequestsCounter + 1.U
  val decreasedCounterVal     = pendingRequestsCounter - 1.U

  val wr_success                  = io.readyRequestInterface.fire()

  io.readyRequestInterface.valid  := (state === s_work_request)

  when (state === s_work_request) {
    io.resp.valid := true.B
    io.resp.bits.data          := Mux(wr_success, increasedCounterVal, pendingRequestsCounter)
    pendingRequestsCounter     := Mux(wr_success, increasedCounterVal, pendingRequestsCounter)
    state                      := s_idle
  }
  
  // Implements NanosID work-fetching

  val nanosID             =   io.readyQInterface.bits.nanosID
  val nwfi_success        =   thereIsReadyPacket && !userReadNanosID

  when (state === s_nanosid_fetch_work) {
    io.resp.valid := true.B
    when (nwfi_success)        { userReadNanosID := true.B }

    io.resp.bits.data          := Mux(nwfi_success, nanosID, workFetchingError)
    state                      := s_idle
  }

  // Implements PicosID work-fetching

  val picosID             =   io.readyQInterface.bits.picosID
  val pwfi_success        =   thereIsReadyPacket && userReadNanosID

  when (state === s_picosid_fetch_work) {
    io.resp.valid := true.B

    when (pwfi_success) {
      pendingRequestsCounter  := decreasedCounterVal
      userReadNanosID         := false.B
    }

    io.resp.bits.data         := Mux(pwfi_success, picosID, workFetchingError)
    state                     := s_idle
  }

  io.readyQInterface.ready  := (state === s_picosid_fetch_work && pwfi_success)
}
