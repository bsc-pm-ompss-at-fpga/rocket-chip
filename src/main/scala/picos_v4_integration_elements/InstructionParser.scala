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

  // TODO: Remove this when the signals get fully initialized
  io.subQInterface.bits := 0.U
  io.subQInterface.valid := 0.U
  io.pmException := 0.U
  io.readyRequestInterface.bits := 0.U
  io.retInterface.bits := 0.U
  io.readyQInterface.ready := 0.U
  io.readyRequestInterface.valid := 0.U
  io.subRequest.valid := 0.U
  io.subRequest.bits.numPackets := 0.U
  io.interrupt := 0.U
  io.subRequest.bits.arbID := 0.U
  io.tag := 0.U
  io.retInterface.valid := 0.U


  val s_idle :: s_initiate_task :: s_add_args_and_parent_info :: s_send_in_dep :: s_send_in_deps :: s_send_out_dep :: s_send_out_deps :: Nil = Enum(7)
  val state = RegInit(s_idle)

  val do_v4_initiate_task            = io.cmd.bits.inst.funct === 8.U 
  val do_v4_add_args_and_parent_info = io.cmd.bits.inst.funct === 9.U 
  val do_v4_send_in_dep              = io.cmd.bits.inst.funct === 10.U
  val do_v4_send_in_deps             = io.cmd.bits.inst.funct === 11.U
  val do_v4_send_out_dep             = io.cmd.bits.inst.funct === 12.U
  val do_v4_send_out_deps            = io.cmd.bits.inst.funct === 13.U

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

      io.resp.bits.data := Mux(io.taskInfo.fire, flag_success, flag_error)
  }

  when (state === s_add_args_and_parent_info) {
      io.resp.valid := true.B
      state := s_idle

      io.argsAndParentInfo.bits.parent_id := registers(0)
      io.argsAndParentInfo.bits.num_args := registers(1)
      io.argsAndParentInfo.valid := true.B

      io.resp.bits.data := Mux(io.argsAndParentInfo.fire, flag_success, flag_error)
  }

  when (state === s_send_in_dep) {
      io.resp.valid := true.B
      state := s_idle

      io.depInfo.bits(0).pointer := registers(0)
      io.depInfo.bits(0).kind := 1.U
      io.depInfo.bits(1).kind := 0.U
      io.depInfo.valid := true.B

      io.resp.bits.data := Mux(io.depInfo.fire, flag_success, flag_error)
  }

  when (state === s_send_in_deps) {
      io.resp.valid := true.B
      state := s_idle

      io.depInfo.bits(0).pointer := registers(0)
      io.depInfo.bits(0).kind := 1.U
      io.depInfo.bits(1).pointer := registers(1)
      io.depInfo.bits(1).kind := 1.U
      io.depInfo.valid := true.B

      io.resp.bits.data := Mux(io.depInfo.fire, flag_success, flag_error)
  }

  when (state === s_send_out_dep) {
      io.resp.valid := true.B
      state := s_idle

      io.depInfo.bits(0).pointer := registers(0)
      io.depInfo.bits(0).kind := 2.U
      io.depInfo.bits(1).kind := 0.U
      io.depInfo.valid := true.B

      io.resp.bits.data := Mux(io.depInfo.fire, flag_success, flag_error)
  }

  when (state === s_send_out_deps) {
      io.resp.valid := true.B
      state := s_idle

      io.depInfo.bits(0).pointer := registers(0)
      io.depInfo.bits(0).kind := 2.U
      io.depInfo.bits(1).pointer := registers(1)
      io.depInfo.bits(1).kind := 2.U
      io.depInfo.valid := true.B

      io.resp.bits.data := Mux(io.depInfo.fire, flag_success, flag_error)
  }
  
  //////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////
  // All the remaining of this class implementation is from the Picos v1 era
  //////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////
  
  /*
  // This register captures the index of the target
  // result register
  // TODO: CHECK: MODIFIED FROM REG TO REGINIT IN 2021
  val req_rd = RegInit(io.resp.bits.rd)

  val s_idle :: s_work_request :: s_nanosid_fetch_work :: s_picosid_fetch_work :: s_submitting :: s_retiring :: s_sub_request :: s_submitting_three :: Nil = Enum(8)
  //val state_v1 = Reg(init = s_idle)
  val state_v1 = RegInit(s_idle)
  val busyWithSubmitThree = RegInit(false.B)

  val subRequestCount = RegInit(0.U(64.W))
  val retirementCount = RegInit(0.U(64.W))

  // Do not generate interrupts
  io.interrupt := false.B

  // For detecting the operation to perform
  val doNFetchWork  = io.cmd.bits.inst.funct === 2.U
  val doRetire      = io.cmd.bits.inst.funct === 3.U
  val doPFetchWork  = io.cmd.bits.inst.funct === 4.U
  val doWorkRequest = io.cmd.bits.inst.funct === 6.U

  // Let the registers reset to 0
  //val x = Reg(UInt(width = xLen), init = UInt(0))
  //val x = RegInit(UInt(value = 0, width = xLen))
  val x = RegInit(0.asUInt(xLen.W))
  //val tag = Reg(UInt(width = xLen), init = UInt(0))
  //val tag = RegInit(UInt(value = 0, width = xLen))
  val tag = RegInit(0.asUInt(xLen.W))

  // The module is always ready to receive data
  io.cmd.ready  := (state_v1 === s_idle)

  // Tautology that says that we are
  // busy whenever we are not idle
  io.busy       := (state_v1 =/= s_idle)

  // Get input when the command is fired
  when (io.cmd.fire()) {
    req_rd            := io.cmd.bits.inst.rd
    x                 := io.cmd.bits.rs1
    tag               := io.cmd.bits.rs2

    when (doNFetchWork) {
      state_v1 := s_nanosid_fetch_work
    } .elsewhen (doRetire) {
      state_v1 := s_retiring
    } .elsewhen (doPFetchWork) {
      state_v1 := s_picosid_fetch_work
    } .elsewhen (doWorkRequest) {
      state_v1 := s_work_request
    }
  }

  // Implements work-fetching
  val pendingRequestsCounter  = RegInit(0.U(xLen.W))
  val userReadNanosID         = RegInit(false.B)
  val workFetchingError       = Fill(64, 1.U)
  val thereIsReadyPacket      = io.readyQInterface.valid
  val increasedCounterVal     = pendingRequestsCounter + 1.U
  val decreasedCounterVal     = pendingRequestsCounter - 1.U

  io.readyQInterface.ready  := (state_v1 === s_picosid_fetch_work && pwfi_success)

    // Implements work request
      io.tag := tag

      val wr_valid                    = (state_v1 === s_work_request)
      val wr_success                  = io.readyRequestInterface.fire()

      io.readyRequestInterface.valid  := (state_v1 === s_work_request)

      when (state_v1 === s_work_request) {
        io.resp.bits.data          := Mux(wr_success, increasedCounterVal, pendingRequestsCounter)
        pendingRequestsCounter     := Mux(wr_success, increasedCounterVal, pendingRequestsCounter)
        state_v1                      := s_idle
      }

    // Implements NanosID work-fetching
      io.tag := tag

      lazy val nanosID             =   io.readyQInterface.bits.nanosID
      lazy val nwfi_valid          =   (state_v1 === s_nanosid_fetch_work)
      lazy val nwfi_success        =   thereIsReadyPacket && !userReadNanosID

      when (state_v1 === s_nanosid_fetch_work) {
        when (nwfi_success)        { userReadNanosID := true.B }

        io.resp.bits.data          := Mux(nwfi_success, nanosID, workFetchingError)
        state_v1                      := s_idle
      }

    // Implements PicosID work-fetching
      io.tag := tag

      lazy val picosID             =   io.readyQInterface.bits.picosID
      lazy val pwfi_valid          =   (state_v1 === s_picosid_fetch_work)
      lazy val pwfi_success        =   thereIsReadyPacket && userReadNanosID

      when (state_v1 === s_picosid_fetch_work) {
        when (pwfi_success) {
          pendingRequestsCounter  := decreasedCounterVal
          userReadNanosID         := false.B
        }

        io.resp.bits.data         := Mux(pwfi_success, picosID, workFetchingError)
        state_v1                     := s_idle
      }

  // Implements retirement
    io.retInterface.bits  := x
    io.retInterface.valid := (state_v1 === s_retiring)
    when (io.retInterface.fire()) {
      state_v1 := s_idle
      retirementCount := retirementCount + 1.U(64.W)
    }

  // Write data to the reply register if
  // any rd-writing instruction has been
  // issued
  io.resp.valid           := wr_valid || nwfi_valid || pwfi_valid || sub3_valid || sub_valid || sub_req_valid

  // The register to write to should
  // always be the one chosen by the
  // user
  io.resp.bits.rd     := req_rd
  */
}
