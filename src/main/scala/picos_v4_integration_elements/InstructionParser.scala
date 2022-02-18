package picos_v4_integration_elements

import chisel3._
import chisel3.util._

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

/*
 * Kind options:
 *  0.U: To be discarded
 *  1.U: In
 *  2.U: Out
 *  3.U: Inout
 */

class InstructionParser extends Module {
	val io = IO(new Bundle {
		val funct                     = Input(UInt(7.W))
		val rd_id                     = Input(UInt(5.W))
		val rs1_bits                  = Input(UInt(64.W))
		val rs2_bits                  = Input(UInt(64.W))
		val resp_bits                 = Output(UInt(64.W))
    val cmd_valid                 = Input(Bool())
    val cmd_ready                 = Output(Bool())
    val resp_valid                = Output(Bool())
    val argsAndParentInfo         = Decoupled(new ArgsAndParentInfo)
    val taskInfo                  = Decoupled(new TaskInfo)
    val depInfo                   = Decoupled(Vec(2, new DepInfo))
	})

  val s_idle :: s_initiate_task :: s_add_args_and_parent_info :: s_send_in_dep :: s_send_in_deps :: s_send_out_dep :: s_send_out_deps :: Nil = Enum(7)
  val state = RegInit(s_idle)

  val do_v4_initiate_task            = io.funct === 8.U 
  val do_v4_add_args_and_parent_info = io.funct === 9.U 
  val do_v4_send_in_dep              = io.funct === 10.U
  val do_v4_send_in_deps             = io.funct === 11.U
  val do_v4_send_out_dep             = io.funct === 12.U
  val do_v4_send_out_deps            = io.funct === 13.U

  val flag_success = 0.U
  val flag_unimplemented = 1.U
  val flag_error = Fill(64, 1.U)

  io.cmd_ready := state === s_idle
  io.resp_valid := false.B
  io.resp_bits := flag_error

  val cmd_fire = io.cmd_ready && io.cmd_valid
  val registers = RegInit(VecInit(Seq.fill(2)(0.U(64.W))))

  when (cmd_fire) {
    registers(0) := io.rs1_bits
    registers(1) := io.rs2_bits

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
      io.resp_valid := true.B
      state := s_idle

      io.taskInfo.bits.task_id := registers(0)
      io.taskInfo.bits.num_deps := registers(1)
      io.taskInfo.valid := true.B

      io.resp_bits := Mux(io.taskInfo.fire, flag_success, flag_error)
  }

  when (state === s_add_args_and_parent_info) {
      io.resp_valid := true.B
      state := s_idle

      io.argsAndParentInfo.bits.parent_id := registers(0)
      io.argsAndParentInfo.bits.num_args := registers(1)
      io.argsAndParentInfo.valid := true.B

      io.resp_bits := Mux(io.argsAndParentInfo.fire, flag_success, flag_error)
  }

  when (state === s_send_in_dep) {
      io.resp_valid := true.B
      state := s_idle

      io.depInfo.bits(0).pointer := registers(0)
      io.depInfo.bits(0).kind := 1.U
      io.depInfo.bits(1).kind := 0.U
      io.depInfo.valid := true.B

      io.resp_bits := Mux(io.depInfo.fire, flag_success, flag_error)
  }

  when (state === s_send_in_deps) {
      io.resp_valid := true.B
      state := s_idle

      io.depInfo.bits(0).pointer := registers(0)
      io.depInfo.bits(0).kind := 1.U
      io.depInfo.bits(1).pointer := registers(1)
      io.depInfo.bits(1).kind := 1.U
      io.depInfo.valid := true.B

      io.resp_bits := Mux(io.depInfo.fire, flag_success, flag_error)
  }

  when (state === s_send_out_dep) {
      io.resp_valid := true.B
      state := s_idle

      io.depInfo.bits(0).pointer := registers(0)
      io.depInfo.bits(0).kind := 2.U
      io.depInfo.bits(1).kind := 0.U
      io.depInfo.valid := true.B

      io.resp_bits := Mux(io.depInfo.fire, flag_success, flag_error)
  }

  when (state === s_send_out_deps) {
      io.resp_valid := true.B
      state := s_idle

      io.depInfo.bits(0).pointer := registers(0)
      io.depInfo.bits(0).kind := 2.U
      io.depInfo.bits(1).pointer := registers(1)
      io.depInfo.bits(1).kind := 2.U
      io.depInfo.valid := true.B

      io.resp_bits := Mux(io.depInfo.fire, flag_success, flag_error)
  }
}
