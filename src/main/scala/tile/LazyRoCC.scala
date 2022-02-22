// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import chisel3._
import chisel3.util._
import chisel3.util.HasBlackBoxResource
import chisel3.experimental.IntParam
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{InOrderArbiter, RoutingInfo}
import freechips.rocketchip.integrators.ReadyPacket

import java.io._
import freechips.rocketchip.subsystem.RocketSubsystem
import freechips.rocketchip.integrators._
import freechips.rocketchip.prci.DelegateIO

import picos_v4_integration_elements._

import freechips.rocketchip.system.{BaseConfig, RoccExampleConfig}

case object BuildRoCC extends Field[Seq[Parameters => LazyRoCC]](Nil)

class RoCCInstruction extends Bundle {
  val funct = Bits(7.W)
  val rs2 = Bits(5.W)
  val rs1 = Bits(5.W)
  val xd = Bool()
  val xs1 = Bool()
  val xs2 = Bool()
  val rd = Bits(5.W)
  val opcode = Bits(7.W)
}

class RoCCCommand extends Bundle {
  val inst = new RoCCInstruction
  val rs1 = Bits(64.W)
  val rs2 = Bits(64.W)
  val status = new MStatus
}

class RoCCResponse extends Bundle {
  val rd = Bits(5.W)
  val data = Bits(64.W)
}

trait EssentialRoCCCoreIO extends Bundle {
  val cmd = Flipped(Decoupled(new RoCCCommand))
  val resp = Decoupled(new RoCCResponse)
  val busy = Output(Bool())
  val interrupt = Output(Bool())
  val exception = Input(Bool())
}

class RoCCCoreIO(implicit p: Parameters) extends CoreBundle()(p) with EssentialRoCCCoreIO {
  val mem = new HellaCacheIO
}

trait PicosRelatedSignals extends Bundle {
  val argsAndParentInfo         = Decoupled(new ArgsAndParentInfo)
  val taskInfo                  = Decoupled(new TaskInfo)
  val depInfo                   = Decoupled(Vec(2, new DepInfo))

  //TODO: MAKE IT CLEANER AND BETTER FITTING
  //      TO THE REST OF THE PARAMETRIZABLE
  //      IO INFRASTRUCTURE
  // FIXME Let routing info get information about the number of cores
  // from elsewhere!
  // FIXME Let tagWidth be set based on the number of cores!
  val tag                     = Output(UInt(10.W))
  val subQInterface           = Decoupled(UInt(32.W))
  val subRequest              = Decoupled(RoutingInfo(64, 48))
  val retInterface            = Decoupled(UInt(64.W))
  val readyRequestInterface   = Decoupled(UInt(0.W))
  val readyQInterface         = Flipped(Decoupled(new ReadyPacket))
  val pmException             = Output(UInt(4.W))
}

class RoCCIO(val nPTWPorts: Int)(implicit p: Parameters) extends RoCCCoreIO()(p) with PicosRelatedSignals {
  val ptw = Vec(nPTWPorts, new TLBPTWIO)
  val fpu_req = Decoupled(new FPInput)
  val fpu_resp = Flipped(Decoupled(new FPResult))
}

class CoreSubmissionUnit extends Bundle {
  val packet0                     = UInt(32.W)
  val packet1                     = UInt(32.W)
  val packet2                     = UInt(32.W)
}

class SubmissionUnit extends Bundle {
  val packet0                     = UInt(32.W)
  val packet1                     = UInt(32.W)
  val packet2                     = UInt(32.W)
}

/** Base classes for Diplomatic TL2 RoCC units **/
abstract class LazyRoCC(
      val opcodes: OpcodeSet,
      val nPTWPorts: Int = 0,
      val usesFPU: Boolean = false
    )(implicit p: Parameters) extends LazyModule {
  val module: LazyRoCCModuleImp
  val atlNode: TLNode = TLIdentityNode()
  val tlNode: TLNode = TLIdentityNode()
  var talksToPicos: Boolean = false
}

class LazyRoCCModuleImp(outer: LazyRoCC) extends LazyModuleImp(outer) {
  val io = IO(new RoCCIO(outer.nPTWPorts))
}

/** Mixins for including RoCC **/

trait HasLazyRoCC extends CanHavePTW { this: BaseTile =>
  val roccs = p(BuildRoCC).map(_(p))

  roccs.map(_.atlNode).foreach { atl => tlMasterXbar.node :=* atl }
  roccs.map(_.tlNode).foreach { tl => tlOtherMastersNode :=* tl }

  nPTWPorts += roccs.map(_.nPTWPorts).sum
  nDCachePorts += roccs.size
}

trait HasLazyRoCCModule extends CanHavePTWModule
    with HasCoreParameters { this: RocketTileModuleImp with HasFpuOpt =>
  
val delegate = IO(new DelegateIO)

	dontTouch(delegate)
	

  val accsTalkingToPicos = outer.roccs.filter(_.talksToPicos)
  require(accsTalkingToPicos.length == 1, "More than one LazyRoCC was found to be talking to Picos")
  println(s"We found ${accsTalkingToPicos.length} RoCC accelerators talking to Picos Manager in this Tile")
  val picosMate = accsTalkingToPicos(0)

  delegate.subQInterface <> picosMate.module.io.subQInterface
  delegate.subRequest <> picosMate.module.io.subRequest
  delegate.retInterface <> picosMate.module.io.retInterface
  delegate.readyRequestInterface <> picosMate.module.io.readyRequestInterface
  delegate.tag := picosMate.module.io.tag
  picosMate.module.io.readyQInterface <> delegate.readyQInterface
  delegate.pmException := picosMate.module.io.pmException
  
  val tileId = outer.obviousId

  val (respArb, cmdRouter) = if(outer.roccs.nonEmpty) {
    val respArb = Module(new RRArbiter(new RoCCResponse, outer.roccs.size))
    val cmdRouter = Module(new RoccCommandRouter(outer.roccs.map(_.opcodes))(outer.p))
    outer.roccs.zipWithIndex.foreach { case (rocc, i) =>
      rocc.module.io.ptw ++=: ptwPorts
      rocc.module.io.cmd <> cmdRouter.io.out(i)
      val dcIF = Module(new SimpleHellaCacheIF()(outer.p))
      dcIF.io.requestor <> rocc.module.io.mem
      dcachePorts += dcIF.io.cache
      respArb.io.in(i) <> Queue(rocc.module.io.resp)
    }

    fpuOpt foreach { fpu =>
      val nFPUPorts = outer.roccs.count(_.usesFPU)
      if (usingFPU && nFPUPorts > 0) {
        val fpArb = Module(new InOrderArbiter(new FPInput()(outer.p), new FPResult()(outer.p), nFPUPorts))
        val fp_rocc_ios = outer.roccs.filter(_.usesFPU).map(_.module.io)
        fpArb.io.in_req <> fp_rocc_ios.map(_.fpu_req)
        fp_rocc_ios.zip(fpArb.io.in_resp).foreach {
          case (rocc, arb) => rocc.fpu_resp <> arb
        }
        fpu.io.cp_req <> fpArb.io.out_req
        fpArb.io.out_resp <> fpu.io.cp_resp
      } else {
        fpu.io.cp_req.valid := false.B
        fpu.io.cp_resp.ready := false.B
      }
    }
    (Some(respArb), Some(cmdRouter))
  } else {
    (None, None)
  }
}

class AccumulatorExample(opcodes: OpcodeSet, val n: Int = 4)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new AccumulatorExampleModuleImp(this)
}

class AccumulatorExampleModuleImp(outer: AccumulatorExample)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {
  val regfile = Mem(outer.n, UInt(xLen.W))
  val busy = RegInit(VecInit(Seq.fill(outer.n){false.B}))

  val cmd = Queue(io.cmd)
  val funct = cmd.bits.inst.funct
  val addr = cmd.bits.rs2(log2Up(outer.n)-1,0)
  val doWrite = funct === 0.U
  val doRead = funct === 1.U
  val doLoad = funct === 2.U
  val doAccum = funct === 3.U
  val memRespTag = io.mem.resp.bits.tag(log2Up(outer.n)-1,0)

  // datapath
  val addend = cmd.bits.rs1
  val accum = regfile(addr)
  val wdata = Mux(doWrite, addend, accum + addend)

  when (cmd.fire() && (doWrite || doAccum)) {
    regfile(addr) := wdata
  }

  when (io.mem.resp.valid) {
    regfile(memRespTag) := io.mem.resp.bits.data
    busy(memRespTag) := false.B
  }

  // control
  when (io.mem.req.fire()) {
    busy(addr) := true.B
  }

  val doResp = cmd.bits.inst.xd
  val stallReg = busy(addr)
  val stallLoad = doLoad && !io.mem.req.ready
  val stallResp = doResp && !io.resp.ready

  cmd.ready := !stallReg && !stallLoad && !stallResp
    // command resolved if no stalls AND not issuing a load that will need a request

  // PROC RESPONSE INTERFACE
  io.resp.valid := cmd.valid && doResp && !stallReg && !stallLoad
    // valid response if valid command, need a response, and no stalls
  io.resp.bits.rd := cmd.bits.inst.rd
    // Must respond with the appropriate tag or undefined behavior
  io.resp.bits.data := accum
    // Semantics is to always send out prior accumulator register value

  io.busy := cmd.valid || busy.reduce(_||_)
    // Be busy when have pending memory requests or committed possibility of pending requests
  io.interrupt := false.B
    // Set this true to trigger an interrupt on the processor (please refer to supervisor documentation)

  // MEMORY REQUEST INTERFACE
  io.mem.req.valid := cmd.valid && doLoad && !stallReg && !stallResp
  io.mem.req.bits.addr := addend
  io.mem.req.bits.tag := addr
  io.mem.req.bits.cmd := M_XRD // perform a load (M_XWR for stores)
  io.mem.req.bits.size := log2Ceil(8).U
  io.mem.req.bits.signed := false.B
  io.mem.req.bits.data := 0.U // we're not performing any stores...
  io.mem.req.bits.phys := false.B
  io.mem.req.bits.dprv := cmd.bits.status.dprv
}

class  TranslatorExample(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes, nPTWPorts = 1) {
  override lazy val module = new TranslatorExampleModuleImp(this)
}

class TranslatorExampleModuleImp(outer: TranslatorExample)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {
  val req_addr = Reg(UInt(coreMaxAddrBits.W))
  val req_rd = Reg(chiselTypeOf(io.resp.bits.rd))
  val req_offset = req_addr(pgIdxBits - 1, 0)
  val req_vpn = req_addr(coreMaxAddrBits - 1, pgIdxBits)
  val pte = Reg(new PTE)

  val s_idle :: s_ptw_req :: s_ptw_resp :: s_resp :: Nil = Enum(4)
  val state = RegInit(s_idle)

  io.cmd.ready := (state === s_idle)

  when (io.cmd.fire()) {
    req_rd := io.cmd.bits.inst.rd
    req_addr := io.cmd.bits.rs1
    state := s_ptw_req
  }

  private val ptw = io.ptw(0)

  when (ptw.req.fire()) { state := s_ptw_resp }

  when (state === s_ptw_resp && ptw.resp.valid) {
    pte := ptw.resp.bits.pte
    state := s_resp
  }

  when (io.resp.fire()) { state := s_idle }

  ptw.req.valid := (state === s_ptw_req)
  ptw.req.bits.valid := true.B
  ptw.req.bits.bits.addr := req_vpn

  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := req_rd
  io.resp.bits.data := Mux(pte.leaf(), Cat(pte.ppn, req_offset), -1.S(xLen.W).asUInt)

  io.busy := (state =/= s_idle)
  io.interrupt := false.B
  io.mem.req.valid := false.B
}

class  CharacterCountExample(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new CharacterCountExampleModuleImp(this)
  override val atlNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("CharacterCountRoCC")))))
}

class CharacterCountExampleModuleImp(outer: CharacterCountExample)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
  with HasCoreParameters
  with HasL1CacheParameters {
  val cacheParams = tileParams.icache.get

  private val blockOffset = blockOffBits
  private val beatOffset = log2Up(cacheDataBits/8)

  val needle = Reg(UInt(8.W))
  val addr = Reg(UInt(coreMaxAddrBits.W))
  val count = Reg(UInt(xLen.W))
  val resp_rd = Reg(chiselTypeOf(io.resp.bits.rd))

  val addr_block = addr(coreMaxAddrBits - 1, blockOffset)
  val offset = addr(blockOffset - 1, 0)
  val next_addr = (addr_block + 1.U) << blockOffset.U

  val s_idle :: s_acq :: s_gnt :: s_check :: s_resp :: Nil = Enum(5)
  val state = RegInit(s_idle)

  val (tl_out, edgesOut) = outer.atlNode.out(0)
  val gnt = tl_out.d.bits
  val recv_data = Reg(UInt(cacheDataBits.W))
  val recv_beat = RegInit(0.U(log2Up(cacheDataBeats+1).W))

  val data_bytes = VecInit(Seq.tabulate(cacheDataBits/8) { i => recv_data(8 * (i + 1) - 1, 8 * i) })
  val zero_match = data_bytes.map(_ === 0.U)
  val needle_match = data_bytes.map(_ === needle)
  val first_zero = PriorityEncoder(zero_match)

  val chars_found = PopCount(needle_match.zipWithIndex.map {
    case (matches, i) =>
      val idx = Cat(recv_beat - 1.U, i.U(beatOffset.W))
      matches && idx >= offset && i.U <= first_zero
  })
  val zero_found = zero_match.reduce(_ || _)
  val finished = Reg(Bool())

  io.cmd.ready := (state === s_idle)
  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := resp_rd
  io.resp.bits.data := count
  tl_out.a.valid := (state === s_acq)
  tl_out.a.bits := edgesOut.Get(
                       fromSource = 0.U,
                       toAddress = addr_block << blockOffset,
                       lgSize = lgCacheBlockBytes.U)._2
  tl_out.d.ready := (state === s_gnt)

  when (io.cmd.fire()) {
    addr := io.cmd.bits.rs1
    needle := io.cmd.bits.rs2
    resp_rd := io.cmd.bits.inst.rd
    count := 0.U
    finished := false.B
    state := s_acq
  }

  when (tl_out.a.fire()) { state := s_gnt }

  when (tl_out.d.fire()) {
    recv_beat := recv_beat + 1.U
    recv_data := gnt.data
    state := s_check
  }

  when (state === s_check) {
    when (!finished) {
      count := count + chars_found
    }
    when (zero_found) { finished := true.B }
    when (recv_beat === cacheDataBeats.U) {
      addr := next_addr
      state := Mux(zero_found || finished, s_resp, s_acq)
    } .otherwise {
      state := s_gnt
    }
  }

  when (io.resp.fire()) { state := s_idle }

  io.busy := (state =/= s_idle)
  io.interrupt := false.B
  io.mem.req.valid := false.B
  // Tie off unused channels
  tl_out.b.ready := true.B
  tl_out.c.valid := false.B
  tl_out.e.valid := false.B
}

class BlackBoxExample(opcodes: OpcodeSet, blackBoxFile: String)(implicit p: Parameters)
    extends LazyRoCC(opcodes) {
  override lazy val module = new BlackBoxExampleModuleImp(this, blackBoxFile)
}

class BlackBoxExampleModuleImp(outer: BlackBoxExample, blackBoxFile: String)(implicit p: Parameters)
    extends LazyRoCCModuleImp(outer)
    with RequireSyncReset
    with HasCoreParameters {

  val blackbox = {
    val roccIo = io
    Module(
      new BlackBox( Map( "xLen" -> IntParam(xLen),
                         "PRV_SZ" -> IntParam(PRV.SZ),
                         "coreMaxAddrBits" -> IntParam(coreMaxAddrBits),
                         "dcacheReqTagBits" -> IntParam(roccIo.mem.req.bits.tag.getWidth),
                         "M_SZ" -> IntParam(M_SZ),
                         "mem_req_bits_size_width" -> IntParam(roccIo.mem.req.bits.size.getWidth),
                         "coreDataBits" -> IntParam(coreDataBits),
                         "coreDataBytes" -> IntParam(coreDataBytes),
                         "paddrBits" -> IntParam(paddrBits),
                         "FPConstants_RM_SZ" -> IntParam(FPConstants.RM_SZ),
                         "fLen" -> IntParam(fLen),
                         "FPConstants_FLAGS_SZ" -> IntParam(FPConstants.FLAGS_SZ)
                   ) ) with HasBlackBoxResource {
        val io = IO( new Bundle {
                      val clock = Input(Clock())
                      val reset = Input(Reset())
                      val rocc = chiselTypeOf(roccIo)
                    })
        override def desiredName: String = blackBoxFile
        addResource(s"/vsrc/$blackBoxFile.v")
      }
    )
  }

  blackbox.io.clock := clock
  blackbox.io.reset := reset
  blackbox.io.rocc.cmd <> io.cmd
  io.resp <> blackbox.io.rocc.resp
  io.mem <> blackbox.io.rocc.mem
  io.busy := blackbox.io.rocc.busy
  io.interrupt := blackbox.io.rocc.interrupt
  blackbox.io.rocc.exception := io.exception
  io.ptw <> blackbox.io.rocc.ptw
  io.fpu_req <> blackbox.io.rocc.fpu_req
  blackbox.io.rocc.fpu_resp <> io.fpu_resp

}

class OpcodeSet(val opcodes: Seq[UInt]) {
  def |(set: OpcodeSet) =
    new OpcodeSet(this.opcodes ++ set.opcodes)

  def matches(oc: UInt) = opcodes.map(_ === oc).reduce(_ || _)
}

object OpcodeSet {
  def custom0 = new OpcodeSet(Seq("b0001011".U))
  def custom1 = new OpcodeSet(Seq("b0101011".U))
  def custom2 = new OpcodeSet(Seq("b1011011".U))
  def custom3 = new OpcodeSet(Seq("b1111011".U))
  def all = custom0 | custom1 | custom2 | custom3
}

case object BlankField extends Field[Boolean]

class BlankConfig extends Config((site, here, up) => {
        case BlankField => false
})

class LazyRoccTestingExtras extends Config((site, here, up) => {
  //case TileKey => 0
  case TileKey => new RocketTileParams
  //case TileVisibilityNodeKey => TLEphemeralNode()(ValName("tile_master"))
  case TileVisibilityNodeKey => TLTempNode()
  //case TileVisibilityNodeKey => TLEphemeralNode()(ValName("tile_master"))
})

object FastCompileCoreRealPicosManager  {
        def main(args: Array[String]): Unit = {
                implicit val p: Parameters = new BlankConfig
                var verilog = stage.ChiselStage.emitVerilog(  new freechips.rocketchip.integrators.CoreRealPicosManager(2, 16)(p)  )
                val pw = new PrintWriter(new File("/tmp/source.v" ))
                pw.write(verilog)
                pw.close
        }
}

class FastCompilationConfig extends Config (
  new RoccExampleConfig ++
  new LazyRoccTestingExtras
)

//FIXME: The following code still fails to provide
//       enough configuration context for compiling
//       'bogus' in isolation from the rest of the
//       system
object FastCompileBogusModule  {
        def main(args: Array[String]): Unit = {
                //implicit val p: Parameters = new BlankConfig
                implicit val p: Parameters = new FastCompilationConfig
                implicit val valName = ValName("derp")
                var bogusInstance = LazyModule(new bogus(OpcodeSet.custom2)(p))
                var verilog = stage.ChiselStage.emitVerilog(  bogusInstance.module  )
                val pw = new PrintWriter(new File("/tmp/source.v" ))
                pw.write(verilog)
                pw.close
        }
}

class bogus(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  talksToPicos = true
  println("Hello from bogus!")
  println(s"[bogus]: Maximum number of bits that hartIDs might take: ${p(MaxHartIdBits)}")
  override lazy val module = new bogusModule(this)
}

class bogusModule[T <: Data](val outer: bogus, typ: T = UInt(32.W), qDepth: Int = 24)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
  with HasCoreParameters {
    /*
    val req_rd = RegInit(io.resp.bits.rd)

    // For detecting the operation to perform
    val funct         = io.cmd.bits.inst.funct

    val PicosV4Parser = Module(new InstructionParser)

    // Return register data
		io.resp.bits.rd := PicosV4Parser.io.resp_rd_id
		io.resp.bits.data := PicosV4Parser.io.resp_bits
    io.resp.valid := PicosV4Parser.io.resp_valid
    PicosV4Parser.io.resp_ready := io.resp.ready

    // Instruction input
    io.cmd.ready := PicosV4Parser.io.cmd_ready
    PicosV4Parser.io.cmd_valid := io.cmd.valid
    PicosV4Parser.io.rd_id := io.cmd.bits.inst.rd
    PicosV4Parser.io.rs1_bits := io.cmd.bits.rs1
    PicosV4Parser.io.rs2_bits := io.cmd.bits.rs2

    PicosV4Parser.io.funct := funct

    io.busy := PicosV4Parser.io.busy                  

    io.argsAndParentInfo <> PicosV4Parser.io.argsAndParentInfo       
    io.taskInfo <> PicosV4Parser.io.taskInfo                 
    io.depInfo <> PicosV4Parser.io.depInfo                   

    // Do not generate interrupts
    io.interrupt := false.B
    // We never issue memory requests
    io.mem.req.valid  := false.B
    */
}

class RoccCommandRouter(opcodes: Seq[OpcodeSet])(implicit p: Parameters)
    extends CoreModule()(p) {
  val io = new Bundle {
    val in = Flipped(Decoupled(new RoCCCommand))
    val out = Vec(opcodes.size, Decoupled(new RoCCCommand))
    val busy = Output(Bool())
  }

  val cmd = Queue(io.in)
  val cmdReadys = io.out.zip(opcodes).map { case (out, opcode) =>
    val me = opcode.matches(cmd.bits.inst.opcode)
    out.valid := cmd.valid && me
    out.bits := cmd.bits
    out.ready && me
  }
  cmd.ready := cmdReadys.reduce(_ || _)
  io.busy := cmd.valid

  assert(PopCount(cmdReadys) <= 1.U,
    "Custom opcode matched for more than one accelerator")
}
