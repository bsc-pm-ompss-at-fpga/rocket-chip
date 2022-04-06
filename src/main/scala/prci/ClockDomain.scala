package freechips.rocketchip.prci

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import chisel3.util._
import freechips.rocketchip.util.{RoutingInfo}
import freechips.rocketchip.integrators.ReadyPacket
import freechips.rocketchip.tile.{TilePRCIDomain, RocketTile, BaseTile}
import freechips.rocketchip.subsystem.RocketSubsystem
import freechips.rocketchip.tile.TileResetDomain
import freechips.rocketchip.aop._
import firrtl.transforms.DontTouchAllTargets
import freechips.rocketchip.util.DontTouch

import picos_v4_integration_elements.{ArgsAndParentInfo, DepInfo, TaskInfo}

class DelegateIO extends Bundle {
  val retInterface            = Decoupled(UInt(64.W))
  val readyRequestInterface   = Decoupled(UInt(0.W))
  val readyQInterface         = Flipped(Decoupled(new ReadyPacket))
  val pmException             = Output(UInt(4.W))

  // A module is the master of its `Decoupled(*)` interfaces
  // and the slave of its Flipped(Decoupled(*)) interfaces
  val argsAndParentInfo         = Decoupled(new ArgsAndParentInfo)
  val taskInfo                  = Decoupled(new TaskInfo)
  val depInfo                   = Decoupled(Vec(2, new DepInfo))
}

class DomainImp(wrapper: Domain) extends LazyRawModuleImp(wrapper) {
    childClock := wrapper.clockBundle.clock
    childReset := wrapper.clockBundle.reset

    // these are just for backwards compatibility with external devices
    // that were manually wiring themselves to the domain's clock/reset input:
    val clock = IO(Output(chiselTypeOf(wrapper.clockBundle.clock)))
    val reset = IO(Output(chiselTypeOf(wrapper.clockBundle.reset)))
  
    val delegate = IO(new DelegateIO)

    clock := wrapper.clockBundle.clock
    reset := wrapper.clockBundle.reset
    
    if (wrapper.className == "TileResetDomain") {
        dontTouch(delegate)

        println("[DomainImp]: Hello! I am a TileResetDomain!")

        val tileResetDomainChildren = wrapper.getChildren
        println(s"[DomainImp tileResetDomainChildren]: ${tileResetDomainChildren}")

        val rocketTileDelegate = tileResetDomainChildren.last.asInstanceOf[RocketTile].module.delegate
        
        rocketTileDelegate.readyQInterface <> delegate.readyQInterface
        delegate.retInterface <> rocketTileDelegate.retInterface
        delegate.readyRequestInterface <> rocketTileDelegate.readyRequestInterface
        delegate.taskInfo <> rocketTileDelegate.taskInfo
        delegate.depInfo <> rocketTileDelegate.depInfo
        delegate.argsAndParentInfo <> rocketTileDelegate.argsAndParentInfo
        delegate.pmException := rocketTileDelegate.pmException

    } else if (wrapper.className == "TilePRCIDomain") {
        dontTouch(delegate)

        println("[DomainImp]: Hello! I am a TilePRCIDomain!")
        println(s"[DomainImp aop.Select.ios(tile.module)]: ${aop.Select.ios(wrapper.tile.module)}")
        println(s"[DomainImp wrapper.getChildren]: ${wrapper.getChildren}")
        val tileResetDomain: TileResetDomain = wrapper.getChildren.last.asInstanceOf[TileResetDomain]
        val tileResetDomainIOs = aop.Select.ios(tileResetDomain.module)
        println(s"[DomainImp tileResetDomainIOs]: ${tileResetDomainIOs}")

        val tileResetDomainDelegate : DelegateIO = tileResetDomainIOs.last.asInstanceOf[DelegateIO]

        val rocketTileIOs = aop.Select.ios(wrapper.tile.module)
        val numIO = aop.Select.ios(wrapper.tile.module).length
        val rocketTileDelegate = rocketTileIOs(numIO - 1)
        
        tileResetDomainDelegate.readyQInterface <> delegate.readyQInterface
        delegate.retInterface <> tileResetDomainDelegate.retInterface
        delegate.readyRequestInterface <> tileResetDomainDelegate.readyRequestInterface
        delegate.taskInfo <> tileResetDomainDelegate.taskInfo
        delegate.depInfo <> tileResetDomainDelegate.depInfo
        delegate.argsAndParentInfo <> tileResetDomainDelegate.argsAndParentInfo
        delegate.pmException := tileResetDomainDelegate.pmException
    }
}

abstract class Domain(implicit p: Parameters) extends LazyModule with HasDomainCrossing {
  def clockBundle: ClockBundle
  var tile: BaseTile = null
  
  println(s"[Domain:{this,this.className}]: ${this}, ${this.className}")

  lazy val module = new DomainImp(this)
}

abstract class ClockDomain(implicit p: Parameters) extends Domain with HasClockDomainCrossing

class ClockSinkDomain(val clockSinkParams: ClockSinkParameters)(implicit p: Parameters) extends ClockDomain
{
  def this(take: Option[ClockParameters] = None, name: Option[String] = None)(implicit p: Parameters) = this(ClockSinkParameters(take = take, name = name))
  val clockNode = ClockSinkNode(Seq(clockSinkParams))
  def clockBundle = clockNode.in.head._1
}

class ClockSourceDomain(val clockSourceParams: ClockSourceParameters)(implicit p: Parameters) extends ClockDomain
{
  def this(give: Option[ClockParameters] = None, name: Option[String] = None)(implicit p: Parameters) = this(ClockSourceParameters(give = give, name = name))
  val clockNode = ClockSourceNode(Seq(clockSourceParams))
  def clockBundle = clockNode.out.head._1
}

abstract class ResetDomain(implicit p: Parameters) extends Domain with HasResetDomainCrossing
