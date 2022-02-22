// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ResetCrossingType, NoResetCrossing}
import freechips.rocketchip.tile._
import chisel3._
import chisel3.util._
import freechips.rocketchip.integrators._
import freechips.rocketchip.util.{RoutingInfo}

import picos_v4_integration_elements.{ArgsAndParentInfo, DepInfo, TaskInfo}

case class RocketCrossingParams(
  crossingType: ClockCrossingType = SynchronousCrossing(),
  master: TilePortParamsLike = TileMasterPortParams(),
  slave: TileSlavePortParams = TileSlavePortParams(),
  mmioBaseAddressPrefixWhere: TLBusWrapperLocation = CBUS,
  resetCrossingType: ResetCrossingType = NoResetCrossing()
) extends TileCrossingParamsLike {
  def forceSeparateClockReset: Boolean = false
}

case class RocketTileAttachParams(
  tileParams: RocketTileParams,
  crossingParams: RocketCrossingParams,
  lookup: LookupByHartIdImpl
) extends CanAttachTile { type TileType = RocketTile }

case object RocketTilesKey extends Field[Seq[RocketTileParams]](Nil)
case object RocketCrossingKey extends Field[Seq[RocketCrossingParams]](List(RocketCrossingParams()))

trait HasRocketTiles extends HasTiles { this: BaseSubsystem =>
  val rocketTiles = tiles.collect { case r: RocketTile => r }

  def coreMonitorBundles = (rocketTiles map { t =>
	t.module.core.rocketImpl.coreMonitorBundle
  }).toList

  def delegates = (rocketTiles map { t =>
	t.module.delegate
  }).toList
}

class RocketSubsystem(implicit p: Parameters) extends BaseSubsystem with HasRocketTiles {
  println(s"[RocketSubsystem]: The current system has ${nTiles} tiles.")
  println(s"[RocketSubsystem]: The hartID list is the following: ${hartIdList}")
  override lazy val module = new RocketSubsystemModuleImp(this)
  
  tiles.zipWithIndex.foreach{case (tile, i) => 
	  val tilePRCIDomain = tile_prci_domains(i)
	  tile.parentSubsystem = this
	  tile.parentPRCITile = tilePRCIDomain
	  tile.obviousId = i
  }

	def connectDelegate(signal: UInt) : Unit = {
		module.picosIO.pmException := signal
	}
}

class RocketSubsystemModuleImp[+L <: RocketSubsystem](_outer: L) extends BaseSubsystemModuleImp(_outer)
	with HasTilesModuleImp
{
  printf(s"[RocketSubsystemModuleImp]: The current system has ${outer.nTiles} tiles.\n")

  val picosIO = IO(new Bundle {
	val picosFinishedInterface  = new PicosFifoBundle(UInt(64.W))
	val picosReadyInterface     = new PicosReadyBundle(UInt(64.W))
	val picosSubInterface       = new PicosFifoBundle(UInt(64.W))
	val pmException             = Output(UInt(4.W))
  })
  
  var nCores = outer.delegates.length

  lazy val pmanager = Module(new CoreRealPicosManager(outer.nTiles))

  picosIO.picosSubInterface <> pmanager.io.picosSubInterface
  picosIO.picosReadyInterface <> pmanager.io.picosReadyInterface
  picosIO.picosFinishedInterface <> pmanager.io.picosFinishedInterface
  // FIXME: THIS SHOULD REFLECT THE STATE OF ALL DELEGATES
  picosIO.pmException := outer.tile_prci_domains(0).module.delegate.pmException

  outer.tile_prci_domains.zipWithIndex.foreach{ case (prci_tile, i) =>
    pmanager.io.subQInterface(i) <> prci_tile.module.delegate.subQInterface
    pmanager.io.subRequest(i) <> prci_tile.module.delegate.subRequest
    pmanager.io.retInterface(i) <> prci_tile.module.delegate.retInterface
    pmanager.io.readyRequestInterface(i) <> prci_tile.module.delegate.readyRequestInterface
    pmanager.io.tag(i) <> prci_tile.module.delegate.tag
    prci_tile.module.delegate.readyQInterface <> pmanager.io.readyQInterface(i)

    pmanager.io.argsAndParentInfoVec(i) <> prci_tile.module.delegate.argsAndParentInfo
    pmanager.io.taskInfoVec(i) <> prci_tile.module.delegate.taskInfo
    pmanager.io.depInfoVec(i) <> prci_tile.module.delegate.depInfo
  }
  
  outer.delegates.zipWithIndex.foreach{ case (delegate, i) =>
	println(s"Hello from tile $i!")
  }
}
