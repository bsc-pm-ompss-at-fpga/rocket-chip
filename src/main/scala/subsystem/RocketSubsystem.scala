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
	val picosFinishedInterface  = new PicosFifoBundle(UInt(32.W))
	val picosReadyInterface     = new PicosReadyBundle(UInt(32.W))
	val picosSubInterface       = new PicosFifoBundle(UInt(32.W))
	val pmException             = Output(UInt(4.W))
  })
  
  var nCores = outer.delegates.length

//   val picosManagerIO = IO(new Bundle {
// 	val tag                     = Flipped(Vec(nCores, Output(UInt(64.W))))
// 	val subQInterface           = Flipped(Vec(nCores, Decoupled(UInt(32.W))))
// 	val subRequest              = Flipped(Vec(nCores, Decoupled(RoutingInfo(64, 48))))
// 	val retInterface            = Flipped(Vec(nCores, Decoupled(UInt(32.W))))
// 	val readyRequestInterface   = Flipped(Vec(nCores, Decoupled(UInt(0.W))))
// 	val readyQInterface         = Vec(nCores, Decoupled(new ReadyPacket))
// 	val pmException             = Output(UInt(4.W))
//   })

  lazy val pmanager = Module(new CoreRealPicosManager(outer.nTiles))

  picosIO.picosSubInterface <> pmanager.io.picosSubInterface
  picosIO.picosReadyInterface <> pmanager.io.picosReadyInterface
  picosIO.picosFinishedInterface <> pmanager.io.picosFinishedInterface
  picosIO.pmException := outer.tile_prci_domains(0).module.delegate.pmException
  
//   TODO: TEST 2021-11-25__16-03-02
//   outer.tile_prci_domains(0).module.delegate.subQInterface := outer.tiles(0).asInstanceOf[RocketTile].module.delegate.subQInterface
//   pmanager.io.subQInterface(0) := outer.tile_prci_domains(0).module.delegate.subQInterface

  outer.tile_prci_domains.zipWithIndex.foreach{case (prci_tile, i) =>

	pmanager.io.subQInterface(i) <> prci_tile.module.delegate.subQInterface
	pmanager.io.subRequest(i) <> prci_tile.module.delegate.subRequest
	pmanager.io.retInterface(i) <> prci_tile.module.delegate.retInterface
	pmanager.io.readyRequestInterface(i) <> prci_tile.module.delegate.readyRequestInterface
	pmanager.io.tag(i) <> prci_tile.module.delegate.tag
	prci_tile.module.delegate.readyQInterface <> pmanager.io.readyQInterface(i)
  }
  
  outer.delegates.zipWithIndex.foreach{ case (delegate, i) =>
	println(s"Hello from tile $i!")

	// pmanager.io.subQInterface(i).bits := 0.U
	// pmanager.io.subQInterface(i).valid := false.B
	// pmanager.io.subRequest(i).bits.arbID := 0.U
	// pmanager.io.subRequest(i).bits.numPackets := 0.U
	// pmanager.io.subRequest(i).valid := false.B
	// pmanager.io.retInterface(i).bits := 0.U
	// pmanager.io.retInterface(i).valid := false.B
	// pmanager.io.readyRequestInterface(i).bits := 0.U
	// pmanager.io.readyRequestInterface(i).valid := false.B
	// pmanager.io.readyQInterface(i).ready := false.B
	// pmanager.io.tag(i) := delegate.tag

	// pmanager.io.subQInterface(i) <> castTile.delegate.subQInterface
	// pmanager.io.subRequest(i) <> castTile.delegate.subRequest
	// pmanager.io.retInterface(i) <> castTile.delegate.retInterface
	// pmanager.io.readyRequestInterface(i) <> castTile.delegate.readyRequestInterface
	// pmanager.io.tag(i) <> castTile.delegate.tag
	// castTile.delegate.readyQInterface <> pmanager.io.readyQInterface(i)
	// castTile.delegate.pmException <> pmanager.io.pmException

	// pmanager.io.subQInterface(i) <> picosManagerIO.subQInterface(i)
	// pmanager.io.subRequest(i) <> picosManagerIO.subRequest(i)
	// pmanager.io.retInterface(i) <> picosManagerIO.retInterface(i)
	// pmanager.io.readyRequestInterface(i) <> picosManagerIO.readyRequestInterface(i)
	// pmanager.io.tag(i) <> picosManagerIO.tag(i)
	// picosManagerIO.readyQInterface(i) <> pmanager.io.readyQInterface(i)
	// picosManagerIO.pmException <> pmanager.io.pmException
  }
}
