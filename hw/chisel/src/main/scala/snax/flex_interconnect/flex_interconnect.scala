package snax.flex_interconnect

import chisel3._
import chisel3.util._
import play.api.libs.json._
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe._

class InterconnectParams(
  val cpuPorts:          Int,        // Number of ports from the CPU
  val streamerAdressing: Seq[Int],   // Address width of the streamer
  val bankAddressing:    Int,        // Address width of the each bank
  val totalBanks:        Int         // Total number of banks available for the interconnect
){
  val usedPortStreamer:Seq[Int] = streamerAdressing.map(_/bankAddressing)
  val numPortCPU:Int      = cpuPorts  
}

class InterconnectParamsIO(
  params: InterconnectParams
) extends Bundle {
  // Add any hardware IO if required here
  // Flipped here makes the output to input, with flipped valid is an output, ready is an input and bits are output.
  // Flipped is used with consumers. For now, I'm assuming that the banks are always the producers, although this is not the case always
  val cpuP      = Vec(params.numPortCPU, 
                      Flipped(Decoupled(UInt(params.bankAddressing.W))))  // CPU data width = bank data width
  // Each streamer port will be of 64 bit wide however a streamer can have more than 1 port,
  // for 128 bit address space streamer would get 2 64 bit port connected to banks in a round robin fashion
  val streamerP = MixedVec(params.usedPortStreamer.map 
                      { numPorts => Vec(numPorts, Flipped(Decoupled(UInt(params.bankAddressing.W))))}) // Streamer data width >= bank data width
  val banks     = Vec(params.totalBanks, 
                      Decoupled(UInt(params.bankAddressing.W)))
}

class flexibleInterconnect(
  params: InterconnectParams
) extends Module {

  // Sanity checks
  require(
    params.totalBanks > 0,
    "Total banks should be more than number of ports from streamer"
  )
  require(
    params.bankAddressing == 64,
    "Each bank must have a 64 bit address bus"
  )

  // Create a boolean matrix to map the interconnect
  val connectReqTot = params.numPortCPU + params.usedPortStreamer.sum
  lazy val connectMat: Array[Array[Boolean]] = Array.fill(connectReqTot, params.totalBanks)(false)

  // Input output parameters
  val io = IO(
    new InterconnectParamsIO(
      params
    )
  )

  // Set connections from CPU to all banks
  for (ports <- 0 until params.numPortCPU) {
    for (banks <- 0 until params.totalBanks) {
      connectMat(ports)(banks) = true
    }
  }

  // Streamer: round-robin allocation
  params.usedPortStreamer
    .scanLeft(params.numPortCPU) { (offset, portsForStreamer) => offset + portsForStreamer } // Compute the offset for ports of the streamer
    .sliding(2) // gives pairs: (start, end)
    .zip(params.usedPortStreamer) // Group with used ports
    .foreach { case (Seq(start, _), portsForStreamer) => // Use the start and number of ports to mark connections
      for (ports <- 0 until portsForStreamer) {
        for (banks <- 0 until params.totalBanks if banks % portsForStreamer == ports) {
          connectMat(start + ports)(banks) = true
        }
      }
    }

  def getStreamerIndices(flatIndex: Int, usedPortStreamer: Seq[Int]): (Int, Int) = {
    var group = 0
    var sum = 0
    while (group < usedPortStreamer.length && sum + usedPortStreamer(group) <= flatIndex) {
      sum += usedPortStreamer(group)
      group += 1
    }
    val port = flatIndex - sum
    (group, port)
  }

  // Use the connection matrix to make the connections
  // Iterate over each bank to make the connection
  for (bankIdx <- 0 until params.totalBanks) {
    val cpuReadySignals      = WireInit(VecInit(Seq.fill(params.numPortCPU)(false.B)))
    val streamerReadySignals = WireInit(VecInit(Seq.fill(connectMat.length - params.numPortCPU)(false.B)))

    for(streamerPortIdx <- params.numPortCPU until connectMat.length) {
      if(connectMat(streamerPortIdx)(bankIdx)){
        val (groupIdx, portIdx) = getStreamerIndices((streamerPortIdx - params.numPortCPU), params.usedPortStreamer)
        io.streamerP(groupIdx)(portIdx).valid   := io.banks(bankIdx).valid
        io.streamerP(groupIdx)(portIdx).bits    := io.banks(bankIdx).bits
        streamerReadySignals(streamerPortIdx - params.numPortCPU) := io.streamerP(groupIdx)(portIdx).ready
      }
    }

    for(cpuPortIdx <- 0 until params.numPortCPU) {
      if(connectMat(cpuPortIdx)(bankIdx)){
        io.cpuP(cpuPortIdx).valid        := io.banks(bankIdx).valid
        io.cpuP(cpuPortIdx).bits         := io.banks(bankIdx).bits
        cpuReadySignals(cpuPortIdx)      := io.cpuP(cpuPortIdx).ready
      }
    }

    when(cpuReadySignals.asUInt.orR) {
      io.banks(bankIdx).ready := true.B  // CPU is ready, so bank can send
    } .elsewhen(streamerReadySignals.asUInt.orR) {
      io.banks(bankIdx).ready := true.B  // No CPU wants, so streamers can take
    } .otherwise {
      io.banks(bankIdx).ready := false.B // No one ready
    }
  }
}
