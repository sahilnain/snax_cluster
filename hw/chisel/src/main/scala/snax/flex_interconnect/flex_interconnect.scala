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

  // Input output parameters
  val io = IO(
    new InterconnectParamsIO(
      params
    )
  )

  // Create a boolean matrix to map the interconnect
  val connectReqTot = params.numPortCPU + params.usedPortStreamer.sum
  var connectMat    = RegInit(VecInit(Seq.fill(connectReqTot)(VecInit(Seq.fill(params.totalBanks)(false.B)))))

  // Set connections from CPU to all banks
  for (ports <- 0 until params.numPortCPU) {
    for (banks <- 0 until params.totalBanks) {
      connectMat(ports)(banks) := true.B
    }
  }

  var portOffset:Int = 0
  for (streameriter <- params.usedPortStreamer.indices) {
    // Set connections from Streamer to required banks
    for (ports <- 0 until params.usedPortStreamer(streameriter)) {
      for (banks <- 0 until params.totalBanks if banks % params.usedPortStreamer(streameriter) == ports){
        connectMat(params.numPortCPU + portOffset + ports)(banks) := true.B
      }
    }
    portOffset += params.usedPortStreamer(streameriter)
  }

  // === Print the matrix: rows = requesters, columns = banks ===
  printf(p"\n--- Connection Matrix (Requesters x Banks) ---\n")
  for (i <- 0 until connectReqTot) {
    for (j <- 0 until params.totalBanks) {
      printf(p"${connectMat(i)(j)} ")
    }
    printf(p"\n")
  }
}
