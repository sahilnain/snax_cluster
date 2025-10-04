/*
* This file can generate a correct connection map and a simple connection based on it. It provides a skeleton for the actual hardware connection,
* Next Steps:
 1. Change the I/O to support existing tcdmReq and tcdmRsp
 2. Adapt the code to support mutliple connections to the same memory port via MUX
 3. Write an arbitration scheme for selection among multiple ports
 4. Change the testing script "hw\chisel\src\test\scala\snax\flex_interconnect\FlexibleInterconnectTest.scala" to include all cases
*/

package snax.flex_interconnect

import snax.utils._

import chisel3._
import chisel3.util._
import play.api.libs.json._
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe._

class InterconnectParams(
  val numPortCPU:       Int,         // Number of ports from the CPU
  val addrWidthStreamer: Seq[Int],   // Address width of the streamer
  val dataWidthStreamer: Seq[Int],   // Address width of the streamer
  val addrWidthBank:     Int,        // Address width of the each bank
  val dataWidthBank:     Int,        // Data width of the each bank
  val totalBanks:        Int         // Total number of banks available for the interconnect
){
  val numPortStreamer:Seq[Int] = addrWidthStreamer.map(_/addrWidthBank)
}

// Class for create a response and a request channel for each port
class portIO(addrWidth: Int, tcdmDataWidth: Int, numPorts: Int) extends Bundle {
  val req_port  = Vec(numPorts,
                    Decoupled(new TcdmReq(addrWidth     = addrWidth,
                                          tcdmDataWidth = tcdmDataWidth)))
  val rsp_port  = Vec(numPorts,
                    Decoupled(new TcdmRsp(tcdmDataWidth = tcdmDataWidth)))
}

class InterconnectParamsIO(params: InterconnectParams) extends Bundle {
  // CPU ports: sending TCDM requests
  val cpuPorts = new portIO(addrWidth     = params.addrWidthBank,
                            tcdmDataWidth = params.addrWidthBank,
                            numPorts      = params.numPortCPU)
  // Streamer ports: each streamer can have multiple ports, sending TCDM requests
  /* The address and data width of each port will be equal to the banks however
     each streamer can have multiple ports so the overall width can be bigger, this 
     allows for flexiblity in the connection */
  val streamerPorts = MixedVec(params.numPortStreamer.map {numPorts =>
                              new portIO( addrWidth     = params.addrWidthBank,
                                          tcdmDataWidth = params.addrWidthBank,
                                          numPorts      = numPorts)})
  // TCDM ports
  val tcdmPorts = new portIO(addrWidth     = params.addrWidthBank,
                              tcdmDataWidth = params.addrWidthBank,
                              numPorts      = params.totalBanks)
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
    params.addrWidthBank == 64,
    "Each bank must have a 64 bit address bus"
  )

  // Create a boolean matrix to map the interconnect
  val numPortsTotal = params.numPortCPU + params.numPortStreamer.sum
  // lazy val because we need to compute this only once and not change after that
  lazy val connectMat: Array[Array[Boolean]] = Array.fill(numPortsTotal, params.totalBanks)(false)

  // Input output parameters
  val io = IO(
    new InterconnectParamsIO(
      params
    )
  )

  // Set connections from CPU to all banks
  for (portsIter <- 0 until params.numPortCPU) {
    for (tcdmPortsIter <- 0 until params.totalBanks) {
      connectMat(portsIter)(tcdmPortsIter) = true
    }
  }

  // Streamer: round-robin allocation
  params.numPortStreamer
    .scanLeft(params.numPortCPU) { (offset, portsForStreamer) => offset + portsForStreamer } // Compute the offset for ports of the streamer
    .sliding(2) // gives pairs: (start, end)
    .zip(params.numPortStreamer) // Group with used ports
    .foreach { case (Seq(start, _), portsForStreamer) => // Use the start and number of ports to mark connections
      for (portsIter <- 0 until portsForStreamer) {
        for (tcdmPortsIter <- 0 until params.totalBanks if tcdmPortsIter % portsForStreamer == portsIter) {
          connectMat(start + portsIter)(tcdmPortsIter) = true
        }
      }
    }

  def getStreamerIndices(flatIndex: Int, numPortStreamer: Seq[Int]): (Int, Int) = {
    var group = 0
    var sum = 0
    while (group < numPortStreamer.length && sum + numPortStreamer(group) <= flatIndex) {
      sum += numPortStreamer(group)
      group += 1
    }
    val port = flatIndex - sum
    (group, port)
  }

  // Use the connection matrix to make the connections
  // Iterate over each bank to make the connection
  for (bankIdx <- 0 until params.totalBanks) {
    // Vector for mapping ready signals
    val cpuReqReadySignals      = WireInit(VecInit(Seq.fill(params.numPortCPU)(false.B)))
    val streamerReqReadySignals = WireInit(VecInit(Seq.fill(connectMat.length - params.numPortCPU)(false.B)))

    // Collect all connected ports to this bank(both CPU and Streamer)
    val connectedCpuPorts      = (0 until params.numPortCPU).filter { cpuPortIdx => connectMat(cpuPortIdx)(bankIdx)}
    val connectedStreamerPorts = (0 until (connectMat.length - params.numPortCPU)).filter{streamerPortIdx =>
      connectMat(streamerPortIdx + params.numPortCPU)(bankIdx)}

    // Total number of connections
    val totalConnections = connectedCpuPorts.size + connectedStreamerPorts.size
    // if there is only 1 connection then we dont need any mux
    if (totalConnections == 1) {
      // Only one port is connected — direct connection, no mux needed
      if (connectedCpuPorts.nonEmpty) {
        val cpuPortIdx = connectedCpuPorts.head

        // Connect CPU -> TCDM request
        io.tcdmPorts(bankIdx).req_port <> io.cpuPorts(cpuPortIdx).req_port
        // Connect TCDM -> CPU response
        io.cpuPorts(cpuPortIdx).rsp_port <> io.tcdmPorts(bankIdx).rsp_port

      } else {
        val streamerFlatIdx = connectedStreamerPorts.head
        val (groupIdx, portIdx) = getStreamerIndices(streamerFlatIdx, params.numPortStreamer)

        // Connect Streamer -> TCDM request
        io.tcdmPorts(bankIdx).req_port <> io.streamerPorts(groupIdx)(portIdx).req_port
        // Connect TCDM -> Streamer response
        io.streamerPorts(groupIdx)(portIdx).rsp_port <> io.tcdmPorts(bankIdx).rsp_port
      }
    } else {
      // Multiple ports connected — need a multiplexer
      // Placeholder arbitration signal
      val selectionLine = Wire(UInt(log2Ceil(totalConnections).W))
      selectionLine := 0.U // hardcoded for now
      val selectedReqPort = Wire(new TcdmReq(addrWidth     = params.addrWidthBank,
                                          tcdmDataWidth    = params.dataWidthBank))
      val selectedRspPort = Wire(new TcdmRsp(tcdmDataWidth = params.dataWidthBank))

      // Request channel connection
      // Now, collect the actual CPU ports based on the filtered indices
      val connectedCpuReqPorts      = VecInit(connectedCpuPorts.map(cpuPortIdx => io.cpuPorts(cpuPortIdx).req_port))
      val connectedStreamerReqPorts = VecInit(connectedStreamerPorts.map { streamerPortIdx =>
          val (groupIdx, portIdx) = getStreamerIndices(streamerPortIdx, params.numPortStreamer)
          io.streamerPorts(groupIdx)(portIdx).req_port})
      // Combine all request ports into a single sequence
      val allConnectedReqPorts = connectedCpuReqPorts ++ connectedStreamerReqPorts
      // Instantiate the MuxDecoupled
      val portReqSelectionMux = Module(new MuxDecoupled(
                                    new TcdmReq(
                                      addrWidth     = params.addrWidthBank,
                                      tcdmDataWidth = params.dataWidthBank), 
                                      numInput = allConnectedReqPorts.length))

      // Multiplexing: Select signal from the ready ports
      portReqSelectionMux.io.sel := selectionLine
      io.tcdmPorts(bankIdx).req_port <> portReqSelectionMux.io.out
      portReqSelectionMux.io.in := VecInit(allConnectedReqPorts)

      // Response channel connection
      // Now, collect the actual CPU ports based on the filtered indices
      val connectedCpuRspPorts      = VecInit(connectedCpuPorts.map(cpuPortIdx => io.cpuPorts(cpuPortIdx).rsp_port))
      val connectedStreamerRspPorts = VecInit(connectedStreamerPorts.map { streamerPortIdx =>
          val (groupIdx, portIdx) = getStreamerIndices(streamerPortIdx, params.numPortStreamer)
          io.streamerPorts(groupIdx)(portIdx).rsp_port})
      // Combine all request ports into a single sequence
      val allConnectedRspPorts = connectedCpuRspPorts ++ connectedStreamerRspPorts
      // Instantiate the MuxDecoupled
      val portRspSelectionMux = Module(new MuxDecoupled(
                                    new TcdmReq(
                                      addrWidth     = params.addrWidthBank,
                                      tcdmDataWidth = params.dataWidthBank), 
                                      numInput = allConnectedRspPorts.length))

      // Multiplexing: Select signal from the ready ports
      portRspSelectionMux.io.sel := selectionLine
      io.tcdmPorts(bankIdx).rsp_port <> portRspSelectionMux.io.out
      portRspSelectionMux.io.in := VecInit(allConnectedRspPorts)
    }
  }
}
