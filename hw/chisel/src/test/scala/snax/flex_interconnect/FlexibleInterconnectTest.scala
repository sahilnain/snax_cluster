package snax.flex_interconnect
import snax.flex_interconnect._

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class FlexibleInterconnectTester extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "flexibleInterconnect"

  it should "initialize and print the correct connection matrix" in {
    val testParams = new InterconnectParams(
      cpuPorts          = 2,      // Number of CPU ports, these ports will connect to all the banks
      streamerAdressing = Seq(256, 512),    // Address width of the streamer
      bankAddressing    = 64,     // Address width of the each bank, this value is mostly fixed to 64
      totalBanks        = 8,     // Total number of bank available
    )

    test(new flexibleInterconnect(testParams)) { c =>
      c.clock.step(2) // <- Needed to print output
    }
  }
}
