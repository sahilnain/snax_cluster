package snax.flex_interconnect

import chisel3._
import circt.stage.ChiselStage
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class InterconnectTest extends AnyFlatSpec with ChiselScalatestTester {

  "flexibleInterconnect" should "connect CPU and streamer ports to banks correctly" in {

    val params = new InterconnectParams(
      cpuPorts = 1,                 // One CPU port
      streamerAdressing = Seq(64),  // One streamer with one 64-bit port
      bankAddressing = 64,          // Banks are 64-bit addressed
      totalBanks = 4                // Four banks
    )

    test(new flexibleInterconnect(params)) { dut =>

      // Initially clear everything
      dut.io.banks.foreach { bank =>
        bank.valid.poke(false.B)
        bank.bits.poke(0.U)
      }
      dut.io.cpuP.foreach { cpu =>
        cpu.ready.poke(false.B)
      }
      for (group <- dut.io.streamerP; port <- group) {
        port.ready.poke(false.B)
      }

      // Test vector: send from bank 0
      dut.io.banks(0).valid.poke(true.B)
      dut.io.banks(0).bits.poke(0xABCD.U)

      // Case 1: CPU is ready
      dut.io.cpuP(0).ready.poke(true.B)
      dut.clock.step(1)
      dut.io.cpuP(0).valid.expect(true.B)
      dut.io.cpuP(0).bits.expect(0xABCD.U)
      dut.io.banks(0).ready.expect(true.B)

      // Case 2: CPU not ready, Streamer is ready
      dut.io.cpuP(0).ready.poke(false.B)
      dut.io.streamerP(0)(0).ready.poke(true.B)
      dut.clock.step(1)
      dut.io.streamerP(0)(0).valid.expect(true.B)
      dut.io.streamerP(0)(0).bits.expect(0xABCD.U)
      dut.io.banks(0).ready.expect(true.B)

      // Case 3: No one is ready
      dut.io.streamerP(0)(0).ready.poke(false.B)
      dut.clock.step(1)
      dut.io.banks(0).ready.expect(false.B)
    }
  }
}
