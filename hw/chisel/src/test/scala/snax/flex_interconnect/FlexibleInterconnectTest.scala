package snax.flex_interconnect
import snax.flex_interconnect._

import chisel3._
import chisel3.iotesters.{PeekPokeTester, Driver}

class flexibleInterconnectTester(c: flexibleInterconnect) extends PeekPokeTester(c) {

  // Initialize the ports
  poke(c.io.banks(0).valid, false.B)   // Initialize valid signals for all banks
  poke(c.io.banks(1).valid, false.B)
  poke(c.io.banks(0).bits, 0.U)
  poke(c.io.banks(1).bits, 0.U)

  // Set up initial valid signals for CPU and streamer ports
  poke(c.io.cpuP(0).valid, true.B)     // CPU port 0 sends data
  poke(c.io.cpuP(1).valid, false.B)    // CPU port 1 does not send data
  poke(c.io.streamerP(0)(0).valid, true.B)  // Streamer port 0 sends data
  poke(c.io.streamerP(1)(0).valid, false.B) // Streamer port 1 does not send data

  // Now simulate a few clock cycles
  step(1)  // Step 1 clock cycle

  // Check the interconnect's response
  // Assert that the bank's ready signal is high if a valid request is sent
  expect(c.io.banks(0).ready, true.B)   // Bank 0 should be ready because CPU port 0 is valid
  expect(c.io.banks(1).ready, true.B)   // Bank 1 should be ready because streamer port 0 is valid

  // Change the inputs: Let CPU port 0 not be valid and make streamer port 1 valid
  poke(c.io.cpuP(0).valid, false.B)
  poke(c.io.streamerP(1)(0).valid, true.B)

  step(1)  // Step 1 clock cycle again

  // Assert that bank 1 should be ready now because streamer 1 is valid
  expect(c.io.banks(0).ready, false.B)  // Bank 0 should no longer be ready (since CPU port 0 isn't valid)
  expect(c.io.banks(1).ready, true.B)   // Bank 1 should be ready because streamer 1 is valid

  // Continue to test round-robin behavior, data transfer, and priority
  // For example, you can test the next clock cycle with different valid signals, etc.

  // Additional checks could include:
  // - Verifying that CPU ports are prioritized over streamer ports
  // - Verifying round-robin behavior (i.e., which streamer port gets assigned to which bank)
}

object flexibleInterconnectTester extends App {
  // Create the testbench and run the tests
  iotesters.Driver.execute(args, () => new flexibleInterconnect(params)) {
    c => new flexibleInterconnectTester(c)
  }
}

