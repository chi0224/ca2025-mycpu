package riscv.core

import chisel3._
import chisel3.util._
import riscv.Parameters

/**
 * VectorRegFile: 32 × VLEN-bit vector register file
 *
 * Register v0 is NOT hardwired to zero (unlike scalar x0).
 * Supports 3 read ports (vs1, vs2, vd for VMACC) and 1 write port.
 */
class VectorRegFile extends Module {
  val io = IO(new Bundle {
    // Write port
    val write_enable  = Input(Bool())
    val write_address = Input(UInt(Parameters.VRegAddrWidth))
    val write_data    = Input(UInt(Parameters.VLEN))

    // Read ports
    val read_address_vs1 = Input(UInt(Parameters.VRegAddrWidth))
    val read_address_vs2 = Input(UInt(Parameters.VRegAddrWidth))
    val read_address_vd  = Input(UInt(Parameters.VRegAddrWidth))

    val read_data_vs1 = Output(UInt(Parameters.VLEN))
    val read_data_vs2 = Output(UInt(Parameters.VLEN))
    val read_data_vd  = Output(UInt(Parameters.VLEN))

    val debug_read_address = Input(UInt(Parameters.VRegAddrWidth))
    val debug_read_data    = Output(UInt(Parameters.VLEN))
  })

  // 32 vector registers, each VLEN bits wide
  val regs = RegInit(VecInit(Seq.fill(Parameters.VRegCount)(0.U(Parameters.VLEN))))

  when(io.write_enable) {
    regs(io.write_address) := io.write_data
  }

  io.read_data_vs1 := regs(io.read_address_vs1)
  io.read_data_vs2 := regs(io.read_address_vs2)
  io.read_data_vd  := regs(io.read_address_vd)

  io.debug_read_data := regs(io.debug_read_address)
}
