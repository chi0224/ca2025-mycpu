package riscv.core

import chisel3._
import chisel3.util._
import riscv.Parameters

/**
 * VectorALU: NumLanes parallel execution lanes
 *
 * Each lane processes one ELEN-bit element per cycle.
 * NumLanes = VLEN / ELEN is derived from Parameters, so changing VLEN/ELEN
 * automatically scales the number of hardware ALU instances.
 *
 * Supported ops (VectorALUOp):
 *   ADD  → vd[i] = vs2[i] + vs1[i]           (vadd.vv)
 *   MUL  → vd[i] = vs2[i] * vs1[i]           (vmul.vv)
 *   MACC → vd[i] = vd[i] + vs1[i] * vs2[i]   (vmacc.vv, RVV spec §11.12)
 */
class VectorALU extends Module {
  val io = IO(new Bundle {
    val vs1    = Input(UInt(Parameters.VLEN))
    val vs2    = Input(UInt(Parameters.VLEN))
    val vd_in  = Input(UInt(Parameters.VLEN))   // accumulator for VMACC
    val op     = Input(UInt(2.W))
    val vl     = Input(UInt(Parameters.DataWidth))

    val result = Output(UInt(Parameters.VLEN))
  })

  // Split VLEN-bit vectors into NumLanes × ELEN-bit elements
  val vs1_elems  = Wire(Vec(Parameters.NumLanes, UInt(Parameters.ELEN)))
  val vs2_elems  = Wire(Vec(Parameters.NumLanes, UInt(Parameters.ELEN)))
  val vd_elems   = Wire(Vec(Parameters.NumLanes, UInt(Parameters.ELEN)))
  val res_elems  = Wire(Vec(Parameters.NumLanes, UInt(Parameters.ELEN)))

  for (i <- 0 until Parameters.NumLanes) {
    vs1_elems(i) := io.vs1((i + 1) * Parameters.ELENBits - 1, i * Parameters.ELENBits)
    vs2_elems(i) := io.vs2((i + 1) * Parameters.ELENBits - 1, i * Parameters.ELENBits)
    vd_elems(i)  := io.vd_in((i + 1) * Parameters.ELENBits - 1, i * Parameters.ELENBits)
  }

  // Instantiate NumLanes scalar ALUs (Seq.fill → parameterized hardware)
  val lanes = Seq.fill(Parameters.NumLanes)(Module(new ALU))

  for (i <- 0 until Parameters.NumLanes) {
    // Default: ADD
    lanes(i).io.func := ALUFunctions.add
    lanes(i).io.op1  := vs1_elems(i)
    lanes(i).io.op2  := vs2_elems(i)

    res_elems(i) := MuxCase(lanes(i).io.result, Seq(
      (io.op === VectorALUOp.MUL)  -> (vs1_elems(i) * vs2_elems(i)),
      (io.op === VectorALUOp.MACC) -> (vd_elems(i) + vs1_elems(i) * vs2_elems(i))
    ))

    // vl mask: lanes beyond current vl keep vd unchanged (tail-agnostic simplified)
    // RVV spec §4.4: elements past vl are "tail elements"
    // For simplicity (face interview): tail elements = unchanged (tail-undisturbed)
    when(i.U >= io.vl) { res_elems(i) := vd_elems(i) }
  }

  // Concatenate lane results back to VLEN-bit output (little-endian: lane0 at LSB)
  io.result := Cat(res_elems.reverse)
}
