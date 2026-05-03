package riscv.core

import chisel3._
import chisel3.util._
import riscv.Parameters

/**
 * Implements vl and vtype registers updated by vsetvli.
 * Only SEW=32 (vsew=010) and LMUL=1 (vlmul=000) are supported.
 *
 * vtype encoding (RVV spec §3.4):
 *   vtype[2:0]  = vlmul  (LMUL)
 *   vtype[5:3]  = vsew   (SEW = 8 << vsew)
 *   vtype[7]    = vill   (illegal if set)
 *
 * vsetvli encoding (RVV spec §6):
 *   inst[30:20] = zimm (vtypei)
 *   inst[19:15] = rs1  (AVL source)
 *   inst[11:7]  = rd   (write new vl)
 */
class VectorCSR extends Module {
  val io = IO(new Bundle {
    val is_vsetvli = Input(Bool())
    val rs1_data   = Input(UInt(Parameters.DataWidth))   // AVL from scalar reg
    val zimm       = Input(UInt(11.W))                    // inst[30:20]

    val vl    = Output(UInt(Parameters.DataWidth))     // current vector length
    val vtype = Output(UInt(Parameters.DataWidth))                        // current vtype
    val vlmax = Output(UInt(Parameters.DataWidth))     // VLMAX = LMUL * VLEN / SEW
  })

  val vl_reg    = RegInit(0.U(Parameters.DataWidth))
  val vtype_reg = RegInit(0.U(Parameters.DataWidth))

  // Decode zimm: vsew[2:0] = zimm[5:3], vlmul[2:0] = zimm[2:0]
  val vsew  = io.zimm(5, 3)   // 010 → SEW=32
  val vlmul = io.zimm(2, 0)   // 000 → LMUL=1
  
  val avl = io.rs1_data

  // VLMAX = (LMUL * VLEN) / SEW
  // For fixed ELEN=32, VLEN=128, LMUL=1: VLMAX = 4
  val vlmax_val = (Parameters.NumLanes).U(Parameters.DataWidth)
  io.vlmax := vlmax_val

  when(io.is_vsetvli) {
    val new_vl = Mux(
      avl === 0.U,
      vlmax_val,
      Mux(avl > vlmax_val, vlmax_val, avl)
    )
    vl_reg    := new_vl
    vtype_reg := io.zimm(7, 0)
    io.vl    := new_vl
    io.vtype := io.zimm(7, 0)
  }.otherwise {
    io.vl    := vl_reg
    io.vtype := vtype_reg
  }
}
