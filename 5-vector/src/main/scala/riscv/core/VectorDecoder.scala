package riscv.core

import chisel3._
import chisel3.util._
import riscv.Parameters

object InstructionTypesV {
  val OPV    = "b1010111".U(7.W)   // opcode for all vector arithmetic
  val LoadV  = "b0000111".U(7.W)   // vector load  (same as scalar Load but width=0b000)
  val StoreV = "b0100111".U(7.W)   // vector store (same as scalar Store but width=0b000)
}

object VectorFunct3 {
  val OPIVV  = "b000".U(3.W)
  val OPMVV  = "b010".U(3.W)
  val OPCFG = "b111".U(3.W)
}

// funct6 (RVV spec § 19)
object VectorFunct6 {
  val VADD  = "b000000".U(6.W)
  val VMUL  = "b100101".U(6.W)
  val VMACC = "b101101".U(6.W)
}

// Vector ALU operation select (internal encoding)
object VectorALUOp {
  val ADD  = 0.U(2.W)
  val MUL  = 1.U(2.W)
  val MACC = 2.U(2.W)
}

// VectorRegFile writeback source
object VRegWriteSource {
  val VectorALU    = 0.U(2.W)
  val VectorMemory = 1.U(2.W)
}

/**
 * VectorDecoder: Decode OP-V (0x57) and vector Load/Store instructions
 *
 * Extracts vd, vs1, vs2 (bits 11:7, 19:15, 24:20) and generates
 * control signals for VectorALU and VectorCSR.
 */
class VectorDecoder extends Module {
  val io = IO(new Bundle {
    val instruction = Input(UInt(Parameters.InstructionWidth))

    val is_vector_instr    = Output(Bool())
    val is_vsetvli         = Output(Bool())
    val is_vle32           = Output(Bool())
    val is_vse32           = Output(Bool())
    val is_vector_alu      = Output(Bool())

    val vd  = Output(UInt(Parameters.VRegAddrWidth))
    val vs1 = Output(UInt(Parameters.VRegAddrWidth))
    val vs2 = Output(UInt(Parameters.VRegAddrWidth))
    val zimm = Output(UInt(11.W))   // inst[30:20], for vsetvli

    val vreg_write_enable = Output(Bool())
    val vreg_write_source = Output(UInt(2.W))
    val valu_op           = Output(UInt(2.W))
  })

  val opcode = io.instruction(6, 0)
  val funct3 = io.instruction(14, 12)
  val funct6 = io.instruction(31, 26)

  // vd/vs1/vs2 share same bit positions as rd/rs1/rs2 in scalar
  io.vd  := io.instruction(11, 7)
  io.vs1 := io.instruction(19, 15)
  io.vs2 := io.instruction(24, 20)
  io.zimm := io.instruction(30, 20)

  val isOPV    = opcode === InstructionTypesV.OPV
  val isLoadV  = opcode === InstructionTypesV.LoadV
  val isStoreV = opcode === InstructionTypesV.StoreV
  // vsetvli: opcode=0x57, funct3=111, inst[31]=0
  val isVsetvli = isOPV && funct3 === VectorFunct3.OPCFG && io.instruction(31) === 0.U

  val isVADD  = isOPV && funct3 === VectorFunct3.OPIVV && funct6 === VectorFunct6.VADD
  val isVMUL  = isOPV && funct3 === VectorFunct3.OPMVV && funct6 === VectorFunct6.VMUL
  val isVMACC = isOPV && funct3 === VectorFunct3.OPMVV && funct6 === VectorFunct6.VMACC
  val isVALU  = isVADD || isVMUL || isVMACC

  io.is_vector_instr := isOPV || isLoadV || isStoreV
  io.is_vsetvli      := isVsetvli
  io.is_vle32        := isLoadV
  io.is_vse32        := isStoreV
  io.is_vector_alu   := isVALU

  io.vreg_write_enable := isVALU || isLoadV
  io.vreg_write_source := Mux(isLoadV, VRegWriteSource.VectorMemory, VRegWriteSource.VectorALU)

  io.valu_op := MuxCase(VectorALUOp.ADD, Seq(
    isVADD  -> VectorALUOp.ADD,
    isVMUL  -> VectorALUOp.MUL,
    isVMACC -> VectorALUOp.MACC
  ))
}
