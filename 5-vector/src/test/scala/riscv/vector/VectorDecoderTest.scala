package riscv.core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class VectorDecoderTest extends AnyFlatSpec with ChiselScalatestTester {

  // ──────────────────────────────────────────────
  // 指令編碼建構器（對應 RVV spec）
  // ──────────────────────────────────────────────

  /** vsetvli rd, rs1, vtypei
   *  [31]=0, [30:20]=zimm(11bit), [19:15]=rs1, [14:12]=111, [11:7]=rd, [6:0]=1010111
   */
  def mkVsetvli(rd: Int, rs1: Int, zimm: Int): Long = {
    val inst = (0L << 31) | ((zimm.toLong & 0x7FFL) << 20) |
               ((rs1.toLong & 0x1FL) << 15) | (0x7L << 12) |
               ((rd.toLong & 0x1FL) << 7) | 0x57L
    inst & 0xFFFFFFFFL
  }

  /** vadd.vv vd, vs2, vs1
   *  funct6=000000, vm=1, [24:20]=vs2, [19:15]=vs1, funct3=000, [11:7]=vd, opcode=1010111
   */
  def mkVadd(vd: Int, vs1: Int, vs2: Int): Long = {
    val inst = (0x00L << 26) | (1L << 25) |
               ((vs2.toLong & 0x1FL) << 20) | ((vs1.toLong & 0x1FL) << 15) |
               (0x0L << 12) | ((vd.toLong & 0x1FL) << 7) | 0x57L
    inst & 0xFFFFFFFFL
  }

  /** vmul.vv vd, vs2, vs1
   *  funct6=100101, funct3=010
   */
  def mkVmul(vd: Int, vs1: Int, vs2: Int): Long = {
    val inst = (0x25L << 26) | (1L << 25) |
               ((vs2.toLong & 0x1FL) << 20) | ((vs1.toLong & 0x1FL) << 15) |
               (0x2L << 12) | ((vd.toLong & 0x1FL) << 7) | 0x57L
    inst & 0xFFFFFFFFL
  }

  /** vmacc.vv vd, vs1, vs2
   *  funct6=101101, funct3=010
   */
  def mkVmacc(vd: Int, vs1: Int, vs2: Int): Long = {
    val inst = (0x2DL << 26) | (1L << 25) |
               ((vs2.toLong & 0x1FL) << 20) | ((vs1.toLong & 0x1FL) << 15) |
               (0x2L << 12) | ((vd.toLong & 0x1FL) << 7) | 0x57L
    inst & 0xFFFFFFFFL
  }

  /** vle32.v vd, (rs1)
   *  opcode=0000111, funct3=000(OPIVV), width=000, [19:15]=rs1, [11:7]=vd
   */
  def mkVle32(vd: Int, rs1: Int): Long = {
    val inst = (0x00L << 26) | (1L << 25) |
               (0x08L << 20) |                // vs2=01000 (mew=0, mop=00, vm=1, nf=000)
               ((rs1.toLong & 0x1FL) << 15) |
               (0x0L << 12) | ((vd.toLong & 0x1FL) << 7) | 0x07L
    inst & 0xFFFFFFFFL
  }

  /** vse32.v vs3, (rs1)
   *  opcode=0100111, funct3=000
   */
  def mkVse32(vs3: Int, rs1: Int): Long = {
    val inst = (0x00L << 26) | (1L << 25) |
               (0x08L << 20) |
               ((rs1.toLong & 0x1FL) << 15) |
               (0x0L << 12) | ((vs3.toLong & 0x1FL) << 7) | 0x27L
    inst & 0xFFFFFFFFL
  }

  /** 普通 scalar 指令（ADD）：opcode=0110011 */
  def mkScalarAdd(rd: Int, rs1: Int, rs2: Int): Long = {
    val inst = ((rs2.toLong & 0x1FL) << 20) | ((rs1.toLong & 0x1FL) << 15) |
               ((rd.toLong & 0x1FL) << 7) | 0x33L
    inst & 0xFFFFFFFFL
  }

  // ──────────────────────────────────────────────
  // Part 1: Directed Smoke Tests
  // ──────────────────────────────────────────────
  behavior of "VectorDecoder - Directed Smoke Tests"

  it should "decode vsetvli correctly" in {
    test(new VectorDecoder) { dut =>
      // vsetvli x1, x2, e32m1 → rd=1, rs1=2, zimm=0x010
      val inst = mkVsetvli(rd=1, rs1=2, zimm=0x010)
      dut.io.instruction.poke(inst.U)
      dut.io.is_vector_instr.expect(true.B)
      dut.io.is_vsetvli.expect(true.B)
      dut.io.is_vle32.expect(false.B)
      dut.io.is_vse32.expect(false.B)
      dut.io.is_vector_alu.expect(false.B)
      dut.io.vreg_write_enable.expect(false.B)
      dut.io.vd.expect(1.U)    // rd=1
      dut.io.vs1.expect(2.U)   // rs1=2
      dut.io.zimm.expect(0x010.U)
    }
  }

  it should "decode vadd.vv correctly" in {
    test(new VectorDecoder) { dut =>
      val inst = mkVadd(vd=4, vs1=1, vs2=2)
      dut.io.instruction.poke(inst.U)
      dut.io.is_vector_instr.expect(true.B)
      dut.io.is_vsetvli.expect(false.B)
      dut.io.is_vector_alu.expect(true.B)
      dut.io.valu_op.expect(VectorALUOp.ADD)
      dut.io.vreg_write_enable.expect(true.B)
      dut.io.vreg_write_source.expect(VRegWriteSource.VectorALU)
      dut.io.vd.expect(4.U)
      dut.io.vs1.expect(1.U)
      dut.io.vs2.expect(2.U)
    }
  }

  it should "decode vmul.vv correctly" in {
    test(new VectorDecoder) { dut =>
      val inst = mkVmul(vd=5, vs1=3, vs2=6)
      dut.io.instruction.poke(inst.U)
      dut.io.is_vector_instr.expect(true.B)
      dut.io.is_vector_alu.expect(true.B)
      dut.io.valu_op.expect(VectorALUOp.MUL)
      dut.io.vreg_write_enable.expect(true.B)
      dut.io.vd.expect(5.U)
      dut.io.vs1.expect(3.U)
      dut.io.vs2.expect(6.U)
    }
  }

  it should "decode vmacc.vv correctly" in {
    test(new VectorDecoder) { dut =>
      val inst = mkVmacc(vd=7, vs1=8, vs2=9)
      dut.io.instruction.poke(inst.U)
      dut.io.is_vector_instr.expect(true.B)
      dut.io.is_vector_alu.expect(true.B)
      dut.io.valu_op.expect(VectorALUOp.MACC)
      dut.io.vreg_write_enable.expect(true.B)
      dut.io.vd.expect(7.U)
      dut.io.vs1.expect(8.U)
      dut.io.vs2.expect(9.U)
    }
  }

  it should "decode vle32.v correctly" in {
    test(new VectorDecoder) { dut =>
      val inst = mkVle32(vd=10, rs1=5)
      dut.io.instruction.poke(inst.U)
      dut.io.is_vector_instr.expect(true.B)
      dut.io.is_vle32.expect(true.B)
      dut.io.is_vse32.expect(false.B)
      dut.io.is_vector_alu.expect(false.B)
      dut.io.vreg_write_enable.expect(true.B)
      dut.io.vreg_write_source.expect(VRegWriteSource.VectorMemory)
      dut.io.vd.expect(10.U)
      dut.io.vs1.expect(5.U)   // rs1 在 vs1 位置
    }
  }

  it should "decode vse32.v correctly" in {
    test(new VectorDecoder) { dut =>
      val inst = mkVse32(vs3=11, rs1=6)
      dut.io.instruction.poke(inst.U)
      dut.io.is_vector_instr.expect(true.B)
      dut.io.is_vle32.expect(false.B)
      dut.io.is_vse32.expect(true.B)
      dut.io.is_vector_alu.expect(false.B)
      dut.io.vreg_write_enable.expect(false.B)  // store 不寫 vreg
    }
  }

  it should "decode scalar instruction as non-vector" in {
    test(new VectorDecoder) { dut =>
      val inst = mkScalarAdd(rd=1, rs1=2, rs2=3)
      dut.io.instruction.poke(inst.U)
      dut.io.is_vector_instr.expect(false.B)
      dut.io.is_vsetvli.expect(false.B)
      dut.io.is_vle32.expect(false.B)
      dut.io.is_vse32.expect(false.B)
      dut.io.is_vector_alu.expect(false.B)
      dut.io.vreg_write_enable.expect(false.B)
    }
  }

  // ──────────────────────────────────────────────
  // Part 2: Register Field Extraction
  // ──────────────────────────────────────────────
  behavior of "VectorDecoder - Register Field Extraction"

  it should "correctly extract vd/vs1/vs2 from all 5-bit combinations" in {
    test(new VectorDecoder) { dut =>
      // 用 vadd.vv 驗證各種 reg 組合
      for (vd <- Seq(0, 1, 15, 16, 31)) {
        for (vs1 <- Seq(0, 7, 31)) {
          val vs2 = (vd + 1) % 32
          val inst = mkVadd(vd=vd, vs1=vs1, vs2=vs2)
          dut.io.instruction.poke(inst.U)
          dut.io.vd.expect(vd.U,  s"vd mismatch for vd=$vd vs1=$vs1 vs2=$vs2")
          dut.io.vs1.expect(vs1.U, s"vs1 mismatch for vd=$vd vs1=$vs1 vs2=$vs2")
          dut.io.vs2.expect(vs2.U, s"vs2 mismatch for vd=$vd vs1=$vs1 vs2=$vs2")
        }
      }
    }
  }

  it should "correctly extract zimm from vsetvli" in {
    test(new VectorDecoder) { dut =>
      for (zimm <- Seq(0x000, 0x010, 0x008, 0x7FF)) {
        val inst = mkVsetvli(rd=1, rs1=1, zimm=zimm)
        dut.io.instruction.poke(inst.U)
        dut.io.is_vsetvli.expect(true.B)
        dut.io.zimm.expect(zimm.U, s"zimm mismatch for zimm=0x${zimm.toHexString}")
      }
    }
  }

  // ──────────────────────────────────────────────
  // Part 3: Corner Cases
  // ──────────────────────────────────────────────
  behavior of "VectorDecoder - Corner Cases"

  it should "NOP (all-zero instruction) is not a vector instruction" in {
    test(new VectorDecoder) { dut =>
      dut.io.instruction.poke(0.U)
      dut.io.is_vector_instr.expect(false.B)
      dut.io.vreg_write_enable.expect(false.B)
    }
  }

  it should "vsetvli with inst[31]=1 should NOT be decoded as vsetvli" in {
    test(new VectorDecoder) { dut =>
      // inst[31]=1 → vsetivli（不同指令），硬體不支援，應該 is_vsetvli=false
      val inst = mkVsetvli(rd=1, rs1=1, zimm=0x010) | (1L << 31)
      dut.io.instruction.poke(inst.U)
      dut.io.is_vsetvli.expect(false.B)
    }
  }

  it should "vse32 does not assert vreg_write_enable" in {
    test(new VectorDecoder) { dut =>
      val inst = mkVse32(vs3=1, rs1=2)
      dut.io.instruction.poke(inst.U)
      dut.io.is_vse32.expect(true.B)
      dut.io.vreg_write_enable.expect(false.B)  // store 只讀不寫
    }
  }
}
