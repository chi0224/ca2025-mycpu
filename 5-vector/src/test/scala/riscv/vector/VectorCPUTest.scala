// SPDX-License-Identifier: MIT
package riscv.vector

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import peripheral.{InstructionROM, Memory, ROMLoader}
import riscv.core.CPU
import riscv.{Parameters, TestAnnotations}

// ──────────────────────────────────────────────────────────────────────────────
// VectorTestTopModule
// ──────────────────────────────────────────────────────────────────────────────
class VectorTestTopModule(exeFilename: String) extends Module {
  val io = IO(new Bundle {
    val regs_debug_read_address  = Input(UInt(Parameters.PhysicalRegisterAddrWidth))
    val regs_debug_read_data     = Output(UInt(Parameters.DataWidth))
    val vreg_debug_read_address  = Input(UInt(Parameters.VRegAddrWidth))
    val vreg_debug_read_data     = Output(UInt(Parameters.VLEN))
    val mem_debug_read_address   = Input(UInt(Parameters.AddrWidth))
    val mem_debug_read_data      = Output(UInt(Parameters.DataWidth))
  })

  val mem             = Module(new Memory(16384))
  val instruction_rom = Module(new InstructionROM(exeFilename))
  val rom_loader      = Module(new ROMLoader(instruction_rom.capacity))

  rom_loader.io.rom_data     := instruction_rom.io.data
  rom_loader.io.load_address := Parameters.EntryAddress
  instruction_rom.io.address := rom_loader.io.rom_address

  val cpu_clkdiv = RegInit(0.U(2.W))
  val cpu_tick   = cpu_clkdiv === 0.U
  cpu_clkdiv := Mux(cpu_clkdiv === 3.U, 0.U, cpu_clkdiv + 1.U)

  withClock(cpu_tick.asClock) {
    val cpu = Module(new CPU)

    cpu.io.instruction_valid       := rom_loader.io.load_finished
    cpu.io.debug_read_address      := io.regs_debug_read_address
    io.regs_debug_read_data        := cpu.io.debug_read_dataㄕㄛ

    cpu.io.vreg_debug_read_address := io.vreg_debug_read_address
    io.vreg_debug_read_data        := cpu.io.vreg_debug_read_data

    mem.io.instruction_address := cpu.io.instruction_address
    cpu.io.instruction         := mem.io.instruction

    cpu.io.vector_memory_bundle <> mem.io.vector_bundle

    when(!rom_loader.io.load_finished) {
      rom_loader.io.bundle           <> mem.io.bundle
      cpu.io.memory_bundle.read_data := 0.U
    }.otherwise {
      rom_loader.io.bundle.read_data := 0.U
      cpu.io.memory_bundle           <> mem.io.bundle
    }
  }

  mem.io.debug_read_address := io.mem_debug_read_address
  io.mem_debug_read_data    := mem.io.debug_read_data
}

// ──────────────────────────────────────────────────────────────────────────────
// 共用 helper
// ──────────────────────────────────────────────────────────────────────────────
trait VectorTestHelper { this: AnyFlatSpec =>

  def waitForCompletion(c: VectorTestTopModule,
                        iters: Int = 50, steps: Int = 1000): Unit = {
    for (i <- 1 to iters) {
      c.clock.step(steps)
      c.io.mem_debug_read_address.poke((i * 4).U)
    }
  }

  def checkMemElements(c: VectorTestTopModule,
                       baseAddr: Int, expected: Seq[Int]): Unit = {
    for ((exp, i) <- expected.zipWithIndex) {
      c.io.mem_debug_read_address.poke((baseAddr + i * 4).U)
      c.clock.step()
      c.io.mem_debug_read_data.expect(
        exp.U,
        s"mem[0x${(baseAddr + i * 4).toHexString}] (element[$i]) should be $exp"
      )
    }
  }

  /**
   * 從 VRF debug port 讀出一整個 vector register（VLEN bits），
   * 再按 32-bit element 拆解後與 expected 比對。
   *
   * 注意：VRF debug port 一次吐出整個 VLEN=128 bits（4 × 32-bit elements）。
   * element[0] 存在 bits[31:0]，element[1] 存在 bits[63:32]，依此類推（little-endian packing）。
   */
  def checkVRegElements(c: VectorTestTopModule,
                        vregIdx: Int, expected: Seq[Int]): Unit = {
    c.io.vreg_debug_read_address.poke(vregIdx.U)
    c.clock.step()
    val raw = c.io.vreg_debug_read_data.peekInt()
    for ((exp, i) <- expected.zipWithIndex) {
      val elem = ((raw >> (i * 32)) & 0xFFFFFFFFL).toInt
      assert(
        elem == exp,
        s"vreg[$vregIdx] element[$i] = 0x${elem.toHexString}, expected 0x${exp.toHexString}"
      )
    }
  }

  /**
   * 印出 VRF 中某個 register 的所有 32-bit element（debug 用）。
   */
  def printVReg(c: VectorTestTopModule, vregIdx: Int, vl: Int = 4): Unit = {
    c.io.vreg_debug_read_address.poke(vregIdx.U)
    c.clock.step()
    val raw = c.io.vreg_debug_read_data.peekInt()
    for (i <- 0 until vl) {
      val elem = ((raw >> (i * 32)) & 0xFFFFFFFFL).toInt
      println(f"[VRF] v$vregIdx element[$i] = 0x$elem%08x ($elem)")
    }
  }
}

// ──────────────────────────────────────────────────────────────────────────────
// Test 1: vsetvli
// vsetvli t0, x0, e32, m1, ta, ma → t0(x5) = vl = 4
// ──────────────────────────────────────────────────────────────────────────────
class VectorVsetvliIntegrationTest extends AnyFlatSpec
    with ChiselScalatestTester with VectorTestHelper {

  behavior of "Vector CPU Integration - vsetvli"

  it should "write vl=4 to scalar register t0 (x5)" in {
    test(new VectorTestTopModule("vsetvli_only.asmbin"))
        .withAnnotations(TestAnnotations.annos) { c =>
      waitForCompletion(c, iters = 10)
      c.io.regs_debug_read_address.poke(5.U)
      c.clock.step()
      c.io.regs_debug_read_data.expect(4.U,
        "vsetvli with e32m1 and VLEN=128 should set vl=4")
    }
  }
}

// ──────────────────────────────────────────────────────────────────────────────
// Test 2: vle32.v + vse32.v round-trip
//
// 組語 vls_roundtrip.s:
//   src  @ 0x2000: [10, 20, 30, 40]
//   vle32.v v1, (0x2000)
//   vse32.v v1, (0x2010)
//
// 驗證：
//   (A) VRF v1 = [10, 20, 30, 40]（vle32 是否寫進 VRF）
//   (B) mem[0x2010..0x201C] = [10, 20, 30, 40]（vse32 是否寫出記憶體）
// ──────────────────────────────────────────────────────────────────────────────
class VectorLoadStoreTest extends AnyFlatSpec
    with ChiselScalatestTester with VectorTestHelper {

  behavior of "Vector CPU Integration - vle32+vse32 round-trip"

  it should "load 4 elements from memory into VRF and store back unchanged" in {
    test(new VectorTestTopModule("vls_roundtrip.asmbin"))
        .withAnnotations(TestAnnotations.annos) { c =>
      waitForCompletion(c)

      // (A) 先確認 VRF v1 的內容是否正確（vle32 有沒有真的寫進去）
      println("=== VRF Debug: checking v1 after vle32 ===")
      printVReg(c, vregIdx = 1)
      checkVRegElements(c, vregIdx = 1, expected = Seq(10, 20, 30, 40))

      // (B) 再確認 memory store 結果（vse32 有沒有真的寫出去）
      checkMemElements(c, baseAddr = 0x2010, expected = Seq(10, 20, 30, 40))
    }
  }
}

// ──────────────────────────────────────────────────────────────────────────────
// Test 3: vadd.vv
//
// 組語 vadd_vector.s:
//   v1=[1,2,3,4]     @ 0x2000
//   v2=[10,20,30,40] @ 0x2010
//   vadd.vv v3, v1, v2 → [11,22,33,44]
//   vse32.v v3, (0x2020)
//
// 驗證：
//   (A) VRF v1=[1,2,3,4], v2=[10,20,30,40]（vle32 是否正確）
//   (B) VRF v3=[11,22,33,44]（vadd 是否正確）
//   (C) mem[0x2020..0x202C] = [11, 22, 33, 44]（vse32 是否正確）
// ──────────────────────────────────────────────────────────────────────────────
class VectorVaddTest extends AnyFlatSpec
    with ChiselScalatestTester with VectorTestHelper {

  behavior of "Vector CPU Integration - vadd.vv"

  it should "compute element-wise addition v1+v2 and store to memory" in {
    test(new VectorTestTopModule("vadd_vector.asmbin"))
        .withAnnotations(TestAnnotations.annos) { c =>
      waitForCompletion(c)

      // ── Scalar register sanity check ──────────────────────────
      c.io.regs_debug_read_address.poke(10.U)   // a0
      c.clock.step(1)
      println(f"[DEBUG] a0 (x10) = 0x${c.io.regs_debug_read_data.peekInt()}%x  (expected 0x2000)")

      c.io.regs_debug_read_address.poke(12.U)   // a2
      c.clock.step(1)
      println(f"[DEBUG] a2 (x12) = 0x${c.io.regs_debug_read_data.peekInt()}%x  (expected 0x2020)")

      c.io.regs_debug_read_address.poke(5.U)    // t0 = vl
      c.clock.step(1)
      println(f"[DEBUG] t0/vl (x5) = 0x${c.io.regs_debug_read_data.peekInt()}%x  (expected 4)")

      // ── (A) VRF v1 應為 [1,2,3,4] ────────────────────────────
      println("=== VRF Debug: v1 (source of vadd) ===")
      printVReg(c, vregIdx = 1)
      checkVRegElements(c, vregIdx = 1, expected = Seq(1, 2, 3, 4))

      // ── (A) VRF v2 應為 [10,20,30,40] ────────────────────────
      println("=== VRF Debug: v2 (source of vadd) ===")
      printVReg(c, vregIdx = 2)
      checkVRegElements(c, vregIdx = 2, expected = Seq(10, 20, 30, 40))

      // ── (B) VRF v3 應為 vadd 結果 [11,22,33,44] ──────────────
      println("=== VRF Debug: v3 (result of vadd.vv) ===")
      printVReg(c, vregIdx = 3)
      checkVRegElements(c, vregIdx = 3, expected = Seq(11, 22, 33, 44))

      // ── (C) 最終 memory store 結果 ───────────────────────────
      checkMemElements(c, baseAddr = 0x2020, expected = Seq(11, 22, 33, 44))
    }
  }
}

// ──────────────────────────────────────────────────────────────────────────────
// Test 4: vmul.vv
//
// 組語 vmul_vector.s:
//   v1=[2,3,4,5]   @ 0x2000
//   v2=[3,4,5,6]   @ 0x2010
//   vmul.vv v3, v1, v2 → [6,12,20,30]
//   vse32.v v3, (0x2020)
//
// 驗證：
//   (A) VRF v3=[6,12,20,30]
//   (B) mem[0x2020..0x202C] = [6, 12, 20, 30]
// ──────────────────────────────────────────────────────────────────────────────
class VectorVmulTest extends AnyFlatSpec
    with ChiselScalatestTester with VectorTestHelper {

  behavior of "Vector CPU Integration - vmul.vv"

  it should "compute element-wise multiply v1*v2 and store to memory" in {
    test(new VectorTestTopModule("vmul_vector.asmbin"))
        .withAnnotations(TestAnnotations.annos) { c =>
      waitForCompletion(c)

      // (A) VRF v3 應為乘法結果
      println("=== VRF Debug: v1, v2, v3 after vmul.vv ===")
      printVReg(c, vregIdx = 1)
      printVReg(c, vregIdx = 2)
      printVReg(c, vregIdx = 3)
      checkVRegElements(c, vregIdx = 3, expected = Seq(6, 12, 20, 30))

      // (B) memory store 結果
      checkMemElements(c, baseAddr = 0x2020, expected = Seq(6, 12, 20, 30))
    }
  }
}

// ──────────────────────────────────────────────────────────────────────────────
// Test 5: vmacc.vv
//
// 組語 vmacc_vector.s:
//   v1=[2,3,4,5]   @ 0x2000  (vs1)
//   v2=[3,4,5,6]   @ 0x2010  (vs2)
//   v3=[1,1,1,1]   @ 0x2020  (vd 初始值)
//   vmacc.vv v3, v1, v2 → v3 = v3 + v1*v2 = [7,13,21,31]
//   vse32.v v3, (0x2030)
//
// 驗證：
//   (A) VRF v3=[7,13,21,31]（vmacc 結果，含初始值累加）
//   (B) mem[0x2030..0x203C] = [7, 13, 21, 31]
// ──────────────────────────────────────────────────────────────────────────────
class VectorVmaccTest extends AnyFlatSpec
    with ChiselScalatestTester with VectorTestHelper {

  behavior of "Vector CPU Integration - vmacc.vv"

  it should "compute vd = vd + vs1*vs2 (multiply-accumulate) correctly" in {
    test(new VectorTestTopModule("vmacc_vector.asmbin"))
        .withAnnotations(TestAnnotations.annos) { c =>
      waitForCompletion(c)

      // (A) 確認 vmacc 前的 operand 以及結果
      println("=== VRF Debug: v1(vs1), v2(vs2), v3(vd=accum) after vmacc.vv ===")
      printVReg(c, vregIdx = 1)
      printVReg(c, vregIdx = 2)
      printVReg(c, vregIdx = 3)
      checkVRegElements(c, vregIdx = 3, expected = Seq(7, 13, 21, 31))

      // (B) memory store 結果
      checkMemElements(c, baseAddr = 0x2030, expected = Seq(7, 13, 21, 31))
    }
  }
}

// ──────────────────────────────────────────────────────────────────────────────
// Test 6: 2×2 矩陣乘法
//
// 組語 matmul2x2.s:
//   A (row-major)  @ 0x2000: [1,2,3,4]
//   B^T (col-major)@ 0x2010: [5,7,6,8]
//   暫存區         @ 0x2040
//   vl=2，逐 dot product 計算：
//     C[0,0]=19, C[0,1]=22, C[1,0]=43, C[1,1]=50
//   結果 sw 到 0x2020..0x202C
//
// 驗證：mem[0x2020..0x202C] = [19, 22, 43, 50]
// ──────────────────────────────────────────────────────────────────────────────
class VectorMatMul2x2Test extends AnyFlatSpec
    with ChiselScalatestTester with VectorTestHelper {

  behavior of "Vector CPU Integration - 2x2 Matrix Multiply"

  it should "compute C=A*B for 2x2 int32 matrices using vmul.vv + scalar add" in {
    test(new VectorTestTopModule("matmul2x2.asmbin"))
        .withAnnotations(TestAnnotations.annos) { c =>
      waitForCompletion(c)

      // 印出相關 VRF 狀態輔助 debug
      println("=== VRF Debug: A rows / B cols / intermediate ===")
      printVReg(c, vregIdx = 1, vl = 2)   // A row 0 or row 1
      printVReg(c, vregIdx = 2, vl = 2)   // B col 0 or col 1
      printVReg(c, vregIdx = 3, vl = 2)   // product intermediate

      // C = [[19,22],[43,50]] row-major
      checkMemElements(c, baseAddr = 0x2020, expected = Seq(19, 22, 43, 50))
    }
  }
}
