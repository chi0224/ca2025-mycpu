package riscv.core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class VectorCSRTest extends AnyFlatSpec with ChiselScalatestTester {

  // 你的設定：VLEN=128, SEW=32, LMUL=1 → VLMAX=4
  val VLMAX = 4

  // zimm 編碼：SEW32(vsew=010=0x08), LMUL1(vlmul=000) → zimm[5:3]=010, [2:0]=000
  // zimm = 0b000_0001_0000 = 0x010
  val ZIMM_SEW32_LMUL1 = 0x010

  // ──────────────────────────────────────────────
  // Part 1: Directed Smoke Tests
  // ──────────────────────────────────────────────
  behavior of "VectorCSR - Directed Smoke Tests"

  it should "output correct VLMAX on reset" in {
    test(new VectorCSR) { dut =>
      dut.io.is_vsetvli.poke(false.B)
      dut.io.rs1_data.poke(0.U)
      dut.io.zimm.poke(0.U)
      dut.clock.step(1)
      dut.io.vlmax.expect(VLMAX.U)
    }
  }

  it should "set vl=AVL when AVL <= VLMAX" in {
    test(new VectorCSR) { dut =>
      dut.io.is_vsetvli.poke(true.B)
      dut.io.rs1_data.poke(3.U)   // AVL=3, 3 <= 4=VLMAX
      dut.io.zimm.poke(ZIMM_SEW32_LMUL1.U)
      dut.clock.step(1)
      dut.io.vl.expect(3.U)
    }
  }

  it should "cap vl=VLMAX when AVL > VLMAX" in {
    test(new VectorCSR) { dut =>
      dut.io.is_vsetvli.poke(true.B)
      dut.io.rs1_data.poke(10.U)  // AVL=10 > VLMAX=4 → 應 cap 到 4
      dut.io.zimm.poke(ZIMM_SEW32_LMUL1.U)
      dut.clock.step(1)
      dut.io.vl.expect(VLMAX.U)
    }
  }

  it should "set vl=VLMAX when AVL=0 (rs1=x0 語意)" in {
    test(new VectorCSR) { dut =>
      dut.io.is_vsetvli.poke(true.B)
      dut.io.rs1_data.poke(0.U)   // AVL=0 → 你的硬體 Mux(avl===0, VLMAX, avl)
      dut.io.zimm.poke(ZIMM_SEW32_LMUL1.U)
      dut.clock.step(1)
      dut.io.vl.expect(VLMAX.U)
    }
  }

  it should "set vl=VLMAX when AVL=4 (exactly VLMAX)" in {
    test(new VectorCSR) { dut =>
      dut.io.is_vsetvli.poke(true.B)
      dut.io.rs1_data.poke(4.U)
      dut.io.zimm.poke(ZIMM_SEW32_LMUL1.U)
      dut.clock.step(1)
      dut.io.vl.expect(4.U)
    }
  }

  it should "not update vl when is_vsetvli=false" in {
    test(new VectorCSR) { dut =>
      // 先設 vl=2
      dut.io.is_vsetvli.poke(true.B)
      dut.io.rs1_data.poke(2.U)
      dut.io.zimm.poke(ZIMM_SEW32_LMUL1.U)
      dut.clock.step(1)
      dut.io.vl.expect(2.U)

      // 關掉 is_vsetvli，換 rs1_data 也不應該改變 vl
      dut.io.is_vsetvli.poke(false.B)
      dut.io.rs1_data.poke(99.U)
      dut.clock.step(1)
      dut.io.vl.expect(2.U)  // 保持 2
    }
  }

  it should "update vtype register with zimm[7:0]" in {
    test(new VectorCSR) { dut =>
      dut.io.is_vsetvli.poke(true.B)
      dut.io.rs1_data.poke(1.U)
      dut.io.zimm.poke(ZIMM_SEW32_LMUL1.U)
      dut.clock.step(1)
      // vtype = zimm[7:0] = 0x10 (SEW32, LMUL1)
      dut.io.vtype.expect((ZIMM_SEW32_LMUL1 & 0xFF).U)
    }
  }

  // ──────────────────────────────────────────────
  // Part 2: Golden Model Random Tests
  // ──────────────────────────────────────────────
  behavior of "VectorCSR - Golden Model Random Verification"

  /** 軟體 Golden Model：模擬 VectorCSR 的 vl 設定邏輯 */
  def goldenVL(avl: Long, vlmax: Long): Long = {
    // 對應你的 Chisel：Mux(avl === 0, vlmaxval, Mux(avl > vlmax, vlmax, avl))
    if (avl == 0)       vlmax
    else if (avl > vlmax) vlmax
    else                avl
  }

  it should "pass 500 random vsetvli tests against golden model" in {
    val rng = new Random(seed = 123)

    test(new VectorCSR) { dut =>
      for (iter <- 0 until 500) {
        val avl  = (rng.nextLong() & 0xFFL)  // 0~255
        val doSet = rng.nextBoolean()

        dut.io.is_vsetvli.poke(doSet.B)
        dut.io.rs1_data.poke(avl.U)
        dut.io.zimm.poke(ZIMM_SEW32_LMUL1.U)
        dut.clock.step(1)

        if (doSet) {
          val expectedVL = goldenVL(avl, VLMAX)
          val gotVL      = dut.io.vl.peek().litValue.toLong

          assert(
            gotVL == expectedVL,
            f"""
[MISMATCH] iter=$iter  avl=$avl  doSet=$doSet
  expected vl = $expectedVL
  got      vl = $gotVL
"""
          )
        }
        // is_vsetvli=false 時不需要檢查（vl 應維持不變）
      }
      println("[PASS] VectorCSR: 500 random vsetvli tests passed")
    }
  }

  // ──────────────────────────────────────────────
  // Part 3: Corner Cases
  // ──────────────────────────────────────────────
  behavior of "VectorCSR - Corner Cases"

  it should "vl=1 (minimum nonzero AVL)" in {
    test(new VectorCSR) { dut =>
      dut.io.is_vsetvli.poke(true.B)
      dut.io.rs1_data.poke(1.U)
      dut.io.zimm.poke(ZIMM_SEW32_LMUL1.U)
      dut.clock.step(1)
      dut.io.vl.expect(1.U)
    }
  }

  it should "vl stays unchanged across multiple non-vsetvli cycles" in {
    test(new VectorCSR) { dut =>
      // 設成 3
      dut.io.is_vsetvli.poke(true.B)
      dut.io.rs1_data.poke(3.U)
      dut.io.zimm.poke(ZIMM_SEW32_LMUL1.U)
      dut.clock.step(1)
      dut.io.vl.expect(3.U)

      // 連續 5 個 cycle 不更新
      dut.io.is_vsetvli.poke(false.B)
      for (_ <- 0 until 5) {
        dut.clock.step(1)
        dut.io.vl.expect(3.U)
      }
    }
  }

  it should "large AVL (0xFF=255) correctly caps to VLMAX" in {
    test(new VectorCSR) { dut =>
      dut.io.is_vsetvli.poke(true.B)
      dut.io.rs1_data.poke(255.U)
      dut.io.zimm.poke(ZIMM_SEW32_LMUL1.U)
      dut.clock.step(1)
      dut.io.vl.expect(VLMAX.U)
    }
  }
}
