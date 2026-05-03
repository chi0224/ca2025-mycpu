package riscv.core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class VectorALUTest extends AnyFlatSpec with ChiselScalatestTester {
  val NumLanes  = 4
  val ELENBits  = 32
  val MASK32    = BigInt("FFFFFFFF", 16)

  /** 4 個 32-bit Int → 128-bit BigInt（lane0 在 LSB） */
  def makeVec(e0: Int, e1: Int, e2: Int, e3: Int): BigInt = {
    val elems = Seq(e0, e1, e2, e3)
    elems.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (e, i)) =>
      acc | ((BigInt(e) & MASK32) << (i * ELENBits))
    }
  }

  /** 128-bit BigInt → 4 個 32-bit Long（方便逐 lane 比對） */
  def unpack(v: BigInt): Seq[Long] =
    (0 until NumLanes).map(i => ((v >> (i * ELENBits)) & MASK32).toLong)

  // ─────────────────────────────────────────────────────────
  // ★ Golden Model（軟體參考模型）
  //   用純 Scala 實作和硬體相同的語意，作為比對基準
  // ─────────────────────────────────────────────────────────
  def golden(
    vs1: Seq[Long], vs2: Seq[Long], vd: Seq[Long],
    op: Int, vl: Int
  ): Seq[Long] = {
    (0 until NumLanes).map { i =>
      if (i >= vl) {
        vd(i)  // tail-undisturbed
      } else {
        val r = op match {
          case 0 => vs2(i) + vs1(i)          // ADD: vadd.vv
          case 1 => vs2(i) * vs1(i)          // MUL: vmul.vv
          case 2 => vd(i) + vs1(i) * vs2(i) // MACC: vmacc.vv
          case _ => 0L
        }
        r & 0xFFFFFFFFL  // 32-bit 截斷（模擬硬體 overflow 行為）
      }
    }
  }

  // ─────────────────────────────────────────────────────────
  // 隨機測試引擎（可重用於三種 op）
  // ─────────────────────────────────────────────────────────
  def runRandomTests(opName: String, opCode: Int, numTests: Int = 1000): Unit = {
    val rng = new Random(seed = 42)  // 固定 seed → reproducible，CI 不會 flaky

    test(new VectorALU) { dut =>
      for (iter <- 0 until numTests) {
        // constrained random：值域 0 ~ 0xFFFF 避免 MUL overflow 太難預測
        val vs1 = Seq.fill(NumLanes)((rng.nextLong() & 0xFFFFL))
        val vs2 = Seq.fill(NumLanes)((rng.nextLong() & 0xFFFFL))
        val vd  = Seq.fill(NumLanes)((rng.nextLong() & 0xFFFFFFFFL))
        val vl  = rng.nextInt(NumLanes + 1)  // 0 ~ 4

        dut.io.vs1.poke(
          vs1.zipWithIndex.foldLeft(BigInt(0)){ case (a,(e,i)) => a | (BigInt(e) << (i*ELENBits)) }.U
        )
        dut.io.vs2.poke(
          vs2.zipWithIndex.foldLeft(BigInt(0)){ case (a,(e,i)) => a | (BigInt(e) << (i*ELENBits)) }.U
        )
        dut.io.vd_in.poke(
          vd.zipWithIndex.foldLeft(BigInt(0)){ case (a,(e,i)) => a | (BigInt(e) << (i*ELENBits)) }.U
        )
        dut.io.op.poke(opCode.U)
        dut.io.vl.poke(vl.U)
        dut.clock.step(1)

        // Golden Model 計算期望值
        val expected = golden(vs1, vs2, vd, opCode, vl)
        val got      = unpack(dut.io.result.peek().litValue)

        // 逐 lane 比對，失敗時印完整 debug 資訊
        for (lane <- 0 until NumLanes) {
          assert(
            got(lane) == expected(lane),
            f"""
[MISMATCH] op=$opName  iter=$iter  vl=$vl  lane=$lane
  vs1       = ${vs1.mkString("[", ", ", "]")}
  vs2       = ${vs2.mkString("[", ", ", "]")}
  vd_in     = ${vd.mkString("[", ", ", "]")}
  expected[$lane] = ${expected(lane)}%d  (0x${expected(lane).toHexString})
  got     [$lane] = ${got(lane)}%d  (0x${got(lane).toHexString})
"""
          )
        }
      }
      println(f"[PASS] $opName: $numTests random tests passed")
    }
  }

  // ─────────────────────────────────────────────────────────
  // Part 1: Directed Tests
  // ─────────────────────────────────────────────────────────
  behavior of "VectorALU - Directed Smoke Tests"

  it should "perform vadd.vv with vl=4" in {
    test(new VectorALU) { dut =>
      dut.io.vs1.poke(makeVec(1, 2, 3, 4).U)
      dut.io.vs2.poke(makeVec(10, 20, 30, 40).U)
      dut.io.vd_in.poke(0.U)
      dut.io.op.poke(VectorALUOp.ADD)
      dut.io.vl.poke(4.U)
      dut.clock.step(1)
      dut.io.result.expect(makeVec(11, 22, 33, 44).U)
    }
  }

  it should "keep tail elements unchanged when vl=2 (tail-undisturbed)" in {
    test(new VectorALU) { dut =>
      dut.io.vs1.poke(makeVec(1, 2, 3, 4).U)
      dut.io.vs2.poke(makeVec(10, 20, 30, 40).U)
      dut.io.vd_in.poke(makeVec(99, 88, 77, 66).U)
      dut.io.op.poke(VectorALUOp.ADD)
      dut.io.vl.poke(2.U)
      dut.clock.step(1)
      dut.io.result.expect(makeVec(11, 22, 77, 66).U)
    }
  }

  it should "produce all-tail result when vl=0" in {
    test(new VectorALU) { dut =>
      dut.io.vs1.poke(makeVec(1, 2, 3, 4).U)
      dut.io.vs2.poke(makeVec(10, 20, 30, 40).U)
      dut.io.vd_in.poke(makeVec(55, 66, 77, 88).U)
      dut.io.op.poke(VectorALUOp.ADD)
      dut.io.vl.poke(0.U)
      dut.clock.step(1)
      dut.io.result.expect(makeVec(55, 66, 77, 88).U)
    }
  }

  it should "perform vmul.vv with vl=4" in {
    test(new VectorALU) { dut =>
      dut.io.vs1.poke(makeVec(2, 3, 4, 5).U)
      dut.io.vs2.poke(makeVec(10, 10, 10, 10).U)
      dut.io.vd_in.poke(0.U)
      dut.io.op.poke(VectorALUOp.MUL)
      dut.io.vl.poke(4.U)
      dut.clock.step(1)
      dut.io.result.expect(makeVec(20, 30, 40, 50).U)
    }
  }

  it should "perform vmacc.vv with vl=4 (vd = vd + vs1 * vs2)" in {
    test(new VectorALU) { dut =>
      dut.io.vs1.poke(makeVec(2, 3, 4, 5).U)
      dut.io.vs2.poke(makeVec(10, 10, 10, 10).U)
      dut.io.vd_in.poke(makeVec(100, 100, 100, 100).U)
      dut.io.op.poke(VectorALUOp.MACC)
      dut.io.vl.poke(4.U)
      dut.clock.step(1)
      dut.io.result.expect(makeVec(120, 130, 140, 150).U)
    }
  }

  // ─────────────────────────────────────────────────────────
  // Part 2: Golden Model Random Tests
  // ─────────────────────────────────────────────────────────
  behavior of "VectorALU - Golden Model Random Verification"

  it should "pass 1000 random tests for vadd.vv" in {
    runRandomTests("vadd.vv", opCode = VectorALUOp.ADD.litValue.toInt)
  }

  it should "pass 1000 random tests for vmul.vv" in {
    runRandomTests("vmul.vv", opCode = VectorALUOp.MUL.litValue.toInt)
  }

  it should "pass 1000 random tests for vmacc.vv" in {
    runRandomTests("vmacc.vv", opCode = VectorALUOp.MACC.litValue.toInt)
  }

  // ─────────────────────────────────────────────────────────
  // Part 3: Corner Cases
  // ─────────────────────────────────────────────────────────
  behavior of "VectorALU - Corner Cases"

  it should "handle 32-bit overflow in vadd (wrap around)" in {
    test(new VectorALU) { dut =>
      // 0xFFFFFFFF + 1 應該 wrap 成 0x00000000
      dut.io.vs1.poke(makeVec(1, 1, 1, 1).U)
      dut.io.vs2.poke(makeVec(-1, -1, -1, -1).U)  // -1 = 0xFFFFFFFF
      dut.io.vd_in.poke(0.U)
      dut.io.op.poke(VectorALUOp.ADD)
      dut.io.vl.poke(4.U)
      dut.clock.step(1)
      dut.io.result.expect(makeVec(0, 0, 0, 0).U)
    }
  }

  it should "handle multiply by zero" in {
    test(new VectorALU) { dut =>
      dut.io.vs1.poke(makeVec(0, 0, 0, 0).U)
      dut.io.vs2.poke(makeVec(999, 999, 999, 999).U)
      dut.io.vd_in.poke(0.U)
      dut.io.op.poke(VectorALUOp.MUL)
      dut.io.vl.poke(4.U)
      dut.clock.step(1)
      dut.io.result.expect(makeVec(0, 0, 0, 0).U)
    }
  }

  it should "vmacc with vl=1 only accumulates lane0, rest are tail" in {
    test(new VectorALU) { dut =>
      dut.io.vs1.poke(makeVec(3, 3, 3, 3).U)
      dut.io.vs2.poke(makeVec(4, 4, 4, 4).U)
      dut.io.vd_in.poke(makeVec(10, 20, 30, 40).U)
      dut.io.op.poke(VectorALUOp.MACC)
      dut.io.vl.poke(1.U)
      dut.clock.step(1)
      // lane0: 10 + 3*4 = 22，lane1~3 保持 vd_in
      dut.io.result.expect(makeVec(22, 20, 30, 40).U)
    }
  }
}
