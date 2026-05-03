package riscv.core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class VectorRegFileTest extends AnyFlatSpec with ChiselScalatestTester {

  val VLEN     = 128
  val VRegCount = 32
  val MASK128  = (BigInt(1) << VLEN) - 1

  // ──────────────────────────────────────────────
  // Helper：寫一個 vreg 再讀回來
  // ──────────────────────────────────────────────
  def writeReg(dut: VectorRegFile, addr: Int, data: BigInt): Unit = {
    dut.io.write_enable.poke(true.B)
    dut.io.write_address.poke(addr.U)
    dut.io.write_data.poke(data.U)
    dut.clock.step(1)
    dut.io.write_enable.poke(false.B)
  }

  // ──────────────────────────────────────────────
  // Part 1: Directed Smoke Tests
  // ──────────────────────────────────────────────
  behavior of "VectorRegFile - Directed Smoke Tests"

  it should "initialize all registers to 0" in {
    test(new VectorRegFile) { dut =>
      dut.io.write_enable.poke(false.B)
      for (addr <- 0 until VRegCount) {
        dut.io.read_address_vs1.poke(addr.U)
        dut.clock.step(1)
        dut.io.read_data_vs1.expect(0.U)
      }
    }
  }

  it should "write and read back via vs1 port" in {
    test(new VectorRegFile) { dut =>
      val data = BigInt("DEADBEEFCAFEBABE1234567890ABCDEF", 16)
      writeReg(dut, 1, data)
      dut.io.read_address_vs1.poke(1.U)
      dut.clock.step(1)
      dut.io.read_data_vs1.expect(data.U)
    }
  }

  it should "write and read back via vs2 port" in {
    test(new VectorRegFile) { dut =>
      val data = BigInt("AABBCCDDEEFF00112233445566778899", 16)
      writeReg(dut, 5, data)
      dut.io.read_address_vs2.poke(5.U)
      dut.clock.step(1)
      dut.io.read_data_vs2.expect(data.U)
    }
  }

  it should "write and read back via vd port (for VMACC)" in {
    test(new VectorRegFile) { dut =>
      val data = BigInt("FEDCBA9876543210FEDCBA9876543210", 16)
      writeReg(dut, 10, data)
      dut.io.read_address_vd.poke(10.U)
      dut.clock.step(1)
      dut.io.read_data_vd.expect(data.U)
    }
  }

  it should "read 3 different registers simultaneously (3-port read)" in {
    test(new VectorRegFile) { dut =>
      // 寫入 3 個不同的暫存器
      writeReg(dut, 1, BigInt("11111111111111111111111111111111", 16))
      writeReg(dut, 2, BigInt("22222222222222222222222222222222", 16))
      writeReg(dut, 3, BigInt("33333333333333333333333333333333", 16))

      // 同時從 3 個 port 讀取
      dut.io.read_address_vs1.poke(1.U)
      dut.io.read_address_vs2.poke(2.U)
      dut.io.read_address_vd.poke(3.U)
      dut.clock.step(1)

      dut.io.read_data_vs1.expect(BigInt("11111111111111111111111111111111", 16).U)
      dut.io.read_data_vs2.expect(BigInt("22222222222222222222222222222222", 16).U)
      dut.io.read_data_vd.expect(BigInt("33333333333333333333333333333333", 16).U)
    }
  }

  it should "not write when write_enable=false" in {
    test(new VectorRegFile) { dut =>
      // write_enable=false 不能寫入
      dut.io.write_enable.poke(false.B)
      dut.io.write_address.poke(7.U)
      dut.io.write_data.poke(BigInt("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 16).U)
      dut.clock.step(1)

      dut.io.read_address_vs1.poke(7.U)
      dut.clock.step(1)
      dut.io.read_data_vs1.expect(0.U)  // 應保持 0
    }
  }

  it should "v0 is writable (unlike scalar x0)" in {
    test(new VectorRegFile) { dut =>
      val data = BigInt("CAFECAFECAFECAFECAFECAFECAFECAFE", 16)
      writeReg(dut, 0, data)
      dut.io.read_address_vs1.poke(0.U)
      dut.clock.step(1)
      dut.io.read_data_vs1.expect(data.U)  // v0 不是硬接 0
    }
  }

  it should "overwrite register with new value" in {
    test(new VectorRegFile) { dut =>
      writeReg(dut, 4, BigInt("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", 16))
      writeReg(dut, 4, BigInt("BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB", 16))
      dut.io.read_address_vs1.poke(4.U)
      dut.clock.step(1)
      dut.io.read_data_vs1.expect(BigInt("BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB", 16).U)
    }
  }

  // ──────────────────────────────────────────────
  // Part 2: Golden Model Random Tests
  // ──────────────────────────────────────────────
  behavior of "VectorRegFile - Golden Model Random Verification"

  it should "pass 500 random write-then-read tests" in {
    val rng = new Random(seed = 77)

    test(new VectorRegFile) { dut =>
      // 軟體 Golden Model：Scala Array 模擬 32 個 128-bit 暫存器
      val model = Array.fill(VRegCount)(BigInt(0))

      for (iter <- 0 until 500) {
        val doWrite = rng.nextBoolean()
        val wAddr   = rng.nextInt(VRegCount)
        val wData   = BigInt(VLEN, rng) & MASK128

        val rAddr1  = rng.nextInt(VRegCount)
        val rAddr2  = rng.nextInt(VRegCount)
        val rAddr3  = rng.nextInt(VRegCount)

        // 寫入
        dut.io.write_enable.poke(doWrite.B)
        dut.io.write_address.poke(wAddr.U)
        dut.io.write_data.poke(wData.U)
        if (doWrite) model(wAddr) = wData

        // 設定讀取位址
        dut.io.read_address_vs1.poke(rAddr1.U)
        dut.io.read_address_vs2.poke(rAddr2.U)
        dut.io.read_address_vd.poke(rAddr3.U)
        dut.clock.step(1)

        // 比對（寫完一個 cycle 後才能讀到新值）
        val got1 = dut.io.read_data_vs1.peek().litValue
        val got2 = dut.io.read_data_vs2.peek().litValue
        val got3 = dut.io.read_data_vd.peek().litValue

        assert(got1 == model(rAddr1),
          f"[MISMATCH] iter=$iter vs1: addr=$rAddr1 expected=${model(rAddr1)} got=$got1")
        assert(got2 == model(rAddr2),
          f"[MISMATCH] iter=$iter vs2: addr=$rAddr2 expected=${model(rAddr2)} got=$got2")
        assert(got3 == model(rAddr3),
          f"[MISMATCH] iter=$iter vd:  addr=$rAddr3 expected=${model(rAddr3)} got=$got3")
      }
      println("[PASS] VectorRegFile: 500 random read/write tests passed")
    }
  }

  // ──────────────────────────────────────────────
  // Part 3: Corner Cases
  // ──────────────────────────────────────────────
  behavior of "VectorRegFile - Corner Cases"

  it should "read-after-write to same address on all 3 ports" in {
    test(new VectorRegFile) { dut =>
      val data = BigInt("DEADBEEFDEADBEEFDEADBEEFDEADBEEF", 16)
      writeReg(dut, 15, data)

      // 三個 port 同時讀同一個位址
      dut.io.read_address_vs1.poke(15.U)
      dut.io.read_address_vs2.poke(15.U)
      dut.io.read_address_vd.poke(15.U)
      dut.clock.step(1)

      dut.io.read_data_vs1.expect(data.U)
      dut.io.read_data_vs2.expect(data.U)
      dut.io.read_data_vd.expect(data.U)
    }
  }

  it should "write to v31 (last register)" in {
    test(new VectorRegFile) { dut =>
      val data = BigInt("F" * 32, 16)
      writeReg(dut, 31, data)
      dut.io.read_address_vs1.poke(31.U)
      dut.clock.step(1)
      dut.io.read_data_vs1.expect(data.U)
    }
  }

  it should "write v1 does not corrupt v2 (no aliasing)" in {
    test(new VectorRegFile) { dut =>
      writeReg(dut, 2, BigInt("22222222222222222222222222222222", 16))
      writeReg(dut, 1, BigInt("11111111111111111111111111111111", 16))

      dut.io.read_address_vs1.poke(2.U)
      dut.clock.step(1)
      dut.io.read_data_vs1.expect(BigInt("22222222222222222222222222222222", 16).U)
    }
  }
}
