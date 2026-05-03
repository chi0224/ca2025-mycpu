package riscv.core

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.Parameters

class VectorMemoryAccessTest extends AnyFlatSpec with ChiselScalatestTester {

  // ──────────────────────────────────────────────
  // 共用 helper
  // ──────────────────────────────────────────────
  /** 把 vl 個 32-bit elements 打包成 128-bit UInt（小端） */
  def packElements(elems: Seq[Long]): BigInt = {
    require(elems.length <= Parameters.NumLanes)
    elems.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (v, i)) =>
      acc | (BigInt(v & 0xFFFFFFFFL) << (i * 32))
    }
  }

  /** 從 128-bit BigInt 取出第 i 個 32-bit element */
  def getElem(data: BigInt, i: Int): Long =
    ((data >> (i * 32)) & 0xFFFFFFFFL).toLong

  // ──────────────────────────────────────────────
  // Part 1: vle32.v — Load path
  // ──────────────────────────────────────────────
  behavior of "MemoryAccess - vle32 (vector load)"

  it should "assert read address and present readdata to wb_vector_read_data" in {
    test(new MemoryAccess) { dut =>
      val baseAddr = 0x1000L

      // 設定向量 load 模式
      dut.io.is_vle32.poke(true.B)
      dut.io.is_vse32.poke(false.B)
      dut.io.memory_read_enable.poke(false.B)
      dut.io.memory_write_enable.poke(false.B)
      dut.io.alu_result.poke(baseAddr.U)
      dut.io.vl.poke(4.U)

      // 驗證 vector bus address = alu_result
      dut.io.vector_memory_bundle.address.expect(baseAddr.U)
      dut.io.vector_memory_bundle.write_enable.expect(false.B)

      // 模擬 memory 回傳 128-bit 資料
      val memData = packElements(Seq(0x11111111L, 0x22222222L, 0x33333333L, 0x44444444L))
      dut.io.vector_memory_bundle.read_data.poke(memData.U)

      // wb_vector_read_data 直接 wire 過去
      dut.io.wb_vector_read_data.expect(memData.U)
    }
  }

  it should "not drive scalar memory bus during vle32" in {
    test(new MemoryAccess) { dut =>
      dut.io.is_vle32.poke(true.B)
      dut.io.is_vse32.poke(false.B)
      dut.io.memory_read_enable.poke(false.B)
      dut.io.memory_write_enable.poke(false.B)
      dut.io.alu_result.poke(0x2000L.U)
      dut.io.vl.poke(4.U)

      // scalar bus 不應被驅動寫
      dut.io.memory_bundle.write_enable.expect(false.B)
    }
  }

  // ──────────────────────────────────────────────
  // Part 2: vse32.v — Store path
  // ──────────────────────────────────────────────
  behavior of "MemoryAccess - vse32 (vector store)"

  it should "drive write_enable, address, write_data on vector bus" in {
    test(new MemoryAccess) { dut =>
      val baseAddr  = 0x3000L
      val storeData = packElements(Seq(0xAAAAAAAAL, 0xBBBBBBBBL, 0xCCCCCCCCL, 0xDDDDDDDDL))

      dut.io.is_vle32.poke(false.B)
      dut.io.is_vse32.poke(true.B)
      dut.io.memory_read_enable.poke(false.B)
      dut.io.memory_write_enable.poke(false.B)
      dut.io.alu_result.poke(baseAddr.U)
      dut.io.vreg_store_data.poke(storeData.U)
      dut.io.vl.poke(4.U)

      dut.io.vector_memory_bundle.write_enable.expect(true.B)
      dut.io.vector_memory_bundle.address.expect(baseAddr.U)
      dut.io.vector_memory_bundle.write_data.expect(storeData.U)
    }
  }

  it should "generate correct write strobes for vl=4 (all 16 bytes active)" in {
    test(new MemoryAccess) { dut =>
      dut.io.is_vle32.poke(false.B)
      dut.io.is_vse32.poke(true.B)
      dut.io.memory_read_enable.poke(false.B)
      dut.io.memory_write_enable.poke(false.B)
      dut.io.alu_result.poke(0x4000L.U)
      dut.io.vreg_store_data.poke(0.U)
      dut.io.vl.poke(4.U)    // 4 elements × 4 bytes = 16 bytes → strobe[15:0] = all 1

      for (i <- 0 until Parameters.VMemStrobeBytes) {
        dut.io.vector_memory_bundle.write_strobe(i).expect(true.B,
          s"strobe[$i] should be true for vl=4")
      }
    }
  }

  it should "generate correct write strobes for vl=2 (only 8 bytes active)" in {
    test(new MemoryAccess) { dut =>
      dut.io.is_vle32.poke(false.B)
      dut.io.is_vse32.poke(true.B)
      dut.io.memory_read_enable.poke(false.B)
      dut.io.memory_write_enable.poke(false.B)
      dut.io.alu_result.poke(0x4000L.U)
      dut.io.vreg_store_data.poke(0.U)
      dut.io.vl.poke(2.U)    // 2 elements × 4 bytes = 8 bytes → strobe[7:0]=1, strobe[15:8]=0

      for (i <- 0 until 8)
        dut.io.vector_memory_bundle.write_strobe(i).expect(true.B,  s"strobe[$i] should be 1 for vl=2")
      for (i <- 8 until Parameters.VMemStrobeBytes)
        dut.io.vector_memory_bundle.write_strobe(i).expect(false.B, s"strobe[$i] should be 0 for vl=2")
    }
  }

  it should "generate correct write strobes for vl=1 (only 4 bytes active)" in {
    test(new MemoryAccess) { dut =>
      dut.io.is_vle32.poke(false.B)
      dut.io.is_vse32.poke(true.B)
      dut.io.memory_read_enable.poke(false.B)
      dut.io.memory_write_enable.poke(false.B)
      dut.io.alu_result.poke(0x4000L.U)
      dut.io.vreg_store_data.poke(0.U)
      dut.io.vl.poke(1.U)    // 1 element × 4 bytes = 4 bytes → strobe[3:0]=1, rest=0

      for (i <- 0 until 4)
        dut.io.vector_memory_bundle.write_strobe(i).expect(true.B,  s"strobe[$i] should be 1 for vl=1")
      for (i <- 4 until Parameters.VMemStrobeBytes)
        dut.io.vector_memory_bundle.write_strobe(i).expect(false.B, s"strobe[$i] should be 0 for vl=1")
    }
  }

  it should "generate correct write strobes for vl=3 (12 bytes active)" in {
    test(new MemoryAccess) { dut =>
      dut.io.is_vle32.poke(false.B)
      dut.io.is_vse32.poke(true.B)
      dut.io.memory_read_enable.poke(false.B)
      dut.io.memory_write_enable.poke(false.B)
      dut.io.alu_result.poke(0x4000L.U)
      dut.io.vreg_store_data.poke(0.U)
      dut.io.vl.poke(3.U)    // 3 elements × 4 bytes = 12 bytes → strobe[11:0]=1, [15:12]=0

      for (i <- 0 until 12)
        dut.io.vector_memory_bundle.write_strobe(i).expect(true.B,  s"strobe[$i] should be 1 for vl=3")
      for (i <- 12 until Parameters.VMemStrobeBytes)
        dut.io.vector_memory_bundle.write_strobe(i).expect(false.B, s"strobe[$i] should be 0 for vl=3")
    }
  }

  // ──────────────────────────────────────────────
  // Part 3: 互斥性驗證
  // ──────────────────────────────────────────────
  behavior of "MemoryAccess - vector/scalar mutual exclusion"

  it should "not assert vector write_enable when scalar store is active" in {
    test(new MemoryAccess) { dut =>
      dut.io.is_vle32.poke(false.B)
      dut.io.is_vse32.poke(false.B)
      dut.io.memory_read_enable.poke(false.B)
      dut.io.memory_write_enable.poke(true.B)  // scalar SW
      dut.io.alu_result.poke(0x1000L.U)
      dut.io.reg2_data.poke(0xDEADBEEFL.U)
      dut.io.funct3.poke(2.U)  // SW
      dut.io.vl.poke(0.U)

      dut.io.vector_memory_bundle.write_enable.expect(false.B)
    }
  }

  it should "not assert scalar write_enable when vse32 is active" in {
    test(new MemoryAccess) { dut =>
      dut.io.is_vle32.poke(false.B)
      dut.io.is_vse32.poke(true.B)
      dut.io.memory_read_enable.poke(false.B)
      dut.io.memory_write_enable.poke(false.B)
      dut.io.alu_result.poke(0x1000L.U)
      dut.io.vreg_store_data.poke(0.U)
      dut.io.vl.poke(4.U)

      dut.io.memory_bundle.write_enable.expect(false.B)
    }
  }

  // ──────────────────────────────────────────────
  // Part 4: vl=0 edge case
  // ──────────────────────────────────────────────
  behavior of "MemoryAccess - vector edge cases"

  it should "vse32 with vl=0 produces all-zero strobes" in {
    test(new MemoryAccess) { dut =>
      dut.io.is_vle32.poke(false.B)
      dut.io.is_vse32.poke(true.B)
      dut.io.memory_read_enable.poke(false.B)
      dut.io.memory_write_enable.poke(false.B)
      dut.io.alu_result.poke(0x5000L.U)
      dut.io.vreg_store_data.poke(0.U)
      dut.io.vl.poke(0.U)  // vl=0 → 不寫任何 element

      for (i <- 0 until Parameters.VMemStrobeBytes)
        dut.io.vector_memory_bundle.write_strobe(i).expect(false.B,
          s"strobe[$i] should be 0 when vl=0")
    }
  }

  it should "vle32 passes through all 128 bits from memory unchanged" in {
    test(new MemoryAccess) { dut =>
      dut.io.is_vle32.poke(true.B)
      dut.io.is_vse32.poke(false.B)
      dut.io.memory_read_enable.poke(false.B)
      dut.io.memory_write_enable.poke(false.B)
      dut.io.alu_result.poke(0x8000L.U)
      dut.io.vl.poke(4.U)

      // 全 F 的 128-bit pattern
      val allF = (BigInt(1) << 128) - 1
      dut.io.vector_memory_bundle.read_data.poke(allF.U)
      dut.io.wb_vector_read_data.expect(allF.U)

      // 全 0
      dut.io.vector_memory_bundle.read_data.poke(0.U)
      dut.io.wb_vector_read_data.expect(0.U)
    }
  }
}
