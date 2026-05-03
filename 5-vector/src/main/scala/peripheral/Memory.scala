// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package peripheral

import chisel3._
import chisel3.util._
import riscv.Parameters

class RAMBundle extends Bundle {
  val address      = Input(UInt(Parameters.AddrWidth))
  val write_data   = Input(UInt(Parameters.DataWidth))
  val write_enable = Input(Bool())
  val write_strobe = Input(Vec(Parameters.WordSize, Bool()))
  val read_data    = Output(UInt(Parameters.DataWidth))
}

// VMemDataWidth = VLEN = 128-bit
// VMemStrobeBytes = VLEN/8 = 16 bytes
class VectorRAMBundle extends Bundle {
  val address      = Input(UInt(Parameters.AddrWidth))
  // byte-addressable，4-byte aligned（與純量 LW 對齊要求相同，RVV spec §7.2）
  val write_data   = Input(UInt(Parameters.VMemDataWidth))
  // 128-bit 一次寫入 4 個 element
  val write_enable = Input(Bool())
  val write_strobe = Input(Vec(Parameters.VMemStrobeBytes, Bool()))
  // 16 個 byte strobe，對應 16 個 byte
  // vse32 時根據 vl 決定幾個有效
  val read_data    = Output(UInt(Parameters.VMemDataWidth))
  // 128-bit 一次讀出 4 個 element
}

// The purpose of this module is to help the synthesis tool recognize
// our memory as a Block RAM template
class BlockRAM(capacity: Int) extends Module {
  val io = IO(new Bundle {
    val read_address  = Input(UInt(Parameters.AddrWidth))
    val write_address = Input(UInt(Parameters.AddrWidth))
    val write_data    = Input(UInt(Parameters.DataWidth))
    val write_enable  = Input(Bool())
    val write_strobe  = Input(Vec(Parameters.WordSize, Bool()))

    val debug_read_address = Input(UInt(Parameters.AddrWidth))

    val read_data       = Output(UInt(Parameters.DataWidth))
    val debug_read_data = Output(UInt(Parameters.DataWidth))
  })
  val mem = SyncReadMem(capacity, Vec(Parameters.WordSize, UInt(Parameters.ByteWidth)))
  when(io.write_enable) {
    val write_data_vec = Wire(Vec(Parameters.WordSize, UInt(Parameters.ByteWidth)))
    for (i <- 0 until Parameters.WordSize) {
      write_data_vec(i) := io.write_data((i + 1) * Parameters.ByteBits - 1, i * Parameters.ByteBits)
    }
    mem.write((io.write_address >> 2.U).asUInt, write_data_vec, io.write_strobe)
  }
  io.read_data       := mem.read((io.read_address >> 2.U).asUInt, true.B).asUInt
  io.debug_read_data := mem.read((io.debug_read_address >> 2.U).asUInt, true.B).asUInt
}

// Memory module: unified instruction and data memory with bounds checking
//
// Features:
// - Synchronous read memory (1-cycle latency)
// - Byte-addressable with byte-level write strobes
// - Separate ports for instruction fetch, data access, and debug
// - Memory bounds validation prevents out-of-bounds corruption
//
// Address mapping:
// - Byte addresses divided by 4 (>> 2) to get word addresses
// - capacity parameter specifies number of 32-bit words
// - Out-of-bounds addresses clamped to 0 for safe reads
class Memory(capacity: Int) extends Module {
  val io = IO(new Bundle {
    val bundle = new RAMBundle
    val vector_bundle = new VectorRAMBundle

    val instruction         = Output(UInt(Parameters.DataWidth))
    val instruction_address = Input(UInt(Parameters.AddrWidth))

    val debug_read_address = Input(UInt(Parameters.AddrWidth))
    val debug_read_data    = Output(UInt(Parameters.DataWidth))
  })

  val mem = SyncReadMem(capacity, Vec(Parameters.WordSize, UInt(Parameters.ByteWidth)))

  // Memory bounds checking: capacity is in words, addresses are word-aligned (>> 2)
  val max_word_address = (capacity - 1).U

  when(io.bundle.write_enable) {
    val write_data_vec = Wire(Vec(Parameters.WordSize, UInt(Parameters.ByteWidth)))
    for (i <- 0 until Parameters.WordSize) {
      write_data_vec(i) := io.bundle.write_data((i + 1) * Parameters.ByteBits - 1, i * Parameters.ByteBits)
    }
    val write_word_addr = (io.bundle.address >> 2.U).asUInt
    // Only write if address is within bounds
    when(write_word_addr <= max_word_address) {
      mem.write(write_word_addr, write_data_vec, io.bundle.write_strobe)
    }
  }

  //Vector Write Path
   when(io.vector_bundle.write_enable) {
    val base_word_addr = (io.vector_bundle.address >> 2.U).asUInt
    // 128-bit 的 base address 轉成 word address
    // 例：address=0x2000 → base_word_addr=0x800

    for (i <- 0 until Parameters.NumLanes) {
      // NumLanes = 4，對應 4 個 32-bit element
      // 每次迭代寫入一個 word（32-bit）

      val word_addr = base_word_addr + i.U
      // word 0：base+0，word 1：base+1，word 2：base+2，word 3：base+3

      val element_data = io.vector_bundle.write_data((i + 1) * Parameters.ELENBits - 1, i * Parameters.ELENBits)
      // 從 128-bit write_data 取出第 i 個 32-bit element
      // i=0：bits[31:0]，i=1：bits[63:32]，...

      val element_data_vec = Wire(Vec(Parameters.WordSize, UInt(Parameters.ByteWidth)))
      for (j <- 0 until Parameters.WordSize) {
        element_data_vec(j) := element_data((j + 1) * Parameters.ByteBits - 1, j * Parameters.ByteBits)
      }
      // 把 32-bit element 拆成 4 個 byte，配合 SyncReadMem 格式

      val element_strobe = io.vector_bundle.write_strobe.slice(i * Parameters.WordSize, (i + 1) * Parameters.WordSize)
      // 從 16-bit strobe 取出對應這個 element 的 4 個 byte strobe
      // element 0：strobe[3:0]，element 1：strobe[7:4]，...
      // vl=3 時：strobe[11:0] 有效，strobe[15:12]=0
      // 所以 element 3（i=3）的 strobe[15:12] 全為 0，這個 word 不會被寫入

      when(word_addr <= max_word_address) {
        mem.write(word_addr, element_data_vec, VecInit(element_strobe))
        // bounds check 後才寫入
      }
    }
  }

  // Clamp read addresses to valid range to prevent out-of-bounds access
  val read_word_addr = Mux(
    (io.bundle.address >> 2.U).asUInt <= max_word_address,
    (io.bundle.address >> 2.U).asUInt,
    0.U
  )
  val debug_word_addr = Mux(
    (io.debug_read_address >> 2.U).asUInt <= max_word_address,
    (io.debug_read_address >> 2.U).asUInt,
    0.U
  )
  val inst_word_addr = Mux(
    (io.instruction_address >> 2.U).asUInt <= max_word_address,
    (io.instruction_address >> 2.U).asUInt,
    0.U
  )

  io.bundle.read_data := mem.read(read_word_addr, true.B).asUInt
  io.debug_read_data  := mem.read(debug_word_addr, true.B).asUInt
  io.instruction      := mem.read(inst_word_addr, true.B).asUInt

  //Vector Read Path
  val vec_base_word_addr = Mux(
    (io.vector_bundle.address >> 2.U).asUInt <= max_word_address,
    (io.vector_bundle.address >> 2.U).asUInt,
    0.U
  )

  val vec_read_words = Wire(Vec(Parameters.NumLanes, UInt(Parameters.DataWidth)))
  for (i <- 0 until Parameters.NumLanes) {
    val word_addr = Mux(
      (vec_base_word_addr + i.U) <= max_word_address,
      vec_base_word_addr + i.U,
      0.U
    )
    vec_read_words(i) := mem.read(word_addr, true.B).asUInt // 讀出第 i 個 word（32-bit）
  }

  io.vector_bundle.read_data := Cat(vec_read_words.reverse)
  // Cat(word3, word2, word1, word0)
  // word0 在 bits[31:0]（LSB），word3 在 bits[127:96]（MSB）
  // .reverse 讓 word0 排在最低位，跟 element 0 對應
}
