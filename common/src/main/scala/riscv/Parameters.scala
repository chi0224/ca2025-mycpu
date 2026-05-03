// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv

import chisel3._
import chisel3.util._

object ImplementationType {
  val ThreeStage = 0
  val FiveStage  = 1
}

object Parameters {
  val AddrBits  = 32
  val AddrWidth = AddrBits.W

  val InstructionBits  = 32
  val InstructionWidth = InstructionBits.W
  val DataBits         = 32
  val DataWidth        = DataBits.W
  val ByteBits         = 8
  val ByteWidth        = ByteBits.W
  val WordSize         = Math.ceil(DataBits / ByteBits).toInt

  val PhysicalRegisters         = 32
  val PhysicalRegisterAddrBits  = log2Up(PhysicalRegisters)
  val PhysicalRegisterAddrWidth = PhysicalRegisterAddrBits.W

  val CSRRegisterAddrBits  = 12
  val CSRRegisterAddrWidth = CSRRegisterAddrBits.W

  val InterruptFlagBits  = 32
  val InterruptFlagWidth = InterruptFlagBits.W

  val HoldStateBits   = 3
  val StallStateWidth = HoldStateBits.W

  val MemorySizeInBytes = 32768
  val MemorySizeInWords = MemorySizeInBytes / 4

  val EntryAddress = 0x1000.U(Parameters.AddrWidth)

  val MasterDeviceCount    = 1
  val SlaveDeviceCount     = 8
  val SlaveDeviceCountBits = log2Up(Parameters.SlaveDeviceCount)

  // Timer peripheral configuration
  val TimerDefaultLimit = 100000000 // 100M cycles (~1s at 100MHz)

  //Vector Extention
  val VLENBits        = 128
  val VLEN            = VLENBits.W
  val ELENBits        = 32
  val ELEN            = ELENBits.W

  val NumLanes        = VLENBits / ELENBits

  val VMemDataBits    = VLENBits
  val VMemDataWidth   = VMemDataBits.W
  val VMemStrobeBytes = VLENBits / 8

  val VRegCount          = 32
  val VRegAddrBits       = log2Up(VRegCount)
  val VRegAddrWidth      = VRegAddrBits.W

  val VLBits             = log2Up(NumLanes + 1)
  val VLWidth            = VLBits.W
}
