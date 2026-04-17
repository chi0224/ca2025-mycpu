// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv.core.fivestage_final

import chisel3._
import riscv.Parameters

/**
 * Advanced Hazard Detection and Control Unit: Maximum optimization
 *
 * Most sophisticated hazard detection supporting early branch resolution
 * in ID stage with comprehensive forwarding support. Achieves best
 * performance through aggressive optimization.
 *
 * Key Enhancements:
 * - **Early branch resolution**: Branches resolved in ID stage (not EX)
 * - **ID-stage forwarding**: Enables immediate branch operand comparison
 * - **Complex hazard detection**: Handles jump dependencies and multi-stage loads
 *
 * Hazard Types and Resolution:
 * 1. **Control Hazards**:
 *    - Branch taken in ID → flush only IF stage (1 cycle penalty)
 *    - Jump in ID → may need stall if operands not ready
 *
 * 2. **Data Hazards**:
 *    - Load-use for ALU → 1 cycle stall
 *    - Load-use for branch → 1-2 cycle stall depending on stage
 *    - Jump register dependencies → stall until operands ready
 *
 * Complex Scenarios Handled:
 *
 * Scenario 1 - Jump with load dependency:
 * ```
 * LW   x1, 0(x2)   # Load x1
 * JALR x3, x1, 0   # Jump to address in x1 → needs stall
 * ```
 *
 * Scenario 2 - Branch with recent ALU result:
 * ```
 * ADD x1, x2, x3   # Compute x1
 * BEQ x1, x4, label # Branch using x1 → forwarded to ID, no stall
 * ```
 *
 * Performance Impact:
 * - CPI ~1.05-1.2 (best achievable)
 * - Branch penalty reduced to 1 cycle
 * - Minimal stalls through aggressive forwarding
 *
 * @note Most complex control logic but best performance
 * @note Requires ID-stage forwarding paths for full benefit
 */
class Control extends Module {
  val io = IO(new Bundle {
    val jump_flag              = Input(Bool())                                     // id.io.if_jump_flag
    val jump_instruction_id    = Input(Bool())                                     // id.io.ctrl_jump_instruction           //
    val rs1_id                 = Input(UInt(Parameters.PhysicalRegisterAddrWidth)) // id.io.regs_reg1_read_address
    val rs2_id                 = Input(UInt(Parameters.PhysicalRegisterAddrWidth)) // id.io.regs_reg2_read_address
    val memory_read_enable_ex  = Input(Bool())                                     // id2ex.io.output_memory_read_enable
    val rd_ex                  = Input(UInt(Parameters.PhysicalRegisterAddrWidth)) // id2ex.io.output_regs_write_address
    val memory_read_enable_mem = Input(Bool())                                     // ex2mem.io.output_memory_read_enable   //
    val rd_mem                 = Input(UInt(Parameters.PhysicalRegisterAddrWidth)) // ex2mem.io.output_regs_write_address   //
    val uses_rs1_id            = Input(Bool())                                     // true only if current ID instruction really reads rs1
    val uses_rs2_id            = Input(Bool())                                     // true only if current ID instruction really reads rs2

    val if_flush = Output(Bool())
    val id_flush = Output(Bool())
    val pc_stall = Output(Bool())
    val if_stall = Output(Bool())
  })

  // Initialize control signals to default (no stall/flush) state
  io.if_flush := false.B
  io.id_flush := false.B
  io.pc_stall := false.B
  io.if_stall := false.B

  // ============================================================
  // [CA25: Exercise 19] Pipeline Hazard Detection
  // ============================================================
  // Hint: Detect data and control hazards, decide when to insert bubbles
  // or flush the pipeline
  //
  // Hazard types:
  // 1. Load-use hazard: Load result used immediately by next instruction
  // 2. Jump-related hazard: Jump instruction needs register value not ready
  // 3. Control hazard: Branch/jump instruction changes PC
  // Hint: For data hazards type 1 and 2, check for register dependencies
  //   Use the decoded `uses_rs1_id` / `uses_rs2_id` flags to test whether the
  //   current ID-stage instruction actually reads rs1/rs2; only then should a
  //   register-number match be considered a true RAW dependency.
  //
  // Control signals:
  // - pc_stall: Freeze PC (don't fetch next instruction)
  // - if_stall: Freeze IF/ID register (hold current fetch result)
  // - id_flush: Flush ID/EX register (insert NOP bubble)
  // - if_flush: Flush IF/ID register (discard wrong-path instruction)

  // Complex hazard detection for early branch resolution in ID stage
  when(
    // ============ Complex Hazard Detection Logic ============
    // This condition detects multiple hazard scenarios requiring stalls:

    // --- Condition 1: EX stage hazards (1-cycle dependencies) ---
    // TODO: Complete hazard detection conditions
    // Need to detect:
    // 1. Jump instruction in ID stage
    // 2. OR Load instruction in EX stage
    // 3. AND destination register is not x0
    // 4. AND destination register conflicts with ID source registers
    //
    ((io.memory_read_enable_ex || io.jump_instruction_id) && // Either:
      // - Jump in ID needs register value, OR
      // - Load in EX (load-use hazard)
      (io.rd_ex =/= 0.U) &&                                                                          // Destination is not x0
      ((io.uses_rs1_id && (io.rd_ex === io.rs1_id)) || (io.uses_rs2_id && (io.rd_ex === io.rs2_id))) // Destination matches ID source
    )
    //
    // Examples triggering Condition 1:
    // a) Jump dependency: ADD x1, x2, x3 [EX]; JALR x0, x1, 0 [ID] → stall
    // b) Load-use: LW x1, 0(x2) [EX]; ADD x3, x1, x4 [ID] → stall
    // c) Load-branch: LW x1, 0(x2) [EX]; BEQ x1, x4, label [ID] → stall

      || // OR

        // --- Condition 2: MEM stage load with jump dependency (2-cycle) ---
        // TODO: Complete MEM stage hazard detection
        // Need to detect:
        // 1. Jump instruction in ID stage
        // 2. Load instruction in MEM stage
        // 3. Destination register is not x0
        // 4. Destination register conflicts with ID source registers
        //
        (io.jump_instruction_id &&                                                                         // Jump instruction in ID
          io.memory_read_enable_mem &&                                                                     // Load instruction in MEM
          (io.rd_mem =/= 0.U) &&                                                                           // Load destination not x0
          ((io.uses_rs1_id && (io.rd_mem === io.rs1_id)) || (io.uses_rs2_id && (io.rd_mem === io.rs2_id))) // Load dest matches jump source
        )
        //
        // Example triggering Condition 2:
        // LW x1, 0(x2) [MEM]; NOP [EX]; JALR x0, x1, 0 [ID]
        // Even with forwarding, load result needs extra cycle to reach ID stage
  ) {
    // Stall action: Insert bubble and freeze pipeline
    // TODO: Which control signals need to be set to insert a bubble?
    // Hint:
    // - Flush ID/EX register (insert bubble)
    // - Freeze PC (don't fetch next instruction)
    // - Freeze IF/ID (hold current fetch result)
    io.id_flush := true.B
    io.pc_stall := true.B
    io.if_stall := true.B

  }.elsewhen(io.jump_flag) {
    // ============ Control Hazard (Branch Taken) ============
    // Branch resolved in ID stage - only 1 cycle penalty
    // Only flush IF stage (not ID) since branch resolved early
    // TODO: Which stage needs to be flushed when branch is taken?
    // Hint: Branch resolved in ID stage, discard wrong-path instruction
    io.if_flush := true.B
    // Note: No ID flush needed - branch already resolved in ID!
    // This is the key optimization: 1-cycle branch penalty vs 2-cycle
  }

  // ============================================================
  // [CA25: Exercise 21] Hazard Detection Summary and Analysis
  // ============================================================
  // Conceptual Exercise: Answer the following questions based on the hazard
  // detection logic implemented above
  //
  // Q1: Why do we need to stall for load-use hazards?
  // A: Load 的資料在 MEM 末才產出，但下一道指令 EX 初就需要，forwarding 也無法跨越這 1 個週期的差距，所以必須插入 1 個 stall bubble，讓 Load 先完成 MEM，再透過 MEM→EX 轉發。
  // Hint: Consider data dependency and forwarding limitations
  //
  // Q2: What is the difference between "stall" and "flush" operations?
  // A: Stall：凍結管線（讓同一道指令再待一個週期），PC 不前進，上游暫存器不更新，同時在 ID/EX 插入 NOP bubble
  //    Flush：清除管線（把已取入但不該執行的指令替換成 NOP），PC 繼續前進到正確目標
  // Hint: Compare their effects on pipeline registers and PC
  //
  // Q3: Why does jump instruction with register dependency need stall?
  // A: JALR 的跳轉目標是 rs1 + offset，rs1 必須在 ID 階段就算出正確位址。若 rs1 來自前一道 Load（EX 階段尚未完成），就無法在 ID 確定跳轉目標，必須 stall。
  // Hint: When is jump target address available?
  //
  // Q4: In this design, why is branch penalty only 1 cycle instead of 2?
  // A: FiveStageFinal 在 ID 階段就完成分支比較（搭配 ID-stage forwarding），只需要清除 IF 階段取錯的那一道指令，不像傳統設計要等到 EX 才判斷而需清 IF+ID（2 個週期）。
  // Hint: Compare ID-stage vs EX-stage branch resolution
  //
  // Q5: What would happen if we removed the hazard detection logic entirely?
  // A: 資料危害會導致 ALU 讀到舊值；控制危害會導致繼續執行不該執行的指令，程式邏輯完全崩潰。
  // Hint: Consider data hazards and control flow correctness
  //
  // Q6: Complete the stall condition summary:
  // Stall is needed when:
  // 1. EX 階段有 Load 且下一道指令使用該 rd（load-use）/ ID 階段是 JALR 且 EX 有 Load 到相同 rd（jump+load 相依）或是與 EX rd有相依關係 (EX stage condition)
  // 2. ID 階段是 JALR 且 MEM 有 Load 到相同 rd（兩週期距離的 jump+load） (MEM stage condition)
  //
  // Flush is needed when:
  // 1. Branch/jump instruction is taken in ID stage (Branch/Jump condition)
  //
}
