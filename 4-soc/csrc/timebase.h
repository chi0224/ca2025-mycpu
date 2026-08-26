// SPDX-License-Identifier: MIT
// Timebase module using RISC-V mcycle CSR (Cycle-based Zero-Division)
#ifndef TIMEBASE_H
#define TIMEBASE_H

#include <stdint.h>

/* 4-soc CPU 核心時脈為 50 MHz */
#define CPU_FREQ_HZ 50000000u

/* 編譯時期常數（Pre-computed Constants），零運算開銷 */
#define CYCLES_PER_MS (CPU_FREQ_HZ / 1000u)    // 50,000 cycles / ms
#define CYCLES_PER_US (CPU_FREQ_HZ / 1000000u) // 50 cycles / us


#define MS_TO_CYCLES(ms) ((uint32_t) (ms) * CYCLES_PER_MS)
#define US_TO_CYCLES(us) ((uint32_t) (us) * CYCLES_PER_US)

/**
 * 讀取 RISC-V 核心 mcycle CSR 暫存器
 * 耗時：僅 1 個 CPU 週期（單一指令完成）
 */
static inline uint32_t timer_get_cycles(void)
{
    uint32_t cycles;
    __asm__ volatile("csrr %0, mcycle" : "=r"(cycles));
    return cycles;
}

/**
 * 基於 Cycle 的非阻塞超時檢查輔助函式
 *
 * @param start_cycle 起始週期的快照
 * @param timeout_cycles 預期的超時週期數
 * @return 1 表示已超時，0 表示尚未超時
 */
static inline int timer_is_timeout(uint32_t start_cycle, uint32_t timeout_cycles)
{
    // 利用無號數溢位特性：(now - start) 永遠能得到精準經過的週期數
    return (timer_get_cycles() - start_cycle) >= timeout_cycles;
}

/**
 * 精確微秒級延遲
 */
static inline void timer_delay_us(uint32_t us)
{
    uint32_t start = timer_get_cycles();
    uint32_t wait_cycles = US_TO_CYCLES(us);
    while (!timer_is_timeout(start, wait_cycles)) {
        __asm__ volatile("nop");
    }
}

/**
 * 精確毫秒級延遲
 */
static inline void timer_delay_ms(uint32_t ms)
{
    uint32_t start = timer_get_cycles();
    uint32_t wait_cycles = MS_TO_CYCLES(ms);
    while (!timer_is_timeout(start, wait_cycles)) {
        __asm__ volatile("nop");
    }
}

#endif /* TIMEBASE_H */
