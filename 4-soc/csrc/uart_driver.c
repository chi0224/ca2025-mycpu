// SPDX-License-Identifier: MIT
// Unified UART Driver Implementation
#include "uart_driver.h"
#include "mmio.h"

extern void enable_interrupt(void);

static ring_buffer_t g_rx_rb;

/**
 * 中斷服務常式（ISR）與 Trap Handler
 *
 * 注意：此函式會覆蓋 init.S 中的 weak trap_handler 符號。
 * 當 CPU 收到中斷時，init.S 會保存暫存器並跳轉至此處。
 */
void trap_handler(uint32_t mepc, uint32_t mcause)
{
    (void) mepc;
    (void) mcause;

    // 檢查 UART 狀態暫存器 bit 1 (RX FIFO 非空)
    while (*UART_STATUS & 0x02) {
        // 讀取硬體 RX_DATA 暫存器
        uint8_t byte = (uint8_t)(*UART_RECV & 0xFF);

        // 快速寫入軟體 SPSC Ring Buffer
        ring_buffer_push(&g_rx_rb, byte);
    }
    // 當硬體 FIFO 被讀空後，硬體會自動將 signal_interrupt 清除
}

void uart_init(void)
{
    // 1. 初始化軟體環形緩衝區
    ring_buffer_init(&g_rx_rb);

    // 2. 啟用硬體 UART 中斷功能
    *UART_INTERRUPT = 1;

    // 3. 啟用 RISC-V 核心特權中斷 (mtvec, mstatus.MIE, mie.MEIE)
    enable_interrupt();
}

void uart_putc(uint8_t c)
{
    // 等待硬體發送緩衝區就緒 (bit 0 = TX ready)
    while (!(*UART_STATUS & 0x01)) {
        __asm__ volatile("nop");
    }
    // 寫入硬體發送暫存器
    *UART_SEND = (uint32_t) c;
}

void uart_puts(const char *s)
{
    if (!s) return;
    while (*s) {
        uart_putc((uint8_t) *s++);
    }
}

int uart_getc_async(uint8_t *ch)
{
    // 直接自軟體 Ring Buffer 取出資料
    return ring_buffer_pop(&g_rx_rb, ch);
}

int uart_getc_timeout(uint8_t *ch, uint32_t timeout_ms)
{
    uint32_t start_cycles = timer_get_cycles();
    uint32_t timeout_cycles = MS_TO_CYCLES(timeout_ms);

    while (1) {
        // 1. 嘗試從 Ring Buffer 讀取
        if (ring_buffer_pop(&g_rx_rb, ch) == 0) {
            return 0; // 成功讀取
        }

        // 2. 檢查是否達到超時週期數
        if (timer_is_timeout(start_cycles, timeout_cycles)) {
            return -1; // 發生超時
        }
    }
}

uint8_t uart_getc_blocking(void)
{
    uint8_t ch = 0;
    // 循環等待直到中斷將資料送入 Ring Buffer
    while (uart_getc_async(&ch) != 0) {
        // 等待中斷喚醒，降低 CPU 空轉功耗，硬體沒有實作wfi指令，實際fetch使用nop模擬
        __asm__ volatile("wfi");
    }
    return ch;
}

uint32_t uart_get_drop_count(void)
{
    return g_rx_rb.drop_count;
}

uint32_t uart_rx_available(void)
{
    return ring_buffer_available(&g_rx_rb);
}
