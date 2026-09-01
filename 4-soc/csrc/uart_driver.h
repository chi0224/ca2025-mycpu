// SPDX-License-Identifier: MIT
// Unified UART Driver Interface with Interrupt and Non-blocking Support
#ifndef UART_DRIVER_H
#define UART_DRIVER_H

#include <stdint.h>
#include "ring_buffer.h"
#include "timebase.h"

/**
 * 初始化 UART 驅動程式
 * 1. 初始化 RX 軟體 Ring Buffer
 * 2. 啟用硬體 UART 接收中斷暫存器
 * 3. 啟用 RISC-V 核心全域中斷 (mstatus.MIE & mie.MEIE)
 */
void uart_init(void);

/**
 * 發送單一字元（含硬體 TX Ready 輪詢）
 */
void uart_putc(uint8_t c);

/**
 * 發送字串
 */
void uart_puts(const char *s);

/**
 * 【非阻塞讀取】立即檢查是否有字元到達
 *
 * @param ch 接收字元的指標
 * @return 0 成功讀取, -1 緩衝區為空（立即返回，不等待）
 */
int uart_getc_async(uint8_t *ch);

/**
 * 【超時讀取】在指定毫秒內等待字元
 *
 * @param ch 接收字元的指標
 * @param timeout_ms 超時等待上限（毫秒）
 * @return 0 成功讀取, -1 超時未收到
 */
int uart_getc_timeout(uint8_t *ch, uint32_t timeout_ms);

/**
 * 【阻塞讀取】等待直到收到字元為止
 * 等待期間執行 wfi（Wait For Interrupt）以節省 CPU 功耗
 */
uint8_t uart_getc_blocking(void);

/**
 * 取得因軟體緩衝區滿載而丟棄的字元總數
 */
uint32_t uart_get_drop_count(void);

/**
 * 查詢目前軟體緩衝區內累積的字元數量
 */
uint32_t uart_rx_available(void);

#endif /* UART_DRIVER_H */
