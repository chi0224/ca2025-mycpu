// SPDX-License-Identifier: MIT
// Lock-free Single-Producer Single-Consumer (SPSC) Ring Buffer
#ifndef RING_BUFFER_H
#define RING_BUFFER_H

#include <stdint.h>

#define RING_BUF_SIZE 128u
#define RING_BUF_MASK (RING_BUF_SIZE - 1u)

typedef struct {
    volatile uint8_t buffer[RING_BUF_SIZE];
    volatile uint32_t head;
    volatile uint32_t tail;
    volatile uint32_t drop_count;
} ring_buffer_t;

/**
 * 初始化環形緩衝區
 */
void ring_buffer_init(ring_buffer_t *rb);

/**
 * 寫入一個位元組 (生產者調用 - ISR 專用)
 *
 * @param rb 指向環形緩衝區的指標
 * @param data 要寫入的資料
 * @return 0 成功, -1 緩衝區已滿 (資料被丟棄並累加 drop_count)
 */
int ring_buffer_push(ring_buffer_t *rb, uint8_t data);

/**
 * 讀出一個位元組 (消費者調用 - Main 專用)
 *
 * @param rb 指向環形緩衝區的指標
 * @param data 接收讀出資料的指標
 * @return 0 成功, -1 緩衝區為空
 */
int ring_buffer_pop(ring_buffer_t *rb, uint8_t *data);

/**
 * 查詢緩衝區內目前累積的資料筆數
 */
uint32_t ring_buffer_available(const ring_buffer_t *rb);

static inline int ring_buffer_is_empty(const ring_buffer_t *rb)
{
    return rb->head == rb->tail;
}

static inline int ring_buffer_is_full(const ring_buffer_t *rb)
{
    return ((rb->head + 1u) & RING_BUF_MASK) == rb->tail;
}

#endif /* RING_BUFFER_H */
