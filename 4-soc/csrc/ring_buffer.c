// SPDX-License-Identifier: MIT
// Lock-free Single-Producer Single-Consumer (SPSC) Ring Buffer Implementation
#include "ring_buffer.h"

void ring_buffer_init(ring_buffer_t *rb)
{
    if (!rb) return;
    rb->head = 0;
    rb->tail = 0;
    rb->drop_count = 0;
    for (uint32_t i = 0; i < RING_BUF_SIZE; i++) {
        rb->buffer[i] = 0;
    }
}

int ring_buffer_push(ring_buffer_t *rb, uint8_t data)
{
    uint32_t next_head = (rb->head + 1u) & RING_BUF_MASK;

    // 檢查緩衝區是否已滿 (滿載時保留 1 slot 區分 full 與 empty)
    if (next_head == rb->tail) {
        rb->drop_count++; // 記錄溢位事件
        return -1;
    }

    // 1. 先將資料寫入緩衝區陣列
    rb->buffer[rb->head] = data;

    // 2. 插入編譯器屏障，防止編譯器將指標更新排在資料寫入之前
    __asm__ volatile("" ::: "memory");

    // 3. 更新 head 指標（具備原子性）
    rb->head = next_head;
    return 0;
}

int ring_buffer_pop(ring_buffer_t *rb, uint8_t *data)
{
    // 檢查緩衝區是否為空
    if (rb->head == rb->tail) {
        return -1;
    }

    // 1. 先讀取當前 tail 指向的資料
    if (data) {
        *data = rb->buffer[rb->tail];
    }

    // 2. 插入編譯器屏障
    __asm__ volatile("" ::: "memory");

    // 3. 更新 tail 指標
    rb->tail = (rb->tail + 1u) & RING_BUF_MASK;
    return 0;
}

uint32_t ring_buffer_available(const ring_buffer_t *rb)
{
    uint32_t head = rb->head;
    uint32_t tail = rb->tail;
    return (head - tail) & RING_BUF_MASK;
}
