# vmacc_vector.s
# 驗證：vmacc.vv vd, vs1, vs2  →  vd = vd + vs1 * vs2
#
# v1(vs1) = [2, 3, 4, 5]   (from mem[0x2000])
# v2(vs2) = [3, 4, 5, 6]   (from mem[0x2010])
# v3(vd)  = [1, 1, 1, 1]   (from mem[0x2020])  ← 初始值
#
# vmacc.vv v3, v1, v2
#   → v3[i] = v3[i] + v1[i]*v2[i]
#   → [1+6, 1+12, 1+20, 1+30] = [7, 13, 21, 31]
#   → store to mem[0x2030]
#
# 驗證點：mem[0x2030]=7, [0x2034]=13, [0x2038]=21, [0x203C]=31

.section .text
.global _start
_start:
    # ── 準備 v1 的資料 ────────────────────────────
    li   a0, 0x2000
    li   t0, 2;   sw t0,  0(a0)
    li   t0, 3;   sw t0,  4(a0)
    li   t0, 4;   sw t0,  8(a0)
    li   t0, 5;   sw t0, 12(a0)

    # ── 準備 v2 的資料 ────────────────────────────
    li   a1, 0x2010
    li   t0, 3;   sw t0,  0(a1)
    li   t0, 4;   sw t0,  4(a1)
    li   t0, 5;   sw t0,  8(a1)
    li   t0, 6;   sw t0, 12(a1)

    # ── 準備 vd 初始值 ────────────────────────────
    li   a2, 0x2020
    li   t0, 1;   sw t0,  0(a2)
    li   t0, 1;   sw t0,  4(a2)
    li   t0, 1;   sw t0,  8(a2)
    li   t0, 1;   sw t0, 12(a2)

    # ── 設 vl=4 ──────────────────────────────────
    vsetvli t0, x0, e32, m1, ta, ma

    # ── Load 三個向量暫存器 ───────────────────────
    vle32.v  v1, (a0)                  # v1 = [2, 3, 4, 5]
    vle32.v  v2, (a1)                  # v2 = [3, 4, 5, 6]
    vle32.v  v3, (a2)                  # v3 = [1, 1, 1, 1]  (vd 初始值)

    # ── vmacc.vv ──────────────────────────────────
    vmacc.vv v3, v1, v2                # v3 = v3 + v1*v2 = [7, 13, 21, 31]

    # ── Store 結果 ────────────────────────────────
    li   a3, 0x2030
    vse32.v  v3, (a3)

loop:
    j loop
