# vadd_vector.s
# 驗證：vadd.vv v3, v1, v2  →  element-wise 加法
#
# v1 = [1, 2, 3, 4]  (from mem[0x2000])
# v2 = [10,20,30,40] (from mem[0x2010])
# v3 = v1 + v2 = [11, 22, 33, 44]  → store to mem[0x2020]
#
# 驗證點：mem[0x2020]=11, [0x2024]=22, [0x2028]=33, [0x202C]=44

.section .text
.global _start
_start:
    # ── 準備 v1 的資料 ────────────────────────────
    li   a0, 0x2000
    li   t0, 1;   sw t0,  0(a0)
    li   t0, 2;   sw t0,  4(a0)
    li   t0, 3;   sw t0,  8(a0)
    li   t0, 4;   sw t0, 12(a0)

    # ── 準備 v2 的資料 ────────────────────────────
    li   a1, 0x2010
    li   t0, 10;  sw t0,  0(a1)
    li   t0, 20;  sw t0,  4(a1)
    li   t0, 30;  sw t0,  8(a1)
    li   t0, 40;  sw t0, 12(a1)

    # ── 設 vl=4 ──────────────────────────────────
    li   t0, 4
    vsetvli t0, t0, e32, m1, ta, ma

    # ── Vector Load ───────────────────────────────
    vle32.v  v1, (a0)                  # v1 = [1, 2, 3, 4]
    vle32.v  v2, (a1)                  # v2 = [10, 20, 30, 40]

    # ── vadd.vv ───────────────────────────────────
    vadd.vv  v3, v1, v2                # v3 = [11, 22, 33, 44]

    # ── Store 結果 ────────────────────────────────
    li   a2, 0x2020
    vse32.v  v3, (a2)

loop:
    j loop
