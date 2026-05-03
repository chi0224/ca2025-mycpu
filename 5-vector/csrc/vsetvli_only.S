# vsetvli_only.s
# 驗證：vsetvli 設 vl=4，寫回 t0(x5)
# 預期：x5 = 4

.section .text
.global _start
_start:
    # AVL=x0 表示 "要求最大 vl"
    # e32m1 → SEW=32, LMUL=1 → vl = VLEN/SEW = 128/32 = 4
    vsetvli t0, x0, e32, m1, ta, ma    # t0(x5) = vl = 4

loop:
    j loop
