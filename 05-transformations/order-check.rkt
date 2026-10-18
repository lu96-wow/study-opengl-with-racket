#lang glsl
;; ============================================================
;; 05 · 在 CPU 上验证变换顺序
;; 运行：racket 05-transformations/order-check.rkt
;; ============================================================
;; 05 课的 M = T · R · S 每个顶点在顶点着色器里乘一次。
;; 这个脚本不建窗口，只在 CPU 上拿同一个点乘不同的矩阵，
;; 让你看到「从右往左作用」和「换个顺序结果不同」到底长什么样。
;;
;; 取矩形右上角 (0.5, 0.5, 0, 1)，做三件事：
;;   S：放大 2 倍
;;   R：绕 z 轴转 90°
;;   T：平移到 (0.5, -0.5)
;;
;; T·R·S 的步骤是 S → R → T；R·T·S 的步骤是 S → T → R。

(require racket/math
         "../lib/matrix.rkt")

;; 命名：vec-3 / vec-4 / mat-4 / vx / vdot / vnormalize 这些名字来自
;; #lang glsl（racket-shader），不是 racket/math；racket/math 只提供
;; degrees->radians 这类数学函数。生成 GLSL 时 vec-3 才写成 vec3。

(define p (vec-4 0.5 0.5 0.0 1.0))          ; 矩形右上角（局部坐标）

(define S (m-scale (vec-3 2.0 2.0 1.0)))
(define R (m-rotate-z (degrees->radians 90.0)))
(define T (m-translate (vec-3 0.5 -0.5 0.0)))

(define (apply-m name m)
  (define r (mat4-mul-vec m p))
  (printf "  ~a  p = (~a, ~a)\n"
          name
          (real->decimal-string (vx r) 2)
          (real->decimal-string (vy r) 2))
  r)

(printf "起点 p = (~a, ~a)\n\n" (vx p) (vy p))

(printf "组合一：M = T · R · S（05 课的选择）\n")
(printf "  矩阵从右往左作用：先 S，再 R，最后 T\n")
(define after-S (apply-m "S " S))
(define after-SR (mat4-mul-vec R after-S))
(printf "  R  p = (~a, ~a)   ← 注意这里用了上一步的结果\n"
        (real->decimal-string (vx after-SR) 2)
        (real->decimal-string (vy after-SR) 2))
(define m1 (mat4-mul T (mat4-mul R S)))
(define r1 (apply-m "M " m1))
(printf "  结果：矩形先自转，最后整体挪到右下角。\n\n")

(printf "组合二：M = R · T · S（换个顺序）\n")
(printf "  矩阵从右往左作用：先 S，再 T，最后 R\n")
(define after-ST (mat4-mul-vec T after-S))
(printf "  T  p = (~a, ~a)\n"
        (real->decimal-string (vx after-ST) 2)
        (real->decimal-string (vy after-ST) 2))
(define m2 (mat4-mul R (mat4-mul T S)))
(define r2 (apply-m "M " m2))
(printf "  结果：平移被一起转走，矩形绕原点公转。\n\n")

(printf "比较：\n")
(printf "  T·R·S → (~a, ~a)\n"
        (real->decimal-string (vx r1) 2) (real->decimal-string (vy r1) 2))
(printf "  R·T·S → (~a, ~a)\n"
        (real->decimal-string (vx r2) 2) (real->decimal-string (vy r2) 2))
(printf "\n同一个点、同样三个矩阵，只换了乘法顺序，结果就不同：矩阵乘法没有交换律。\n")
(printf "GPU 顶点着色器只负责乘：gl-position = M · vec4(pos, 1.0)。\n")
(printf "M 是 CPU 先乘好的，所以「先做什么、后做什么」是 CPU 侧的决定。\n")
