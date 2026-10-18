#lang glsl
;; ============================================================
;; 09 · 先在 CPU 上算一遍 Phong
;; 运行：racket 09-basic-lighting/phong-cpu.rkt
;; ============================================================
;; 这个脚本不创建窗口。它把 09 课的三个片元拿到 CPU 上，
;; 用和 frag-src 完全相同的公式算一遍，让你先看到数字，
;; 再去 9.3 看片元着色器里的同一套代码。
;;
;; 和 shader 的对应关系：
;;   v-frag-pos  ←  model * vec4(local-pos, 1.0)
;;   v-normal    ←  normal-matrix * local-normal
;;   下面的 phong ← frag-src 的 main()
;;
;; 参数和 09 课一致：ambient 0.1、shininess 32、specularStrength 0.5、
;; 物体色 (1.0 0.5 0.31)、光 (1.2 1.0 2.0)、相机 (0 0 3)。

(require racket/math
         "../lib/matrix.rkt")

;; 命名：vec-3 / vec-4 / mat-4 / vx / vdot / vnormalize 这些名字来自
;; #lang glsl（racket-shader），不是 racket/math；racket/math 只提供
;; degrees->radians 这类数学函数。生成 GLSL 时 vec-3 才写成 vec3。

(define object-color (vec-3 1.0 0.5 0.31))
(define light-color (vec-3 1.0 1.0 1.0))
(define light-pos (vec-3 1.2 1.0 2.0))
(define eye-pos (vec-3 0.0 0.0 3.0))

(define model
  (mat4-mul (m-rotate-y (degrees->radians 15.0))
            (m-rotate-x (degrees->radians -12.0))))
(define normal-matrix (m-normal-matrix model))

(define (->vec4 v) (vec-4 (vx v) (vy v) (vz v) 1.0))
(define (v->list v) (list (vx v) (vy v) (vz v)))

;; 片元着色器的公式，逐字照搬
(define (phong frag-pos normal)
  (define norm (vnormalize normal))
  (define light-dir (vnormalize (vsub light-pos frag-pos)))
  (define view-dir (vnormalize (vsub eye-pos frag-pos)))
  (define reflect-dir (vreflect (vneg light-dir) norm))

  (define ambient 0.1)
  (define diff (max (vdot norm light-dir) 0.0))
  (define spec (* (expt (max (vdot view-dir reflect-dir) 0.0) 32.0) 0.5))
  (define color (vmul (vmul light-color (+ ambient diff spec)) object-color))
  (list norm light-dir reflect-dir ambient diff spec color))

;; 三个采样点：立方体局部坐标 + 局部法线
(define samples
  (list (list "正面中央" (vec-3 0.0 0.0 0.5) (vec-3 0.0 0.0 1.0))
        (list "正面右上" (vec-3 0.5 0.5 0.5) (vec-3 0.0 0.0 1.0))
        (list "左面中央" (vec-3 -0.5 0.0 0.0) (vec-3 -1.0 0.0 0.0))))

(printf "09 · CPU 版 Phong（和 frag-src 同一个公式）\n\n")
(printf "light-pos = ~a\n" (v->list light-pos))
(printf "eye-pos   = ~a\n" (v->list eye-pos))
(printf "object    = ~a\n\n" (v->list object-color))

(for ([s (in-list samples)])
  (define name (list-ref s 0))
  (define local-pos (list-ref s 1))
  (define local-normal (list-ref s 2))

  ;; 顶点着色器在 CPU 上做的两件事
  (define world4 (mat4-mul-vec model (->vec4 local-pos)))
  (define frag-pos (vec-3 (vx world4) (vy world4) (vz world4)))
  (define world-normal (mat3-mul-vec normal-matrix local-normal))

  (define-values (n l r ambient diff spec color)
    (apply values (phong frag-pos world-normal)))

  (printf "── ~a ──\n" name)
  (printf "  local-pos    = ~a\n" (v->list local-pos))
  (printf "  v-frag-pos   = ~a\n" (v->list frag-pos))
  (printf "  v-normal     = ~a\n" (v->list world-normal))
  (printf "  n            = ~a\n" (v->list n))
  (printf "  l            = ~a\n" (v->list l))
  (printf "  r            = ~a\n" (v->list r))
  (printf "  ambient=~a  diffuse=~a  specular=~a\n"
          (real->decimal-string ambient 3)
          (real->decimal-string diff 3)
          (real->decimal-string spec 3))
  (printf "  color        = ~a\n\n" (v->list color)))

(printf "把同样的数放进 9.3 的 frag-src：\n")
(printf "  (float ambient 0.1)\n")
(printf "  (float diff (max (dot norm light-dir) 0.0))\n")
(printf "  (float spec (* (pow (max (dot view-dir reflect-dir) 0.0) 32.0) 0.5))\n")
(printf "  (set! frag-color (vec-4 (* (+ ambient diff spec) u-light-color u-object-color) 1.0))\n\n")
(printf "数值一样，区别只是：这里 CPU 逐个采样点算；片元着色器对每个像素并行算。\n")
