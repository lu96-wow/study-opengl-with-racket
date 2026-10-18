#lang glsl
;; ============================================================
;; 复习 02 · 矩阵计算：CPU 算，还是 GPU 算？
;; 运行：racket review/02-matrices/matrix-review.rkt
;; ============================================================
;; 这个脚本不创建窗口。它把 05–07 课的矩阵链路在 CPU 上算一遍，
;; 让你看到 GPU 顶点着色器里那行乘法到底在算什么。
;;
;; GPU 里的公式（每个顶点）：
;;   gl_Position = u_proj * u_view * u_model * vec4(pos, 1.0)
;;
;; 本脚本在 CPU 里做同样的事：
;;   clip = MVP * p
;;   ndc  = clip.xyz / clip.w
;;
;; 两种做法的数学完全一样，区别只是“在哪算、数据怎么流动”。

(require racket/math
         "../../lib/matrix.rkt")

;; 命名：vec-3 / vec-4 / mat-4 / vx / vdot / vnormalize 这些名字来自
;; #lang glsl（racket-shader），不是 racket/math；racket/math 只提供
;; degrees->radians 这类数学函数。生成 GLSL 时 vec-3 才写成 vec3。

(define (v->list v) (f32vector->list v))
(define (m->list m) (f32vector->list m))

;; 1. model：把物体平移到相机前方
(define model
  (m-translate (vec-3 0.5 0.0 -1.0)))

;; 2. view：相机在 (0,0,5)，看向原点
(define view
  (m-look-at (vec-3 0.0 0.0 5.0)
             (vec-3 0.0 0.0 0.0)
             (vec-3 0.0 1.0 0.0)))

;; 3. projection：45° 视场，4:3
(define proj
  (m-perspective (degrees->radians 45.0) (/ 4.0 3.0) 0.1 100.0))

;; 4. CPU 侧把三个矩阵乘成一个 MVP
(define mvp (mat4-mul proj (mat4-mul view model)))

;; 5. 一个顶点：(0.5, 0, 0, 1)
(define p (vec-4 0.5 0.0 0.0 1.0))

;; 6. CPU 顶点变换
(define clip (mat4-mul-vec mvp p))
(define w (vw clip))
(define ndc (vec-3 (/ (vx clip) w)
                   (/ (vy clip) w)
                   (/ (vz clip) w)))

(printf "复习 02 · 矩阵计算\n\n")
(printf "model =\n~a\n\n" (m->list model))
(printf "mvp =\n~a\n\n" (m->list mvp))
(printf "p    = ~a\n" (v->list p))
(printf "clip = ~a\n" (v->list clip))
(printf "w    = ~a\n" w)
(printf "ndc  = ~a\n\n" (v->list ndc))

;; 7. 一次算多个顶点：CPU 循环
(define points
  (list (vec-4 -0.5 -0.5 0.0 1.0)
        (vec-4  0.5 -0.5 0.0 1.0)
        (vec-4  0.0  0.5 0.0 1.0)))

(printf "CPU 逐个顶点算 NDC：\n")
(for ([pt (in-list points)])
  (define c (mat4-mul-vec mvp pt))
  (define ww (vw c))
  (printf "  p=~a -> ndc=~a\n"
          (v->list pt)
          (list (/ (vx c) ww) (/ (vy c) ww) (/ (vz c) ww))))

(printf "\n如果把这些计算搬到 GPU：\n")
(printf "  1. CPU 只上传 model/view/proj 三个矩阵 + 原始顶点 pos\n")
(printf "  2. 顶点着色器对每个顶点执行 u_proj * u_view * u_model * vec4(pos,1.0)\n")
(printf "  3. CPU 不需要遍历顶点，也不需要回传结果\n\n")
(printf "反过来，如果留在 CPU：\n")
(printf "  1. CPU 每帧遍历所有顶点，算好 NDC/世界坐标\n")
(printf "  2. 再把结果上传成新的 VBO\n")
(printf "  3. 顶点少、变化少时更简单；顶点多、相机每帧变时上传成本会压过计算成本\n")
