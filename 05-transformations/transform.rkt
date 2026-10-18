#lang glsl-opengl
;; ============================================================
;; 05 · 变换
;; 运行：racket 05-transformations/transform.rkt
;; 上一课：04-textures/textures.rkt
;; ============================================================
;; 目标：让 04 课那张贴了棋盘的矩形动起来——绕自己的中心自转、大小来回脉动，
;; 整体还偏在屏幕右下角。这三件事其实就是三个矩阵：平移 T、旋转 R、缩放 S。
;; 先乘成一个 M，再在顶点着色器里乘到每个顶点上：
;;   gl-position = M * vec-4(a-pos, 0.0, 1.0)
;; 矩阵公式在 lib/matrix.rkt 里（05/06/07 三节共用一份），这里只管用。

(require racket/class
         racket/gui
         "../lib/window.rkt"
         "../lib/matrix.rkt")

;; ------------------------------------------------------------
;; 一、棋盘纹理（同 04 课：代码生成一张棋盘，不依赖图片文件）
;; ------------------------------------------------------------
;; OpenGL 要的是"一条连续的 RGBA 字节流"（每像素 4 个字节，0-255）。
;; 这里画一张 size×size 的棋盘：每个 cell×cell 的方块在两种颜色之间交替。
(define tex-size 64)
(define tex-cell 8)

(define (make-checker-texture size cell)
  (define px (make-u8-array (* size size 4) 255))
  (for* ([y (in-range size)] [x (in-range size)])
    (define on? (even? (+ (quotient x cell) (quotient y cell))))
    (define i (* (+ (* y size) x) 4))
    (define r (if on? 230 40))
    (define g (if on? 140 70))
    (define b (if on? 60 130))
    (u8-array-set! px i r)
    (u8-array-set! px (+ i 1) g)
    (u8-array-set! px (+ i 2) b)
    (u8-array-set! px (+ i 3) 255))          ; alpha 不透明
  px)

;; ------------------------------------------------------------
;; 二、着色器：顶点位置改为"原位置乘变换矩阵"
;; ------------------------------------------------------------
;; 和 04 课相比只多了一样东西：uniform mat-4 u-transform。
;; 它在 CPU 端每帧算好、每帧上传，是整个矩形共用的一个矩阵——
;; 顶点着色器对四个顶点各乘一次，于是整个矩形一起动。
(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec-2 a-pos)     ; 属性 0：位置
        (layout (location 1) in vec-2 a-uv)      ; 属性 1：UV 坐标
        (uniform mat-4 u-transform)              ; 变换矩阵：平移·旋转·缩放
        (out vec-2 v-uv)
        (define (main) void
          ;; 位置先补一个 z=0、w=1，再乘矩阵；w=1 是给平移留的位置。
          (set! gl-position (* u-transform (vec-4 a-pos 0.0 1.0)))
          (set! v-uv a-uv))))

;; 片元着色器和 04 课一模一样：按 UV 采样棋盘。
(define frag-src
  (glsl (version 330 core)
        (in vec-2 v-uv)
        (uniform sampler-2d u-tex)
        (out vec-4 frag-color)
        (define (main) void
          (set! frag-color (texture u-tex v-uv)))))

;; ------------------------------------------------------------
;; 三、顶点数据：位置(vec-2) + UV(vec-2) 交错，共 4 个 float = 16 字节
;; ------------------------------------------------------------
;;    x  y  u  v | x  y  u  v | …
;; 顶点只写"原始"位置：矩形中心在原点。往哪儿摆、转多少、多大，全交给矩阵。
(define vertex-data
  (concat-vecs (vec-2  0.5  0.5) (vec-2 1.0 1.0)     ; 右上
               (vec-2  0.5 -0.5) (vec-2 1.0 0.0)     ; 右下
               (vec-2 -0.5 -0.5) (vec-2 0.0 0.0)     ; 左下
               (vec-2 -0.5  0.5) (vec-2 0.0 1.0)))   ; 左上

(define indices (u16-array 0 1 2   0 2 3))          ; 两个三角形拼成矩形

(define program #f)
(define vao #f)
(define texture #f)
(define u-transform-loc #f)

(define (init)
  (set! program (build-program (gl-vertex-shader vert-src)
                               (gl-fragment-shader frag-src)))

  ;; ---- 3.1 创建并上传纹理 ----
  (set! texture (texture-new))
  (gl-bind-texture gl-texture-2d texture)
  (gl-tex-parameter-i gl-texture-2d gl-texture-wrap-s gl-repeat)
  (gl-tex-parameter-i gl-texture-2d gl-texture-wrap-t gl-repeat)
  (gl-tex-parameter-i gl-texture-2d gl-texture-min-filter gl-linear-mipmap-linear)
  (gl-tex-parameter-i gl-texture-2d gl-texture-mag-filter gl-linear)

  (define checker (make-checker-texture tex-size tex-cell))
  (gl-tex-image-2d gl-texture-2d 0 gl-rgba tex-size tex-size 0
                   gl-rgba gl-unsigned-byte checker)
  (gl-generate-mipmap gl-texture-2d)

  ;; ---- 3.2 uniform：采样器指向 0 号纹理单元 ----
  (program-use program)                       ; uniform 作用在"当前程序"上
  (uniform-set! (uniform-location program "u_tex") 'sampler-2d 0)
  ;; 变换矩阵的位置也查一次存起来，draw 里每帧往这里送新算出的矩阵。
  (set! u-transform-loc (uniform-location program "u_transform"))

  ;; ---- 3.3 VAO / VBO / EBO ----
  (set! vao (vertex-array-new))
  (gl-bind-vertex-array vao)

  (define vbo (buffer-new))
  (gl-bind-buffer gl-array-buffer vbo)
  (buffer-upload gl-array-buffer vertex-data gl-static-draw)
  ;; 步长和偏移都从布局算出来，不写字面量。
  (define stride (shader-stride-bytes 'vec-2 'vec-2))   ; 2 + 2 = 4 个 float = 16 字节
  (define uv-offset (shader-byte-size 'vec-2))         ; 跳过位置那 2 个 float = 8 字节
  (gl-vertex-attrib-pointer 0 2 gl-float #f stride 0)
  (gl-enable-vertex-attrib-array 0)
  (gl-vertex-attrib-pointer 1 2 gl-float #f stride uv-offset)
  (gl-enable-vertex-attrib-array 1)

  (define ebo (buffer-new))
  (gl-bind-buffer gl-element-array-buffer ebo)
  (buffer-upload gl-element-array-buffer indices gl-static-draw)

  (gl-bind-vertex-array 0))

(define (draw)
  ;; 从程序启动到现在的秒数，用来当旋转角度和脉动相位。
  (define t (/ (current-inexact-monotonic-milliseconds) 1000.0))

  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear gl-color-buffer-bit)

  (program-use program)

  ;; 缩放脉动：s 在 0.6..1.0 之间来回，让"大小在变"这件事一眼看得出来。
  (define s (+ 0.8 (* 0.2 (sin (* t 2.0)))))

  ;; 组合：M = T · R · S。乘法从右往左作用——点先被 S 缩、再被 R 转、最后被 T 移。
  ;; 旋转和缩放都是"绕原点"的，而原点正好是矩形中心，所以看到的是自转 + 原地脉动；
  ;; 最后那步平移把整个矩形挪到右下角。
  (define M (mat4-mul (m-translate (vec-3 0.5 -0.5 0.0))
                      (mat4-mul (m-rotate-z t)
                                (m-scale (vec-3 s s 1.0)))))
  ;; 上传矩阵：uniform-set! 按 'mat-4 选对 gl-uniform-matrix-4fv，并固定 count=1、transpose=#f
  ;; （mat-4 本来就是列主序，与 OpenGL 期望的上传布局一致，不需要转置）。
  (uniform-set! u-transform-loc 'mat-4 M)

  (gl-bind-vertex-array vao)
  (gl-draw-elements gl-triangles 6 gl-unsigned-short 0)
  (gl-bind-vertex-array 0))

(define-values (frame canvas)
  (make-window #:title "05 变换"
               #:width 800
               #:height 600
               #:init init
               #:draw draw))
(send frame show #t)

;; 矩阵每帧在变，需要定时器不断请求重绘
(define timer
  (new timer% [interval 16]
              [notify-callback (lambda () (send canvas refresh))]))

;; ------------------------------------------------------------
;; 试试看（改完重跑）：
;;   · 把组合改成 (mat4-mul (m-rotate-z t) (m-translate (vec-3 0.5 -0.5 0.0)))
;;     → 变成先平移后旋转，矩形绕着屏幕中心公转，不再原地自转。
;;   · 把 m-rotate-z 换成 m-rotate-x / m-rotate-y → 看起来被"压扁"了，
;;     因为还没有深度和投影（06 讲）。
;;   · 把 m-scale 的第三个分量也改成 s → 画面没变化：z 不影响 2D 顶点。
;;   · 把 m-translate 的 z 改成 -0.5 → 画面也没变化：没有投影时 z 只是白写。
;; ============================================================