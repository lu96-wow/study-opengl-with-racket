#lang glsl-opengl
;; ============================================================
;; 04 · 纹理（补充）：同一张矩形纹理贴到三角形
;; 运行：racket 04-textures/triangle-texture.rkt
;; ============================================================
;; 纹理本身是一张矩形像素表（width × height），但“贴到哪”由 UV 决定。
;; 几何可以是三角形、四边形、任意 mesh；不需要和纹理形状一致。
;;
;; 本文件用和 04 课同一套棋盘纹理，只画一个三角形。
;; 三角形的三个 UV:
;;   (0,0) (1,0) (0.5,1)
;; 所以它显示的是棋盘下半部分的一个三角形区域。
;;
;; 想换区域就改 UV；想显示完整矩形，请用四边形（两个三角形），
;; 因为三角形只有三个角，无法同时覆盖矩形的四个角。

(require racket/class
         "../lib/window.rkt")

;; ------------------------------------------------------------
;; 一、棋盘纹理（和 04 课相同）
;; ------------------------------------------------------------
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
    (u8-array-set! px (+ i 3) 255))
  px)

;; ------------------------------------------------------------
;; 二、着色器：普通的“位置 + UV → 采样”
;; ------------------------------------------------------------
(glsl-shader tri-vert
  (version 330 core)
  (layout (location 0) in vec-2 a-pos)
  (layout (location 1) in vec-2 a-uv)
  (out vec-2 v-uv)
  (define (main) void
    (set! v-uv a-uv)
    (set! gl-position (vec-4 a-pos 0.0 1.0))))

(glsl-shader tri-frag
  (version 330 core)
  (in vec-2 v-uv)
  (uniform sampler-2d u-tex)
  (out vec-4 frag-color)
  (define (main) void
    (set! frag-color (texture u-tex v-uv))))

;; ------------------------------------------------------------
;; 三、三角形数据
;; ------------------------------------------------------------
;; 每个顶点 = 位置 vec-2 + UV vec-2
;; UV 只取棋盘的一个三角形区域，所以画出来不是一个“矩形图”，
;; 而是“矩形图上的一个三角形裁片”。
(define vertex-data
  (concat-vecs
   (vec-2 -0.6 -0.6) (vec-2 0.0 0.0)   ; 左下
   (vec-2  0.6 -0.6) (vec-2 1.0 0.0)   ; 右下
   (vec-2  0.0  0.6) (vec-2 0.5 1.0))) ; 上中

(define indices (u16-array 0 1 2))

(define program 0)
(define vao 0)
(define texture 0)
;; 这些缓冲句柄也放在顶层：关窗时 shutdown 要能拿到（0 = 还没创建）
(define vbo 0)
(define ebo 0)

(define (init)
  (set! program (build-program (gl-vertex-shader tri-vert)
                               (gl-fragment-shader tri-frag)))

  ;; ---- 纹理 ----
  (set! texture (texture-new))
  (gl-bind-texture gl-texture-2d texture)
  (gl-tex-parameter-i gl-texture-2d gl-texture-wrap-s gl-repeat)
  (gl-tex-parameter-i gl-texture-2d gl-texture-wrap-t gl-repeat)
  (gl-tex-parameter-i gl-texture-2d gl-texture-min-filter gl-linear-mipmap-linear)
  (gl-tex-parameter-i gl-texture-2d gl-texture-mag-filter gl-linear)
  (gl-tex-image-2d gl-texture-2d 0 gl-rgba tex-size tex-size 0
                   gl-rgba gl-unsigned-byte (make-checker-texture tex-size tex-cell))
  (gl-generate-mipmap gl-texture-2d)

  ;; 采样器指向 0 号纹理单元
  (program-use program)
  (gl-active-texture gl-texture0)
  (gl-bind-texture gl-texture-2d texture)
  (uniform-set! (uniform-location program "u_tex") 'sampler-2d 0)

  ;; ---- VAO / VBO / EBO ----
  (set! vao (vertex-array-new))
  (gl-bind-vertex-array vao)

  (set! vbo (buffer-new))
  (gl-bind-buffer gl-array-buffer vbo)
  (buffer-upload gl-array-buffer vertex-data gl-static-draw)

  ;; 步长和偏移仍然从布局算出来
  (define stride (shader-stride-bytes 'vec-2 'vec-2))
  (define uv-offset (shader-byte-size 'vec-2))
  (gl-vertex-attrib-pointer 0 2 gl-float #f stride 0)
  (gl-enable-vertex-attrib-array 0)
  (gl-vertex-attrib-pointer 1 2 gl-float #f stride uv-offset)
  (gl-enable-vertex-attrib-array 1)

  (set! ebo (buffer-new))
  (gl-bind-buffer gl-element-array-buffer ebo)
  (buffer-upload gl-element-array-buffer indices gl-static-draw)

  (gl-bind-vertex-array 0))

(define (draw)
  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear gl-color-buffer-bit)

  (program-use program)
  (gl-bind-vertex-array vao)
  ;; 3 个索引 = 一个三角形
  (gl-draw-elements gl-triangles 3 gl-unsigned-short 0)
  (gl-bind-vertex-array 0))

;; ------------------------------------------------------------
;; 关窗前释放：这些句柄归调用方，不删就一直占着显存
;; ------------------------------------------------------------
;; 顺序：先解绑当前状态，再删引用方（VAO）→ 缓冲 → 纹理 → program。
;; 初始值是 0：0 不是有效对象，GL 的 delete 会忽略它（init 没跑完也能安全关窗）。
(define (shutdown)
  (gl-bind-vertex-array 0)      ; 别删正在用的
  (gl-use-program 0)
  (vertex-array-delete vao)
  (buffer-delete vbo)
  (buffer-delete ebo)
  (texture-delete texture)
  (program-delete program))

(define-values (frame canvas)
  (make-window #:title "04 纹理贴到三角形"
               #:init init
               #:draw draw
               #:on-close shutdown))
(send frame show #t)
