#lang glsl-opengl
;; ============================================================
;; 02 · 用索引画一个矩形（EBO）
;; 运行：racket 02-hello-triangle/rectangle.rkt
;; 上一课：02-hello-triangle/triangle.rkt
;; ============================================================
;; 上一课用 3 个顶点画三角形。矩形 = 两个三角形 = 6 个顶点，但只有 4 个角。
;; 直接写 6 个顶点会重复两个角。用索引缓冲（EBO）只存 4 个角 + 一张下标表。
;; 和上一课的差别只有两处：多上传一个 EBO，画的时候换成 gl-draw-elements。
;; 本文件继续使用 02.0 介绍的便利层；raw 等价物见教程 2.0 的对照表。

(require racket/class
         "../lib/window.rkt")

;; 两段着色器：和上一课一样，只是颜色换成蓝色
(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec-3 a-pos)
        (define (main) void
          (set! gl-position (vec-4 a-pos 1.0)))))

(define frag-src
  (glsl (version 330 core)
        (out vec-4 frag-color)
        (define (main) void
          (set! frag-color (vec-4 0.2 0.7 1.0 1.0)))))

;; 4 个角（顶点只存一份）
(define vertices
  (vec-array (vec-3  0.5  0.5 0.0)     ; 0 右上
          (vec-3  0.5 -0.5 0.0)     ; 1 右下
          (vec-3 -0.5 -0.5 0.0)     ; 2 左下
          (vec-3 -0.5  0.5 0.0)))   ; 3 左上

;; 下标表：告诉 GPU 每次取哪 3 个角拼三角形（(0,1,2) 和 (0,2,3) 共用 0、2）
;; 裸写法： (u16vector 0 1 2 0 2 3)（ffi/vector）。
;; u16-array 是它的命名版：同元素类型、同内存，额外做 0..65535 检查。
(define indices (u16-array 0 1 2
                           0 2 3))

(define program #f)
(define vao #f)

(define (init)
  (set! program (build-program (gl-vertex-shader vert-src)
                               (gl-fragment-shader frag-src)))

  ;; 顶点数据上传：只依赖 gl-array-buffer 这个绑定，和 VAO 无关
  (define vbo (buffer-new))
  (gl-bind-buffer gl-array-buffer vbo)
  (buffer-upload gl-array-buffer vertices gl-static-draw)

  ;; 建 VAO、描述位置属性（这两步依赖 VAO）
  (set! vao (vertex-array-new))
  (gl-bind-vertex-array vao)
  (gl-vertex-attrib-pointer 0 3 gl-float #f 0 0)
  (gl-enable-vertex-attrib-array 0)

  ;; 索引缓冲 EBO。
  ;; ★和 ARRAY_BUFFER 不同：ELEMENT_ARRAY_BUFFER 的绑定是 VAO 状态，
  ;;   所以必须在 VAO 还绑着的时候绑它，VAO 才会记住；画的时候才找得到。
  (define ebo (buffer-new))
  (gl-bind-buffer gl-element-array-buffer ebo)
  (buffer-upload gl-element-array-buffer indices gl-static-draw)

  (gl-bind-vertex-array 0))

(define (draw)
  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear gl-color-buffer-bit)

  (program-use program)
  (gl-bind-vertex-array vao)
  ;; 和 gl-draw-arrays 的区别：不按顺序取顶点，而是按 EBO 里的下标取。
  ;; gl-draw-elements(图元, 索引个数, 索引类型, 从第几个索引开始)
  (gl-draw-elements gl-triangles 6 gl-unsigned-short 0)
  (gl-bind-vertex-array 0))

(define-values (frame canvas)
  (make-window #:title "02 矩形（索引绘制）"
               #:init init
               #:draw draw))
(send frame show #t)
