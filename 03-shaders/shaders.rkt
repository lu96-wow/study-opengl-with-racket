#lang glsl-opengl
;; ============================================================
;; 03 · 着色器：类型、阶段间传值、uniform、多属性
;; 运行：racket 03-shaders/shaders.rkt
;; 上一课：02-hello-triangle/rectangle.rkt
;; ============================================================
;; 效果：三角形三个角红/绿/蓝，中间自动渐变，整体亮度随时间脉动。
;; 四件事都在这一个程序里：GLSL 类型、out→in 传值、uniform、多个顶点属性。

(require racket/class
         racket/gui
         "../lib/window.rkt")

;; 顶点着色器。
;; GLSL 的类型：float/int/bool 是标量，vec-2/vec-3/vec-4 是向量（浮点字面量要写 1.0）。
;; 这里有两个输入属性：位置 a-pos（0 号槽）、颜色 a-color（1 号槽）。
(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec-2 a-pos)
        (layout (location 1) in vec-3 a-color)
        ;; out：交给片元着色器。名字和类型要和那边的 in 一致，两段靠这个配对。
        (out vec-3 v-color)
        (define (main) void
          (set! gl-position (vec-4 a-pos 0.0 1.0))
          (set! v-color a-color))))       ; 原样交出去

;; 片元着色器。
;; v-color 拿到的是"插值后"的颜色：三个顶点分别红/绿/蓝，GPU 按距离混合，
;; 于是三角形中间自动渐变——混合代码一行都不用写。
;; u-time 是 uniform：整个绘制共用一份，由 CPU 每帧更新（这里用来做亮度脉动）。
(define frag-src
  (glsl (version 330 core)
        (in vec-3 v-color)
        (uniform float u-time)
        (out vec-4 frag-color)
        (define (main) void
          (float k (+ 0.5 (* 0.5 (sin u-time))))   ; sin 在 -1..1 → 变换成 0.5..1.0
          (set! frag-color (vec-4 (* v-color k) 1.0)))))

;; 03 开始使用 uniform 便利层：uniform-location 查位置，uniform-set! 按类型上传。
;; 对应的 raw 两步（gl-get-uniform-location + gl-uniform-*）见教程 3.4。
;;
;; 顶点数据：每个顶点 = 位置(vec-2) + 颜色(vec-3)，交错排成一条：
;;   x y r g b | x y r g b | x y r g b
(define vertex-data
  (concat-vecs (vec-2 -0.6 -0.5) (vec-3 1.0 0.0 0.0)    ; 红
               (vec-2  0.6 -0.5) (vec-3 0.0 1.0 0.0)    ; 绿
               (vec-2  0.0  0.6) (vec-3 0.0 0.0 1.0)))  ; 蓝

(define program 0)
(define vao 0)
(define u-time-loc #f)
;; 这些缓冲句柄也放在顶层：关窗时 shutdown 要能拿到（0 = 还没创建）
(define vbo 0)

(define (init)
  (set! program (build-program (gl-vertex-shader vert-src)
                               (gl-fragment-shader frag-src)))

  (set! vao (vertex-array-new))
  (gl-bind-vertex-array vao)
  (set! vbo (buffer-new))
  (gl-bind-buffer gl-array-buffer vbo)
  (buffer-upload gl-array-buffer vertex-data gl-static-draw)

  ;; 步长 = 一个顶点占多少字节；偏移 = 这个属性前面要跳过多少字节。
  ;; 两个数都从布局算出来，不写字面量：以后把位置换成 vec-3，下面自动跟着对。
  (define stride (shader-stride-bytes 'vec-2 'vec-3))   ; 2 + 3 = 5 个 float = 20 字节
  (define color-offset (shader-byte-size 'vec-2))      ; 跳过位置那 2 个 float = 8 字节
  (gl-vertex-attrib-pointer 0 2 gl-float #f stride 0)
  (gl-enable-vertex-attrib-array 0)
  (gl-vertex-attrib-pointer 1 3 gl-float #f stride color-offset)
  (gl-enable-vertex-attrib-array 1)

  (gl-bind-vertex-array 0)

  ;; uniform 的位置在链接后查一次、存起来。找不到会返回 -1。
  (set! u-time-loc (uniform-location program "u_time")))

(define (draw)
  (define t (/ (current-inexact-monotonic-milliseconds) 1000.0))

  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear gl-color-buffer-bit)

  (program-use program)
  ;; 上传 uniform：必须在 program-use 之后，因为它作用在"当前程序"上。
  ;; uniform-set! 按类型符号自动选对 gl-uniform-*（'float→1f、'vec-3→3f、'mat-4→matrix-4fv）。
  ;; 裸写法： (gl-uniform-1f u-time-loc t)；uniform-set! 只是按类型符号帮你选函数。
  (uniform-set! u-time-loc 'float t)

  (gl-bind-vertex-array vao)
  (gl-draw-arrays gl-triangles 0 3)
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
  (program-delete program))

(define-values (frame canvas)
  (make-window #:title "03 着色器"
               #:init init
               #:draw draw
               #:on-close shutdown))
(send frame show #t)

;; uniform 每帧在变，需要定时器不断请求重绘
(define timer
  (new timer% [interval 16]
              [notify-callback (lambda () (send canvas refresh))]))
