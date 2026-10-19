#lang glsl-opengl
;; ============================================================
;; 06 · 坐标系：model / view / projection 三个矩阵
;; 运行：racket 06-coordinate-systems/coordinates.rkt
;; 上一课：05-transformations/transform.rkt
;; ============================================================
;; 目标：把立方体摆进一个 3D 世界。这件事拆成三段：
;;   ① model      把物体的局部坐标摆到世界里的某个位置、朝向
;;   ② view       把整个世界搬成"相机在原点、看向 -z"的样子
;;   ③ projection 把眼前那个视锥压成标准立方体，并让远的物体变小
;; 乘起来的顺序从右往左作用：顶点先被 model 搬，再被 view 搬，最后被 proj 压。
;; 这一课还第一次开深度缓冲，否则后画的立方体会盖住先画的、跟远近无关。

(require racket/class
         racket/gui
         racket/math
         "../lib/window.rkt"
         "../lib/gpu.rkt"
         "../lib/matrix.rkt")

;; ------------------------------------------------------------
;; 窗口尺寸：做成常量
;; ------------------------------------------------------------
;; 投影要一个 aspect = 宽/高，它必须和实际窗口同源，否则画面会横向或纵向拉伸。
(define win-w 800)
(define win-h 600)

;; aspect 的初始值；窗口缩放时由 #:on-size 更新。
;; lib/window.rkt 的 #:on-size 在窗口尺寸变化、viewport 重设之后被调用，参数就是新的
;; 客户区宽高；下面把它交给一个改 aspect 的回调，draw 下一帧重算投影就不变形了。
(define aspect (/ win-w win-h))

;; ------------------------------------------------------------
;; 一、生成纹理像素（同 04 课）
;; ------------------------------------------------------------
;; 出处：04-textures/textures.rkt 的 make-checker-texture。棋盘是本教程的惯用图案：
;; 贴对了是一格一格的方块，UV 错了或采样错了立刻看得出来。
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
;; 二、着色器：三个矩阵挨个乘上去
;; ------------------------------------------------------------
;; 一路都在 4x4 矩阵里算，顶点是 vec-4（第 4 个分量 w 恒为 1）。
;; 乘的顺序：model 最靠里（最先作用在顶点上），proj 最靠外。
(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec-3 a-pos)     ; 属性 0：局部坐标
        (layout (location 1) in vec-2 a-uv)      ; 属性 1：UV 坐标
        (uniform mat-4 u-model)                  ; 局部 → 世界
        (uniform mat-4 u-view)                   ; 世界 → 观察
        (uniform mat-4 u-proj)                   ; 观察 → 裁剪
        (out vec-2 v-uv)
        (define (main) void
          (set! gl-position (* u-proj (* u-view (* u-model (vec-4 a-pos 1.0)))))
          (set! v-uv a-uv))))

(define frag-src
  (glsl (version 330 core)
        (in vec-2 v-uv)
        (uniform sampler-2d u-tex)               ; 采样器：指向某张纹理
        (out vec-4 frag-color)
        (define (main) void
          (set! frag-color (texture u-tex v-uv)))))

;; ------------------------------------------------------------
;; 三、立方体数据：24 个顶点 + 36 个索引
;; ------------------------------------------------------------
;; 为什么是 24 个顶点而不是 8 个：立方体确有 8 个角，但每个角被 3 个面共用，
;; 而同一个角在三个面上需要的 UV 各不相同（每个面都要把 0..1 铺满）。
;; 一个顶点只能带一对 UV，所以每个面得自己摊一份 4 个顶点：6 面 × 4 = 24。
;; 法向是同样的道理（07 之后才会用到），本课只说 UV。
;; 布局：位置 vec-3 + UV vec-2 交错
;;    x  y  z  u  v | x  y  z  u  v | …
(define vertex-data
  (concat-vecs (vec-3 -0.5 -0.5 -0.5) (vec-2 0.0 0.0)     ; 后面 4 个角
               (vec-3  0.5 -0.5 -0.5) (vec-2 1.0 0.0)
               (vec-3  0.5  0.5 -0.5) (vec-2 1.0 1.0)
               (vec-3 -0.5  0.5 -0.5) (vec-2 0.0 1.0)
               (vec-3 -0.5 -0.5  0.5) (vec-2 0.0 0.0)     ; 前面
               (vec-3  0.5 -0.5  0.5) (vec-2 1.0 0.0)
               (vec-3  0.5  0.5  0.5) (vec-2 1.0 1.0)
               (vec-3 -0.5  0.5  0.5) (vec-2 0.0 1.0)
               (vec-3 -0.5  0.5  0.5) (vec-2 1.0 0.0)     ; 左面
               (vec-3 -0.5  0.5 -0.5) (vec-2 1.0 1.0)
               (vec-3 -0.5 -0.5 -0.5) (vec-2 0.0 1.0)
               (vec-3 -0.5 -0.5  0.5) (vec-2 0.0 0.0)
               (vec-3  0.5  0.5  0.5) (vec-2 1.0 0.0)     ; 右面
               (vec-3  0.5  0.5 -0.5) (vec-2 1.0 1.0)
               (vec-3  0.5 -0.5 -0.5) (vec-2 0.0 1.0)
               (vec-3  0.5 -0.5  0.5) (vec-2 0.0 0.0)
               (vec-3 -0.5 -0.5 -0.5) (vec-2 0.0 1.0)     ; 底面
               (vec-3  0.5 -0.5 -0.5) (vec-2 1.0 1.0)
               (vec-3  0.5 -0.5  0.5) (vec-2 1.0 0.0)
               (vec-3 -0.5 -0.5  0.5) (vec-2 0.0 0.0)
               (vec-3 -0.5  0.5 -0.5) (vec-2 0.0 1.0)     ; 顶面
               (vec-3  0.5  0.5 -0.5) (vec-2 1.0 1.0)
               (vec-3  0.5  0.5  0.5) (vec-2 1.0 0.0)
               (vec-3 -0.5  0.5  0.5) (vec-2 0.0 0.0)))

;; 每个面两个三角形。24 个顶点两两不共用，索引就是顺序往下数：
;; 6 面 × 6 个下标 = 36。
;; ★ 绕序：从物体外面看，每个三角形都必须是逆时针（CCW），否则打开
;;   背面剔除时这个面会消失。后面/右面/顶面最容易照前面/左面/底面写反；
;;   判断、手算和修正方法见教程 6.5.1。
(define indices
  (u16-array 0 3 2   2 1 0        ; 后面
             4 5 6   6 7 4        ; 前面
             8 9 10  10 11 8      ; 左面
             12 14 13  14 12 15   ; 右面
             16 17 18  18 19 16   ; 底面
             20 22 21  22 20 23)) ; 顶面

;; 十个立方体各自摆在哪里（世界坐标）。同一份顶点数据配十个不同的 model 矩阵，
;; 于是十次绘制出十个立方体——这正是 model 矩阵的意义。
(define cube-positions
  (list (vec-3  0.0  0.0   0.0)
        (vec-3  2.0  5.0 -15.0)
        (vec-3 -1.5 -2.2  -2.5)
        (vec-3 -3.8 -2.0 -12.3)
        (vec-3  2.4 -0.4  -3.5)
        (vec-3 -1.7  3.0  -7.5)
        (vec-3  1.3 -2.0  -2.5)
        (vec-3  1.5  2.0  -2.5)
        (vec-3  1.5  0.2  -1.5)
        (vec-3 -1.3  1.0  -1.5)))

(define program #f)
(define vao #f)
(define texture #f)
(define u-model-loc #f)
(define u-view-loc #f)
(define u-proj-loc #f)

(define (init)
  (set! program (own! 'program (build-program (gl-vertex-shader vert-src)
                                              (gl-fragment-shader frag-src))))

  ;; ---- 深度缓冲 ----
  ;; 每个像素记住目前画到的最浅的深度；新片元比它更远就直接丢弃。
  ;; 不开启的话，后画的片元无条件盖住先画的，遮挡和远近无关（见 6.5）。
  (gl-enable gl-depth-test)

  ;; ---- 背面剔除 ----
  ;; 闭合物体背对相机的面永远看不见，不必光栅化。默认正面是逆时针（CCW），
  ;; 这里剔除背面。注意 GL_CULL_FACE 常量因和函数 glCullFace 撞名，
  ;; 按本教程命名规则叫 var-cull-face；gl-cull-face 是函数（见 6.5.1）。
  (gl-enable var-cull-face)
  (gl-cull-face gl-back)

  ;; ---- 纹理：创建对象、设参数、上传像素（同 04 课） ----
  (set! texture (own! 'texture (texture-new)))
  (gl-bind-texture gl-texture-2d texture)
  (gl-tex-parameter-i gl-texture-2d gl-texture-wrap-s gl-repeat)
  (gl-tex-parameter-i gl-texture-2d gl-texture-wrap-t gl-repeat)
  (gl-tex-parameter-i gl-texture-2d gl-texture-min-filter gl-linear-mipmap-linear)
  (gl-tex-parameter-i gl-texture-2d gl-texture-mag-filter gl-linear)
  (define checker (make-checker-texture tex-size tex-cell))
  (gl-tex-image-2d gl-texture-2d 0 gl-rgba tex-size tex-size 0
                   gl-rgba gl-unsigned-byte checker)
  ;; 用了带 mipmap 的过滤就必须生成 mipmap，否则纹理不完整、采样是黑的。
  (gl-generate-mipmap gl-texture-2d)

  (program-use program)
  (uniform-set! (uniform-location program "u_tex") 'sampler-2d 0)
  (gl-active-texture gl-texture0)
  (gl-bind-texture gl-texture-2d texture)

  ;; ---- VAO / VBO / EBO ----
  (set! vao (own! 'vertex-array (vertex-array-new)))
  (gl-bind-vertex-array vao)

  (define vbo (own! 'buffer (buffer-new)))
  (gl-bind-buffer gl-array-buffer vbo)
  (buffer-upload gl-array-buffer vertex-data gl-static-draw)
  ;; 步长和偏移都从布局算出来，不写字面量：vec-3 + vec-2 = 5 个 float = 20 字节，
  ;; UV 前面要跳过位置那 3 个 float = 12 字节。
  (define stride (shader-stride-bytes 'vec-3 'vec-2))
  (define uv-offset (shader-byte-size 'vec-3))
  (gl-vertex-attrib-pointer 0 3 gl-float #f stride 0)
  (gl-enable-vertex-attrib-array 0)
  (gl-vertex-attrib-pointer 1 2 gl-float #f stride uv-offset)
  (gl-enable-vertex-attrib-array 1)

  (define ebo (own! 'buffer (buffer-new)))
  (gl-bind-buffer gl-element-array-buffer ebo)
  (buffer-upload gl-element-array-buffer indices gl-static-draw)

  (gl-bind-vertex-array 0)

  ;; 三个矩阵 uniform 的位置，链接后各查一次存起来，draw 里直接用。
  (set! u-model-loc (uniform-location program "u_model"))
  (set! u-view-loc (uniform-location program "u_view"))
  (set! u-proj-loc (uniform-location program "u_proj")))

(define (draw)
  (define t (/ (current-inexact-monotonic-milliseconds) 1000.0))

  (gl-clear-color 0.10 0.12 0.20 1.0)
  ;; 清屏掩码：01 课见过掩码——一个 bit 代表一块缓冲。这里用按位或把颜色和深度
  ;; 两块一起擦；只擦颜色的话，上一帧的深度还留着，这帧的遮挡就全错。
  (gl-clear (bitwise-ior gl-color-buffer-bit gl-depth-buffer-bit))

  (program-use program)

  ;; view：本课偷懒——相机就在原点看向 -z，于是把整个世界沿 -z 推 3 米即可
  ;; （等价于相机退到 z=+3 处）。真正的相机在 07 课，用 look-at 由 eye/center/up 算。
  (uniform-set! u-view-loc 'mat-4 (m-translate (vec-3 0.0 0.0 -3.0)))
  ;; projection：竖直视场角 45°、aspect 取当前窗口宽高比（由 #:on-size 维护）、
  ;; near 0.1、far 100。投影矩阵每帧重算，所以 aspect 一改，下一帧就生效。
  (uniform-set! u-proj-loc 'mat-4 (m-perspective (degrees->radians 45.0)
                                        aspect
                                        0.1 100.0))

  (gl-bind-vertex-array vao)
  ;; 同一份顶点数据，配上十个不同的 model 矩阵，画十次。
  (for ([pos (in-list cube-positions)] [i (in-naturals)])
    (define speed (+ 0.5 (* 0.3 i)))       ; 每个立方体转速不同，看得出是各自在转
    (define model (mat4-mul (m-translate pos) (m-rotate-x (* t speed))))
    (uniform-set! u-model-loc 'mat-4 model)
    (gl-draw-elements gl-triangles 36 gl-unsigned-short 0))
  (gl-bind-vertex-array 0))

(define-values (frame canvas)
  (make-window #:title "06 坐标系"
               #:width win-w
               #:height win-h
               #:init init
               #:draw draw
               ;; 窗口尺寸变化、viewport 重设之后被调用，参数是新的客户区宽高。
               #:on-size (lambda (cw ch) (set! aspect (/ cw ch)))
               #:on-close release-all!))
(send frame show #t)

;; 立方体在转，需要定时器不断请求重绘。
(define timer
  (new timer% [interval 16]
              [notify-callback (lambda () (send canvas refresh))]))

;; ------------------------------------------------------------
;; 试试看（改完重跑）：
;;   · 注释掉 (gl-enable gl-depth-test)：后画的立方体盖住先画的，遮挡全乱；
;;     恢复后立刻正常——这就是深度缓冲在干活。
;;   · 把 45.0 改成 90.0（fovy 变大）：视野更宽，物体在画面里变小、变多。
;;   · 把 near 从 0.1 改成 4.0：近平面切掉观察空间 z 大于 -4（即世界 z 大于 -1）
;;     的部分，世界原点那个立方体正好在里面，于是只有它消失。
;;   · 把某个位置的 z 从 -2.5 改成 5.0：view 是平移 -3，世界 z=5 落到观察空间
;;     z=+2——在相机背后（w 变成负数），整个被裁掉，于是它消失。
;; ============================================================