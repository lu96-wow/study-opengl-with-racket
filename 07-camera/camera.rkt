#lang glsl-opengl
;; ============================================================
;; 07 · 相机
;; 运行：racket 07-camera/camera.rkt
;; 上一课：06-coordinate-systems/coordinates.rkt
;; ============================================================
;; 06 课的 view 矩阵是拿 m-translate 把世界往后推 3 格——那等价于"相机钉死在原点、
;; 永远朝 -z 看"，转不了头。这一课把相机真正做出来：
;;   ① m-look-at(eye, center, up) 里的 f / s / u 三个基是怎么推出来的
;;   ② 它凭什么就是"世界 → 相机"的坐标变换
;;   ③ 让 eye 绕世界原点转圈，同一堆立方体从各个角度看
;; 立方体那边（顶点数据、位置表、model 的做法）全部沿用 06 课，这一课只换 view。
;; 相机不是 GPU 对象，只是一个每帧重算、再上传的 mat-4。

(require racket/class
         racket/gui                     ; timer%
         racket/math                    ; degrees->radians
         "../lib/window.rkt"
         "../lib/matrix.rkt")           ; m-look-at / m-translate / m-rotate-x / m-perspective

;; ------------------------------------------------------------
;; 一、窗口尺寸与 aspect
;; ------------------------------------------------------------
(define win-w 800)
(define win-h 600)
;; aspect 是投影矩阵的参数，必须和窗口的宽高比一致，否则画面被拉伸。
;; 它是可变的：窗口被拖成别的比例时，#:on-size 回调会把它重算（见文件末尾 make-window）。
(define aspect (/ win-w win-h))

;; ------------------------------------------------------------
;; 二、棋盘纹理（同 04 课：用代码生成，不依赖图片文件）
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
    (u8-array-set! px (+ i 3) 255))          ; alpha 不透明
  px)

;; ------------------------------------------------------------
;; 三、着色器：三个矩阵串起来
;; ------------------------------------------------------------
;; 顶点要连着过三道变换，写成矩阵就是从右往左乘：
;;   model：把单位立方体摆到世界里的某处
;;   view ：把世界换进相机坐标（本课的主角）
;;   proj ：把相机看到的视锥投影到 NDC
(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec-3 a-pos)
        (layout (location 1) in vec-2 a-uv)
        (uniform mat-4 u-model)
        (uniform mat-4 u-view)
        (uniform mat-4 u-proj)
        (out vec-2 v-uv)
        (define (main) void
          (set! gl-position (* u-proj (* u-view (* u-model (vec-4 a-pos 1.0)))))
          (set! v-uv a-uv))))

(define frag-src
  (glsl (version 330 core)
        (in vec-2 v-uv)
        (uniform sampler-2d u-tex)
        (out vec-4 frag-color)
        (define (main) void
          (set! frag-color (texture u-tex v-uv)))))

;; ------------------------------------------------------------
;; 四、立方体数据：24 个顶点 + 36 个索引（同 06 课）
;; ------------------------------------------------------------
;; 立方体 6 个面、每面 4 个角，共 24 个顶点。8 个角点本来可以共用，
;; 但每个面都要贴自己完整的一张纹理，UV 各不相同，所以按面拆开。
;; 尺寸 ±0.5（边长 1），index 每面 6 个（两个三角形），共 36 个。
(define vertex-data
  (concat-vecs
   ;; 后面 z = -0.5
   (vec-3 -0.5 -0.5 -0.5) (vec-2 0.0 0.0)
   (vec-3  0.5 -0.5 -0.5) (vec-2 1.0 0.0)
   (vec-3  0.5  0.5 -0.5) (vec-2 1.0 1.0)
   (vec-3 -0.5  0.5 -0.5) (vec-2 0.0 1.0)
   ;; 前面 z = +0.5
   (vec-3 -0.5 -0.5  0.5) (vec-2 0.0 0.0)
   (vec-3  0.5 -0.5  0.5) (vec-2 1.0 0.0)
   (vec-3  0.5  0.5  0.5) (vec-2 1.0 1.0)
   (vec-3 -0.5  0.5  0.5) (vec-2 0.0 1.0)
   ;; 左面 x = -0.5
   (vec-3 -0.5  0.5  0.5) (vec-2 1.0 0.0)
   (vec-3 -0.5  0.5 -0.5) (vec-2 1.0 1.0)
   (vec-3 -0.5 -0.5 -0.5) (vec-2 0.0 1.0)
   (vec-3 -0.5 -0.5  0.5) (vec-2 0.0 0.0)
   ;; 右面 x = +0.5
   (vec-3  0.5  0.5  0.5) (vec-2 1.0 0.0)
   (vec-3  0.5  0.5 -0.5) (vec-2 1.0 1.0)
   (vec-3  0.5 -0.5 -0.5) (vec-2 0.0 1.0)
   (vec-3  0.5 -0.5  0.5) (vec-2 0.0 0.0)
   ;; 底面 y = -0.5
   (vec-3 -0.5 -0.5 -0.5) (vec-2 0.0 1.0)
   (vec-3  0.5 -0.5 -0.5) (vec-2 1.0 1.0)
   (vec-3  0.5 -0.5  0.5) (vec-2 1.0 0.0)
   (vec-3 -0.5 -0.5  0.5) (vec-2 0.0 0.0)
   ;; 顶面 y = +0.5
   (vec-3 -0.5  0.5 -0.5) (vec-2 0.0 1.0)
   (vec-3  0.5  0.5 -0.5) (vec-2 1.0 1.0)
   (vec-3  0.5  0.5  0.5) (vec-2 1.0 0.0)
   (vec-3 -0.5  0.5  0.5) (vec-2 0.0 0.0)))

(define indices
  (u16-array 0 3 2   2 1 0          ; 后面
             4 5 6   6 7 4          ; 前面
             8 9 10  10 11 8        ; 左面
             12 14 13 14 12 15      ; 右面
             16 17 18 18 19 16      ; 底面
             20 22 21 22 20 23))    ; 顶面

;; 十个立方体在世界里的位置（learnopengl 那套散布法：近处零星几个、远处一大片）。
;; 同一个位置表在 06 课已经用过一遍，这里原样搬过来。
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

(define program 0)
(define vao 0)
(define texture 0)
;; 这些缓冲句柄也放在顶层：关窗时 shutdown 要能拿到（0 = 还没创建）
(define vbo 0)
(define ebo 0)

(define (init)
  (set! program (build-program (gl-vertex-shader vert-src)
                               (gl-fragment-shader frag-src)))

  ;; ---- 棋盘纹理（同 04 课）----
  (set! texture (texture-new))
  (gl-bind-texture gl-texture-2d texture)
  (gl-tex-parameter-i gl-texture-2d gl-texture-wrap-s gl-repeat)
  (gl-tex-parameter-i gl-texture-2d gl-texture-wrap-t gl-repeat)
  (gl-tex-parameter-i gl-texture-2d gl-texture-min-filter gl-linear-mipmap-linear)
  (gl-tex-parameter-i gl-texture-2d gl-texture-mag-filter gl-linear)
  (gl-tex-image-2d gl-texture-2d 0 gl-rgba tex-size tex-size 0
                   gl-rgba gl-unsigned-byte (make-checker-texture tex-size tex-cell))
  (gl-generate-mipmap gl-texture-2d)

  (program-use program)
  (uniform-set! (uniform-location program "u_tex") 'sampler-2d 0)
  (gl-active-texture gl-texture0)
  (gl-bind-texture gl-texture-2d texture)

  ;; ---- VAO / VBO / EBO ----
  (set! vao (vertex-array-new))
  (gl-bind-vertex-array vao)

  (set! vbo (buffer-new))
  (gl-bind-buffer gl-array-buffer vbo)
  (buffer-upload gl-array-buffer vertex-data gl-static-draw)
  ;; 步长和偏移都从布局算出来，不写字面量。
  (define stride (shader-stride-bytes 'vec-3 'vec-2))   ; 3 + 2 = 5 个 float = 20 字节
  (define uv-offset (shader-byte-size 'vec-3))         ; 跳过位置那 3 个 float = 12 字节
  (gl-vertex-attrib-pointer 0 3 gl-float #f stride 0)
  (gl-enable-vertex-attrib-array 0)
  (gl-vertex-attrib-pointer 1 2 gl-float #f stride uv-offset)
  (gl-enable-vertex-attrib-array 1)

  (set! ebo (buffer-new))
  (gl-bind-buffer gl-element-array-buffer ebo)
  (buffer-upload gl-element-array-buffer indices gl-static-draw)

  (gl-bind-vertex-array 0)

  ;; ★一开始画立体的东西就得开深度测试：谁在前、谁被挡住由深度缓冲裁决，
  ;;   否则后画的近处立方体会盖住先画的远处立方体。
  (gl-enable gl-depth-test))

(define (draw)
  (gl-clear-color 0.10 0.12 0.20 1.0)
  ;; 掩码按位或：颜色和深度一起清。只清颜色的话，上一帧的深度值还留在缓冲里，
  ;; 这一帧的遮挡判断就全错了。
  (gl-clear (bitwise-ior gl-color-buffer-bit gl-depth-buffer-bit))

  ;; ---- 相机：绕着世界原点转圈 ----
  ;; eye 在半径 3、高 1.2 的圆上走：t 每秒转 0.5 弧度，转一圈约 12.6 秒。
  ;; y 固定 1.2 是让相机略高于目标（原点），于是有一点俯视感，看得到立方体顶面。
  (define t (/ (current-inexact-monotonic-milliseconds) 1000.0))
  (define eye (vec-3 (* 3.0 (cos (* t 0.5))) 1.2 (* 3.0 (sin (* t 0.5)))))
  ;; 每帧重算：eye 在动，view 就得跟着动。
  ;; center 是相机看的目标；up 只说"头顶朝哪"，长度无所谓（look-at 会归一化）。
  (define view (m-look-at eye (vec-3 0.0 0.0 0.0) (vec-3 0.0 1.0 0.0)))

  ;; 投影同 06：竖直视场 45°、aspect 与窗口同源、近远平面 0.1 / 100。
  (define proj (m-perspective (degrees->radians 45.0) aspect 0.1 100.0))

  (program-use program)
  (gl-bind-vertex-array vao)

  ;; view / proj 整帧只算一份，循环外上传一次；model 每个立方体不同，得在循环里传。
  ;; mat-4 是列主序的 f32vector，和 uniform 的布局一致，所以 transpose 传 #f。
  (uniform-set! (uniform-location program "u_view") 'mat-4 view)
  (uniform-set! (uniform-location program "u_proj") 'mat-4 proj)

  (for ([pos (in-list cube-positions)])
    ;; model = 平移 × 绕 x 自转 × 绕 y 自转：先让立方体转到自己的姿态，再挪到位置。
    ;; 组合顺序从右往左作用在顶点上；把平移写在右边的话，立方体会被旋转甩出去。
    (define model (mat4-mul (m-translate pos)
                            (mat4-mul (m-rotate-x (* t 0.5))
                                      (m-rotate-y t))))
    (uniform-set! (uniform-location program "u_model") 'mat-4 model)
    ;; 一个立方体 36 个索引：24 个顶点、6 个面、每面 6 个索引。
    (gl-draw-elements gl-triangles 36 gl-unsigned-short 0))

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
  (make-window #:title "07 相机"
               #:width win-w
               #:height win-h
               #:init init
               #:draw draw
               ;; 窗口尺寸变了 → 重设 viewport 之后回调这里，把 aspect 一起改掉
               #:on-size (lambda (cw ch) (set! aspect (/ cw ch)))
               #:on-close shutdown))
(send frame show #t)

;; 定时器只负责"每隔 16ms 请求重绘"，绘制永远发生在 draw 里（同 01 课）。
(define timer
  (new timer% [interval 16]
              [notify-callback (lambda () (send canvas refresh))]))

;; ------------------------------------------------------------
;; 试试看（改完重跑）：
;;   · center 改成 eye 正下方、或 up 改成 (0,0,0) → f 与 up 平行，叉积为 0，
;;     normalize 零向量得 NaN，画面整个崩掉（原因见 7.5）
;;   · 半径 3.0 改成 1.0 → 立方体群冲出视野，被近平面 0.1 裁掉
;;   · up 改成 (0,0,1) → u 轴换了个方向，画面"躺倒"
;; ============================================================
