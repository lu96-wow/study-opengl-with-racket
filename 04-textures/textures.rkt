#lang glsl-opengl
;; ============================================================
;; 04 · 纹理
;; 运行：racket 04-textures/textures.rkt
;; 上一课：03-shaders/shaders.rkt
;; ============================================================
;; 目标：把一个矩形"贴"上一张图。纹理要讲的四件事：
;;   ① 纹理对象：把像素数据上传到显存
;;   ② UV 坐标：顶点上告诉 GPU "这个角取图的哪里"
;;   ③ 环绕（wrap）：UV 超出 0..1 时怎么办
;;   ④ 过滤 + mipmap：纹理被放大/缩小时怎么取像素
;; 这里用代码生成两张图（棋盘 + 径向渐变），不用外部图片文件。

(require racket/class
         "../lib/window.rkt"
         "../lib/gpu.rkt")

;; ------------------------------------------------------------
;; 一、生成纹理像素
;; ------------------------------------------------------------
;; OpenGL 要的是"一条连续的 RGBA 字节流"（每像素 4 个字节，0-255）。
;; 这里画一张 size×size 的棋盘：每个 cell×cell 的方块在两种颜色之间交替。
(define tex-size 64)
(define tex-cell 8)

(define (make-checker-texture size cell)
  ;; 裸写法： (make-u8vector (* size size 4) 255) / (u8vector-set! px i v)
  ;; u8-array 家族是命名版（u8vector 即 bytes），元素范围 0..255。
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

;; 第二张图是径向渐变：每像素按到中心的距离取灰度，中心亮、边缘暗。
;; 选它是为了和棋盘的"硬边"形成对比——两个采样结果 mix 之后两种特征都还看得见。
(define (make-radial-texture size)
  (define center (/ (- size 1) 2.0))                 ; 图中心的像素坐标
  (define span (sqrt (* 2.0 center center)))         ; 中心到角的距离，用来归一化
  (define px (make-u8-array (* size size 4) 255))
  (for* ([y (in-range size)] [x (in-range size)])
    (define dx (- x center))
    (define dy (- y center))
    (define d (sqrt (+ (* dx dx) (* dy dy))))       ; 到中心的距离
    (define v (inexact->exact (round (* 255.0 (- 1.0 (/ d span))))))
    (define i (* (+ (* y size) x) 4))
    (u8-array-set! px i v)
    (u8-array-set! px (+ i 1) v)
    (u8-array-set! px (+ i 2) v)
    (u8-array-set! px (+ i 3) 255))          ; alpha 不透明
  px)

;; ------------------------------------------------------------
;; 二、着色器：用 UV 采样纹理
;; ------------------------------------------------------------
;; 顶点多了一个属性 a-uv：每个顶点对应图片上的哪个位置（0..1）。
;; 片元着色器用 sampler-2d（采样器）按 UV 去"取颜色"。
;; v-uv 和颜色一样会被插值，所以矩形内部每个像素都拿到自己的 UV。
(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec-2 a-pos)     ; 属性 0：位置
        (layout (location 1) in vec-2 a-uv)      ; 属性 1：UV 坐标
        (out vec-2 v-uv)
        (define (main) void
          (set! gl-position (vec-4 a-pos 0.0 1.0))
          (set! v-uv a-uv))))

(define frag-src
  (glsl (version 330 core)
        (in vec-2 v-uv)
        (uniform sampler-2d u-tex)               ; 采样器：指向某张纹理
        (uniform sampler-2d u-overlay)           ; 第二个采样器：指向叠在上面的那张
        (out vec-4 frag-color)
        (define (main) void
          ;; texture(采样器, UV) → 这张图在 UV 处的颜色
          ;; mix(a, b, t) = a*(1-t) + b*t：GLSL 内建的逐分量线性插值。
          ;; t=0 全取 a、t=1 全取 b；0.35 让棋盘占大头、径向渐变盖在上面。
          (set! frag-color (mix (texture u-tex v-uv) (texture u-overlay v-uv) 0.35)))))

;; ------------------------------------------------------------
;; 三、顶点数据：位置(vec-2) + UV(vec-2) 交错，共 4 个 float = 16 字节
;; ------------------------------------------------------------
;;    x  y  u  v | x  y  u  v | …
;; UV 约定：(0,0) 是图的一角，(1,1) 是对角。给矩形四个角配上对应 UV。
(define vertex-data
  (concat-vecs (vec-2  0.5  0.5) (vec-2 1.0 1.0)     ; 右上
               (vec-2  0.5 -0.5) (vec-2 1.0 0.0)     ; 右下
               (vec-2 -0.5 -0.5) (vec-2 0.0 0.0)     ; 左下
               (vec-2 -0.5  0.5) (vec-2 0.0 1.0)))   ; 左上

(define indices (u16-array 0 1 2   0 2 3))          ; 两个三角形拼成矩形

(define program #f)
(define vao #f)
(define texture #f)
(define overlay #f)

(define (init)
  (set! program (own! 'program (build-program (gl-vertex-shader vert-src)
                                              (gl-fragment-shader frag-src))))

  ;; ---- 3.1 创建并上传纹理 ----
  ;; 纹理对象：显存里的一块"图"。texture-new 生成编号，gl-bind-texture 设为当前。
  (set! texture (own! 'texture (texture-new)))
  (gl-bind-texture gl-texture-2d texture)

  ;; 环绕方式：UV 超出 0..1 时，REPEAT = 平铺重复（另有 CLAMP_TO_EDGE 等）。
  (gl-tex-parameter-i gl-texture-2d gl-texture-wrap-s gl-repeat)
  (gl-tex-parameter-i gl-texture-2d gl-texture-wrap-t gl-repeat)
  ;; 过滤：放大用 LINEAR（平滑）；缩小用 LINEAR_MIPMAP_LINEAR（在 mipmap 层之间插值）。
  (gl-tex-parameter-i gl-texture-2d gl-texture-min-filter gl-linear-mipmap-linear)
  (gl-tex-parameter-i gl-texture-2d gl-texture-mag-filter gl-linear)

  ;; 上传像素：gl-tex-image-2d(目标, mip 层, 内部格式, 宽, 高, 边框, 输入格式, 输入类型, 数据)
  ;; 这里 RGBA8：内部格式/输入格式都是 gl-rgba，类型 gl-unsigned-byte。
  (define checker (make-checker-texture tex-size tex-cell))
  (gl-tex-image-2d gl-texture-2d 0 gl-rgba tex-size tex-size 0
                   gl-rgba gl-unsigned-byte checker)

  ;; ★用了 mipmap 过滤就必须生成 mipmap，否则纹理不完整、采样结果是黑的（常见坑）。
  (gl-generate-mipmap gl-texture-2d)

  ;; ---- 3.2 把纹理告诉采样器 ----
  ;; 采样器 u-tex 是一个整数，含义是"用几号纹理单元"。
  ;; 纹理单元是采样器的"插槽"：先用 gl-active-texture 选槽，再把纹理绑到槽上。
  ;; 只有一个纹理时用 0 号槽。
  (program-use program)                       ; uniform 作用在"当前程序"上
  (uniform-set! (uniform-location program "u_tex") 'sampler-2d 0)
  (gl-active-texture gl-texture0)
  (gl-bind-texture gl-texture-2d texture)

  ;; 第二张贴图要第二个插槽：0 号槽给 u-tex、1 号槽给 u-overlay，采样器靠 uniform 整数认槽。
  ;; 步骤和上面一样：选槽 → 绑纹理 → 设参数 → 传像素 → 生成 mipmap → 报槽号。
  (set! overlay (own! 'texture (texture-new)))
  ;; 1 号槽就是 gl-texture1；槽号常量连续（GL_TEXTURE0 + i 即第 i 号槽），
  ;; 本库收录到 gl-texture3，循环里仍可写 (+ gl-texture0 i)。
  (gl-active-texture gl-texture1)
  (gl-bind-texture gl-texture-2d overlay)
  (gl-tex-parameter-i gl-texture-2d gl-texture-wrap-s gl-repeat)
  (gl-tex-parameter-i gl-texture-2d gl-texture-wrap-t gl-repeat)
  (gl-tex-parameter-i gl-texture-2d gl-texture-min-filter gl-linear-mipmap-linear)
  (gl-tex-parameter-i gl-texture-2d gl-texture-mag-filter gl-linear)
  (gl-tex-image-2d gl-texture-2d 0 gl-rgba tex-size tex-size 0
                   gl-rgba gl-unsigned-byte (make-radial-texture tex-size))
  (gl-generate-mipmap gl-texture-2d)
  (uniform-set! (uniform-location program "u_overlay") 'sampler-2d 1)

  ;; ---- 3.3 VAO / VBO / EBO ----
  (set! vao (own! 'vertex-array (vertex-array-new)))
  (gl-bind-vertex-array vao)

  (define vbo (own! 'buffer (buffer-new)))
  (gl-bind-buffer gl-array-buffer vbo)
  (buffer-upload gl-array-buffer vertex-data gl-static-draw)
  ;; 步长和偏移都从布局算出来，不写字面量。
  (define stride (shader-stride-bytes 'vec-2 'vec-2))   ; 2 + 2 = 4 个 float = 16 字节
  (define uv-offset (shader-byte-size 'vec-2))         ; 跳过位置那 2 个 float = 8 字节
  (gl-vertex-attrib-pointer 0 2 gl-float #f stride 0)
  (gl-enable-vertex-attrib-array 0)
  (gl-vertex-attrib-pointer 1 2 gl-float #f stride uv-offset)
  (gl-enable-vertex-attrib-array 1)

  (define ebo (own! 'buffer (buffer-new)))
  (gl-bind-buffer gl-element-array-buffer ebo)
  (buffer-upload gl-element-array-buffer indices gl-static-draw)

  (gl-bind-vertex-array 0))

(define (draw)
  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear gl-color-buffer-bit)

  (program-use program)
  (gl-bind-vertex-array vao)
  (gl-draw-elements gl-triangles 6 gl-unsigned-short 0)
  (gl-bind-vertex-array 0))

(define-values (frame canvas)
  (make-window #:title "04 纹理"
               #:init init
               #:draw draw
               #:on-close release-all!))
(send frame show #t)

;; ------------------------------------------------------------
;; 试试看（改完重跑）：
;;   · 把 UV 的 1.0 改成 2.0 → 纹理平铺 2×2（REPEAT 生效）
;;   · gl-texture-wrap-s 改成 gl-clamp-to-edge → 超出部分拉边缘像素
;;   · mag-filter 改成 gl-nearest → 放大时看到方块像素（不插值）
;;   · 去掉 gl-generate-mipmap → 画面变黑（用了 mipmap 过滤却没生成）
;;
;; 换成真实图片：可以用 racket/draw 读文件（read-bitmap + get-argb-pixels），
;; 拿到 RGBA 字节后同样喂给 gl-tex-image-2d。
;; ============================================================
