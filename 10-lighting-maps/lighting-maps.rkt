#lang glsl-opengl
;; ============================================================
;; 10 · 光照贴图：diffuse map + specular map
;; 运行：racket 10-lighting-maps/lighting-maps.rkt
;; 上一课：09-basic-lighting/basic-lighting.rkt
;; ============================================================
;; 目标：09 课里物体颜色是一个 uniform（u-object-color）——整块立方体一个色，每个像素
;; 反射同样的红绿蓝。真实物体表面各处的颜色并不一样，于是本课把这个 uniform 换成一张
;; 贴图：逐片元采样，采到什么颜色就反射什么颜色。这张图叫 diffuse map。
;;
;; 贴图也不只能当"颜色"。09 课的镜面高光整面都可能出现，而真实的高光只长在某些地方
;; （金属字只有笔画反光、磨亮的边角才有亮点），所以第二张贴图当开关用：它的 r 分量乘进
;; spec 项，白的地方允许高光、黑的地方高光被乘成 0。这张图叫 specular map。
;;
;; 纹理槽的机制也不新：04 课的 4.8 已经用 0 号槽 + 1 号槽叠过两张图。区别在用法——
;; 04 课用 mix 把两张图混成一个颜色（你中有我），本课是两张图各管一项光照（各司其职）。

(require racket/class
         racket/math                     ; degrees->radians
         "../lib/window.rkt"
         "../lib/matrix.rkt")            ; m-look-at / m-perspective / m-normal-matrix / …

;; ------------------------------------------------------------
;; 一、窗口尺寸与 aspect（同 06 / 08 / 09 课）
;; ------------------------------------------------------------
;; 投影要一个 aspect = 宽/高，必须和窗口客户区同源，否则画面会被拉伸。
(define win-w 800)
(define win-h 600)
(define aspect (/ win-w win-h))

;; ------------------------------------------------------------
;; 二、两张贴图的像素数据（都在 CPU 上算出来，不用外部图片文件）
;; ------------------------------------------------------------
(define tex-size 64)
(define tex-cell 8)

;; ---- diffuse map：棋盘 ----
;; 这个函数逐字来自 04-textures/textures.rkt 的 make-checker-texture，一行没改。
;; 本课在意的不是"棋盘怎么画"，而是"这张图被当成了物体颜色、逐片元取用"。
;; 每 cell×cell 的方块在两种颜色之间交替，共 8×8 个方块。
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

;; ---- specular map：中心一个白圆、其余全黑 ----
;; 注意它不是一个"颜色"：白圆里的像素 r = 255（采样后 1.0），圆外 r = 0。
;; 片元拿 r 乘进 spec 项，圆外的高光就被乘成 0——这张图只回答"这里允不允许出现高光"，
;; 不回答"高光是什么颜色"。半径取 size/4：圆正好盖住 09 课正面那块高光，面的大部分
;; 又被关掉，"高光只在圆内"这件事一眼能看出来。
(define (make-spec-mask-texture size)
  (define center (/ (- size 1) 2.0))         ; 图中心的像素坐标
  (define radius (/ size 4.0))               ; 白圆半径 ≈ 16 像素
  (define px (make-u8-array (* size size 4) 255))
  (for* ([y (in-range size)] [x (in-range size)])
    (define dx (- x center))
    (define dy (- y center))
    (define d (sqrt (+ (* dx dx) (* dy dy))))          ; 到中心的距离
    (define v (if (<= d radius) 255 0))                ; 圆内白、圆外黑
    (define i (* (+ (* y size) x) 4))
    (u8-array-set! px i v)
    (u8-array-set! px (+ i 1) v)
    (u8-array-set! px (+ i 2) v)
    (u8-array-set! px (+ i 3) 255))          ; alpha 不透明
  px)

;; ------------------------------------------------------------
;; 三、着色器
;; ------------------------------------------------------------
;; 顶点着色器和 09 课几乎一样，只多了一个属性 uv（04 课用过的 UV，本课第一次和法线
;; 同处一个顶点）和一条 out v-uv。位置、法线、UV 三者都在同一份交错缓冲里，各占一个
;; 属性槽。三项光照仍然逐片元算，这里照旧只负责把数据递下去。
(glsl-shader vert-src
  (version 330 core)
        (layout (location 0) in vec-3 pos)      ; 属性 0：局部坐标
        (layout (location 1) in vec-3 normal)   ; 属性 1：法线（09 课第一次用）
        (layout (location 2) in vec-2 uv)       ; 属性 2：UV（04 课第一次用）
        (uniform mat-4 u-model)                   ; 局部 → 世界
        (uniform mat-4 u-view)                    ; 世界 → 观察
        (uniform mat-4 u-proj)                    ; 观察 → 裁剪
        (uniform mat-3 u-normal-matrix)            ; 法线专用的 mat-3（为什么见 09 课 9.2）
        (out vec-3 v-normal)
        (out vec-3 v-frag-pos)
        (out vec-2 v-uv)
        (define (main) void
          (set! v-normal (* u-normal-matrix normal))
          (set! v-frag-pos (vec-3 (* u-model (vec-4 pos 1.0))))
          (set! v-uv uv)
          (set! gl-position (* u-proj (* u-view (* u-model (vec-4 pos 1.0)))))))

;; 物体的片元：09 课那三项一项没少，只是每一项现在都乘了一张贴图。
;;
;;   物体颜色  diffuse-color = texture(u-diffuse-map, v-uv)
;;     —— 09 课的 u-object-color 被这张图顶替了，颜色逐像素来自贴图。
;;   高光开关  spec-mask = texture(u-spec-map, v-uv).x
;;     —— 一个 0..1 的标量：白 = 1 允许高光，黑 = 0 把高光乘没。
;;
;; 哪张图乘进哪一项是刻意的：
;;   ambient × diffuse-color   保底光也是"漫反射出去的光"，颜色同样由物体决定
;;   diff    × diffuse-color   漫反射项：面越正对光越亮，颜色仍是物体的
;;   spec    × spec-mask       高光项：与物体颜色无关（高光就是光源的颜色），只由开关管
;; 所以 diffuse map 决定"这个位置反射什么颜色"，specular map 决定"这个位置反不反光"。
;; 背光面 diff 为 0、spec 也为 0，贴图乘以 0 还是 0——背光面照样暗，贴图不会把面照亮。
(glsl-shader frag-src
  (version 330 core)
        (in vec-3 v-normal)
        (in vec-3 v-frag-pos)
        (in vec-2 v-uv)
        (uniform vec-3 u-light-pos)               ; 光源位置（世界坐标）
        (uniform vec-3 u-light-color)             ; 光的颜色（同 08 课）
        (uniform vec-3 u-view-pos)                ; 相机位置（世界坐标）——高光要它
        (uniform sampler-2d u-diffuse-map)        ; 0 号槽：物体颜色
        (uniform sampler-2d u-spec-map)           ; 1 号槽：高光开关
        (out vec-4 frag-color)
        (define (main) void
          (vec-3 norm (normalize v-normal))
          (vec-3 light-dir (normalize (- u-light-pos v-frag-pos)))
          ;; texture(...) 返回 vec-4，颜色只要前三个分量，套一层 vec-3(...) 丢掉 alpha。
          (vec-3 diffuse-color (vec-3 (texture u-diffuse-map v-uv)))
          ;; 只取 r 分量（三个通道同值，取哪个都一样）：采样后是 0.0 ~ 1.0。
          (float spec-mask (x (texture u-spec-map v-uv)))
          (float ambient 0.1)
          (float diff (max (dot norm light-dir) 0.0))
          (vec-3 reflect-dir (reflect (- light-dir) norm))
          (vec-3 view-dir (normalize (- u-view-pos v-frag-pos)))
          (float spec (* (pow (max (dot view-dir reflect-dir) 0.0) 32.0) 0.5))
          ;; spec × spec-mask 是个标量，先用 vec-3(...) 摊成 vec-3 再加——三项里只有它跟物体颜色无关。
          (vec-3 lit (+ (* ambient diffuse-color)
                       (* diff diffuse-color)
                       (vec-3 (* spec spec-mask))))
          ;; 09 课最后那一步还在：三项之和再乘光色（物体色已经长在 diffuse-color 里了）。
          (set! frag-color (vec-4 (* lit u-light-color) 1.0))))

;; ------------------------------------------------------------
;; 四、立方体几何：24 个顶点，每个带位置 + 法线 + UV
;; ------------------------------------------------------------
;; 还是 09 课那个立方体、同样的 24 顶点 / 36 索引（06 课解释过为什么不是 8 个）。
;; 本课每个顶点又多带一条 UV：贴图要它才知道"这个角取图的哪里"。
;;
;; 布局仍旧交给 shader-struct：字段表里多加一个 (vec-2 uv)，步长自己变成 32 字节，
;; uv 的偏移也自己算出来——下面的 attrib 指针照样只写字段名，一个字节字面量都没有。
(shader-struct vertex (vec-3 pos) (vec-3 normal) (vec-2 uv))

;; 每个面的 4 个角依次配 UV (0,0) (1,0) (1,1) (0,1)：贴图正好铺满一个面、不重复也不拉伸。
;; 于是每个面的中心都是 UV (0.5,0.5)——specular map 的白圆就落在面的正中央，
;; 和 09 课正面那块高光的位置对上。
(define cube-verts
  (list
   ;; 后面 z = -0.5，法线朝 -z
   (vertex (vec-3 -0.5 -0.5 -0.5) (vec-3 0.0 0.0 -1.0) (vec-2 0.0 0.0))
   (vertex (vec-3  0.5 -0.5 -0.5) (vec-3 0.0 0.0 -1.0) (vec-2 1.0 0.0))
   (vertex (vec-3  0.5  0.5 -0.5) (vec-3 0.0 0.0 -1.0) (vec-2 1.0 1.0))
   (vertex (vec-3 -0.5  0.5 -0.5) (vec-3 0.0 0.0 -1.0) (vec-2 0.0 1.0))
   ;; 前面 z = +0.5，法线朝 +z
   (vertex (vec-3 -0.5 -0.5  0.5) (vec-3 0.0 0.0 1.0) (vec-2 0.0 0.0))
   (vertex (vec-3  0.5 -0.5  0.5) (vec-3 0.0 0.0 1.0) (vec-2 1.0 0.0))
   (vertex (vec-3  0.5  0.5  0.5) (vec-3 0.0 0.0 1.0) (vec-2 1.0 1.0))
   (vertex (vec-3 -0.5  0.5  0.5) (vec-3 0.0 0.0 1.0) (vec-2 0.0 1.0))
   ;; 左面 x = -0.5，法线朝 -x
   (vertex (vec-3 -0.5  0.5  0.5) (vec-3 -1.0 0.0 0.0) (vec-2 0.0 0.0))
   (vertex (vec-3 -0.5  0.5 -0.5) (vec-3 -1.0 0.0 0.0) (vec-2 1.0 0.0))
   (vertex (vec-3 -0.5 -0.5 -0.5) (vec-3 -1.0 0.0 0.0) (vec-2 1.0 1.0))
   (vertex (vec-3 -0.5 -0.5  0.5) (vec-3 -1.0 0.0 0.0) (vec-2 0.0 1.0))
   ;; 右面 x = +0.5，法线朝 +x
   (vertex (vec-3  0.5  0.5  0.5) (vec-3 1.0 0.0 0.0) (vec-2 0.0 0.0))
   (vertex (vec-3  0.5  0.5 -0.5) (vec-3 1.0 0.0 0.0) (vec-2 1.0 0.0))
   (vertex (vec-3  0.5 -0.5 -0.5) (vec-3 1.0 0.0 0.0) (vec-2 1.0 1.0))
   (vertex (vec-3  0.5 -0.5  0.5) (vec-3 1.0 0.0 0.0) (vec-2 0.0 1.0))
   ;; 底面 y = -0.5，法线朝 -y
   (vertex (vec-3 -0.5 -0.5 -0.5) (vec-3 0.0 -1.0 0.0) (vec-2 0.0 0.0))
   (vertex (vec-3  0.5 -0.5 -0.5) (vec-3 0.0 -1.0 0.0) (vec-2 1.0 0.0))
   (vertex (vec-3  0.5 -0.5  0.5) (vec-3 0.0 -1.0 0.0) (vec-2 1.0 1.0))
   (vertex (vec-3 -0.5 -0.5  0.5) (vec-3 0.0 -1.0 0.0) (vec-2 0.0 1.0))
   ;; 顶面 y = +0.5，法线朝 +y
   (vertex (vec-3 -0.5  0.5 -0.5) (vec-3 0.0 1.0 0.0) (vec-2 0.0 0.0))
   (vertex (vec-3  0.5  0.5 -0.5) (vec-3 0.0 1.0 0.0) (vec-2 1.0 0.0))
   (vertex (vec-3  0.5  0.5  0.5) (vec-3 0.0 1.0 0.0) (vec-2 1.0 1.0))
   (vertex (vec-3 -0.5  0.5  0.5) (vec-3 0.0 1.0 0.0) (vec-2 0.0 1.0))))

;; 每个顶点铺平成一个 f32vector（8 个 float = 3 位置 + 3 法线 + 2 UV），24 个接起来
;; 就是整条 VBO。交错格式仍由 shader-struct 的字段表决定，代码里没有字节字面量。
(define cube-data (apply concat-vecs (map vertex->f32vector cube-verts)))

;; 每个面两个三角形、6 个下标，六个面合计 36（同 06 / 08 / 09 课）。
(define indices
  (u16-array 0 3 2   2 1 0        ; 后面
             4 5 6   6 7 4        ; 前面
             8 9 10  10 11 8      ; 左面
             12 14 13  14 12 15   ; 右面
             16 17 18  18 19 16   ; 底面
             20 22 21  22 20 23)) ; 顶面

;; ------------------------------------------------------------
;; 五、场景：一个物体 + 一盏光 + 一台不动的相机
;; ------------------------------------------------------------
;; 物体颜色没有了——它现在在 diffuse map 里。其余参数和 09 课一样。
(define light-color (vec-3 1.0 1.0 1.0))        ; 光的颜色：白
(define light-pos (vec-3 1.2 1.0 2.0))          ; 光在世界里的位置：物体右上前方
(define eye-pos (vec-3 0.0 0.0 3.0))            ; 相机位置：本课不动

;; model 是 09 课那个固定的倾斜姿态：先绕 x 轴 -12°，再绕 y 轴 15°（从右往左作用）。
;; 为什么这么摆，09 课逐面算过：正面法线 ≈ (0.25, 0.21, 0.95)，落在"光与眼睛的中间
;; 方向"上 → 中央一块高光；左面和底面背对光 → 只剩 ambient。本课要看的正是这一块：
;; 高光的位置没变，但现在它能不能出现由 specular map 的白圆说了算。
(define model
  (mat4-mul (m-rotate-y (degrees->radians 15.0))
            (m-rotate-x (degrees->radians -12.0))))

(define object-program #f)
(define vao #f)
(define diffuse-map #f)
(define spec-map #f)
;; uniform 位置每个 program 各有一份，链接完查一次存起来（同 08 / 09 课）。

;; 本课不画灯立方体。08 / 09 课画它，是为了说明"光在哪"——一个白方块让 u-light-pos
;; 那个坐标看得见。这一课的焦点是贴图，多画一个方块只会挡在物体前面添乱，
;; 所以光位置固定 (1.2, 1.0, 2.0)、只当坐标用（09 课末尾也提过：灯落在画面之外
;; 不影响光照，光不必出现在画面里才能照亮物体）。

(glsl-uniform-setter! object-program vert-src frag-src)

(define (init)
  (set! object-program (build-program (gl-vertex-shader vert-src)
                                      (gl-fragment-shader frag-src)))

  ;; 画立体的东西就得开深度测试（06 课）。
  (gl-enable gl-depth-test)

  ;; 链接完把各 uniform 位置查一次存起来（采样器的槽号也要用它，所以放在贴图之前）。

  ;; ---- 两张贴图绑到 0 号、1 号槽 ----
  ;; uniform 作用在"当前程序"上，所以先 use（同 04 课 4.8）。
  (program-use object-program)

  ;; diffuse map → 0 号槽。上传步骤和 04 课一模一样：选槽 → 绑纹理 → 设参数 →
  ;; 传像素 → 生成 mipmap → 把槽号告诉采样器。
  (set! diffuse-map (texture-new))
  (gl-active-texture gl-texture0)
  (gl-bind-texture gl-texture-2d diffuse-map)
  (gl-tex-parameter-i gl-texture-2d gl-texture-wrap-s gl-repeat)
  (gl-tex-parameter-i gl-texture-2d gl-texture-wrap-t gl-repeat)
  (gl-tex-parameter-i gl-texture-2d gl-texture-min-filter gl-linear-mipmap-linear)
  (gl-tex-parameter-i gl-texture-2d gl-texture-mag-filter gl-linear)
  (gl-tex-image-2d gl-texture-2d 0 gl-rgba tex-size tex-size 0
                   gl-rgba gl-unsigned-byte (make-checker-texture tex-size tex-cell))
  ;; 用了 mipmap 过滤就必须生成 mipmap，否则纹理不完整、采样结果是黑的（04 课的坑）。
  (gl-generate-mipmap gl-texture-2d)
  (set-u-diffuse-map! 0)

  ;; specular map → 1 号槽。槽号常量连续：第 i 号槽写 (+ gl-texture0 i)，
  ;; 本课只有两张图，直接写 gl-texture1。
  (set! spec-map (texture-new))
  (gl-active-texture gl-texture1)
  (gl-bind-texture gl-texture-2d spec-map)
  (gl-tex-parameter-i gl-texture-2d gl-texture-wrap-s gl-repeat)
  (gl-tex-parameter-i gl-texture-2d gl-texture-wrap-t gl-repeat)
  (gl-tex-parameter-i gl-texture-2d gl-texture-min-filter gl-linear-mipmap-linear)
  (gl-tex-parameter-i gl-texture-2d gl-texture-mag-filter gl-linear)
  (gl-tex-image-2d gl-texture-2d 0 gl-rgba tex-size tex-size 0
                   gl-rgba gl-unsigned-byte (make-spec-mask-texture tex-size))
  (gl-generate-mipmap gl-texture-2d)
  (set-u-spec-map! 1)

  ;; ---- VAO / VBO / EBO ----
  (set! vao (vertex-array-new))
  (gl-bind-vertex-array vao)

  (define vbo (buffer-new))
  (gl-bind-buffer gl-array-buffer vbo)
  (buffer-upload gl-array-buffer cube-data gl-static-draw)
  ;; shader 的 in 名和 shader-struct 字段名同名，glsl-vertex-layout! 按名字自动对齐：
  ;; slot 取自 shader 的 location，步长/偏移取自字段表（无字节字面量）。
  (glsl-vertex-layout! vert-src vertex)

  (define ebo (buffer-new))
  (gl-bind-buffer gl-element-array-buffer ebo)
  (buffer-upload gl-element-array-buffer indices gl-static-draw)

  (gl-bind-vertex-array 0))

(define (draw)
  (gl-clear-color 0.10 0.12 0.20 1.0)
  ;; 颜色和深度一起擦（掩码，01 课）。
  (gl-clear (bitwise-ior gl-color-buffer-bit gl-depth-buffer-bit))

  ;; 相机固定 (0,0,3) 看向原点（同 08 / 09 课）。
  (define view (m-look-at eye-pos (vec-3 0.0 0.0 0.0) (vec-3 0.0 1.0 0.0)))
  (define proj (m-perspective (degrees->radians 45.0) aspect 0.1 100.0))

  (program-use object-program)
  (gl-bind-vertex-array vao)

  (set-u-model! model)
  (set-u-view! view)
  (set-u-proj! proj)
  ;; 法线矩阵是 mat-3，setter 内部自动选 matrix-3fv 且 transpose 固定 #f（同 09 课）。
  (set-u-normal-matrix! (m-normal-matrix model))
  (set-u-light-pos! light-pos)
  (set-u-light-color! light-color)
  (set-u-view-pos! eye-pos)
  ;; 采样器 uniform 在 init 里报过槽号（0 和 1），它们是"用哪张图"的设置，不随帧变，
  ;; 所以 draw 里不用再传一次（同 04 课）。
  (gl-draw-elements gl-triangles 36 gl-unsigned-short 0)

  (gl-bind-vertex-array 0))

(define-values (frame canvas)
  (make-window #:title "10 光照贴图"
               #:width win-w
               #:height win-h
               #:init init
               #:draw draw
               ;; 窗口尺寸变化、viewport 重设之后被调用，参数是新的客户区宽高。
               #:on-size (lambda (cw ch) (set! aspect (/ cw ch)))))
(send frame show #t)

;; 本课没有 timer：立方体是静姿态，没有东西在动，就不必每 16ms 请求一次重绘（同 08 / 09）。
;; canvas 也用不着 focus——本课不收键盘。

;; ------------------------------------------------------------
;; 试试看（改完重跑）：
;;   · 掩码改成条纹：把 make-spec-mask-texture 的判定换成"隔 8 像素白/黑"——
;;       (define v (if (even? (quotient x 8)) 255 0))
;;     高光立刻变成条纹状：只有白条那一列允许反光，黑条上高光被乘成 0。
;;   · 掩码取反：把圆内外的黑白换过来——
;;       (define v (if (<= d radius) 0 255))
;;     高光就从"一块实心圆斑"变成"挖掉一个圆"：正中央那圈被洗白的颜色没了（实测中心
;;     像素的灰度 243 → 124，全画面再没有一处 ≥230），而圆外那圈残余的高光被放开。
;;     实心斑换成淡环——两项贡献正好对调，位置一动没动。
;;   · diffuse map 换成 04 课的径向渐变图（make-radial-texture 原样抄过来，替换
;;     make-checker-texture 那一行）：整块立方体的颜色随位置从中心到边缘渐暗，
;;     而高光的位置一点不动——两项各管各的，颜色图换不动高光，掩码图也换不动颜色。
;;   · 故意乘错项：把 spec-mask 乘进 ambient 项（spec 项那边照旧）——
;;       (vec-3 lit (+ (* ambient diffuse-color (vec-3 spec-mask))
;;                    (* diff diffuse-color)
;;                    (vec-3 (* spec spec-mask))))
;;     ambient 是三项里唯一"处处都有"的亮度，于是背光的左面和底面也浮出一块圆斑：
;;     圆内按棋盘颜色染了 0.1 的保底光、圆外纯黑，形状一眼可辨——而 09 课里背光面
;;     是整片均匀的暗。同时正面圆外丢了那点保底光、暗色格子更黑。掩码本是"高光开关"，
;;     乘到 ambient 上就变成"保底光的开关"了：贴图乘进哪一项，它管的就是哪一项。
;; ============================================================