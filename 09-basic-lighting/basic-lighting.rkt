#lang glsl-opengl
;; ============================================================
;; 09 · 基础光照：ambient + diffuse + specular
;; 运行：racket 09-basic-lighting/basic-lighting.rkt
;; 上一课：08-colors/colors.rkt
;; ============================================================
;; 目标：08 课只会"物体色 × 光色"，颜色有了，但整块立方体每一面一样亮——那一步里
;; 没有"方向"：光从哪来、面朝哪，都不影响结果。本课把方向补上，逐片元算 Phong 三项：
;;   ambient   保底光：面完全背光时也有点亮度，否则背光面纯黑、不像真实世界
;;   diffuse   漫反射：面越正对光源越亮，背向为 0（max(dot(n,l), 0)）
;;   specular  镜面高光：反射方向恰好对着眼睛时才亮（pow(max(dot(v,r),0), 32)）
;; 要算这三项，片元得知道两件它现在不知道的事：这一点的法线、这一点在世界里的位置。
;; 于是顶点着色器第一次多传两个量出去（v-normal / v-frag-pos），法线属性（location 1）
;; 和"法线矩阵"也在本课第一次登场。
;;
;; 顶点布局本课也换了写法：用 shader-struct 把 shader 的 struct 镜像到 CPU 侧，
;; 步长和偏移全部由字段名给出——03/04 课的 shader-stride-bytes / shader-byte-size
;; 又往前走了一步。

(require racket/class
         racket/math                     ; degrees->radians
         "../lib/window.rkt"
         "../lib/gpu.rkt"
         "../lib/matrix.rkt")            ; m-look-at / m-perspective / m-normal-matrix / …

;; ------------------------------------------------------------
;; 一、窗口尺寸与 aspect（同 06 / 08 课）
;; ------------------------------------------------------------
;; 投影要一个 aspect = 宽/高，必须和窗口客户区同源，否则画面会被拉伸。
(define win-w 800)
(define win-h 600)
(define aspect (/ win-w win-h))

;; ------------------------------------------------------------
;; 二、着色器
;; ------------------------------------------------------------
;; 顶点着色器除了把位置算好，还要多带两个插值量给片元：
;;   v-frag-pos  这个顶点在世界里的位置（片元里插值后 ≈ 每个像素的世界位置）
;;   v-normal   世界空间的法线（片元里插值后 ≈ 每个像素的法线）
;; 光照明亮程度必须逐片元算，这两个量就是它要的输入。
;;
;; (glsl-shader name form ...) 与 (glsl ...) 一样产出 shader，并额外把编译期接口表
;; 绑为 name-interface。裸等价： (define name (glsl ...))——但那样接口只能运行期用
;; (shader-program-interface name) 取，宏（vertex-layout / uniform-setter）用不了。
(glsl-shader vert-src
  (version 330 core)
  (layout (location 0) in vec-3 pos)       ; 属性名 = CPU 侧 shader-struct 字段名
  (layout (location 1) in vec-3 normal)
  (uniform mat-4 u-model)                   ; 局部 → 世界
  (uniform mat-4 u-view)                    ; 世界 → 观察
  (uniform mat-4 u-proj)                    ; 观察 → 裁剪
  (uniform mat-3 u-normal-matrix)            ; 法线专用的 mat-3，为什么不是 u-model 见 9.2
  (out vec-3 v-normal)
  (out vec-3 v-frag-pos)
  (define (main) void
    (set! v-normal (* u-normal-matrix normal))
    (set! v-frag-pos (vec-3 (* u-model (vec-4 pos 1.0))))
    (set! gl-position (* u-proj (* u-view (* u-model (vec-4 pos 1.0)))))))

;; 物体的片元：本课的全部内容。三项相加，最后乘光色和物色——08 课那一步还在，
;; 只是现在"光有多少"不再是个常数，而是逐像素算出来的 ambient + diff + spec。
;;
;; light-dir（l）从片元指向光源；view-dir（v）从片元指向相机；reflect-dir（r）
;; 是 l 打到表面上弹开的方向，也就是"光被镜面反射到哪去了"。
;; 眼睛正好站在 r 上（v 与 r 重合）时就看见高光。
;; （这里用 Racket 风格的连字符；racket-shader 会换成 GLSL 的 _，生成文本里是 light_dir / v_normal。）
(glsl-shader frag-src
  (version 330 core)
  (in vec-3 v-normal)
  (in vec-3 v-frag-pos)
  (uniform vec-3 u-light-pos)               ; 光源位置（世界坐标）
  (uniform vec-3 u-light-color)             ; 光的颜色（同 08 课）
  (uniform vec-3 u-view-pos)                ; 相机位置（世界坐标）——高光要它
  (uniform vec-3 u-object-color)            ; 物体本色（同 08 课）
  (out vec-4 frag-color)
  (define (main) void
    (vec-3 norm (normalize v-normal))
    (vec-3 light-dir (normalize (- u-light-pos v-frag-pos)))
    (float ambient 0.1)
    (float diff (max (dot norm light-dir) 0.0))
    (vec-3 reflect-dir (reflect (- light-dir) norm))
    (vec-3 view-dir (normalize (- u-view-pos v-frag-pos)))
    (float spec (* (pow (max (dot view-dir reflect-dir) 0.0) 32.0) 0.5))
    (set! frag-color (vec-4 (* (+ ambient diff spec) u-light-color u-object-color) 1.0))))

;; 灯自己的两个 shader（同 08 课思路）：顶点只要位置，片元只把光色原样输出。
;; 灯是光本身，不是"被光照到的东西"，所以它不算法线、不做那三项。
;; 注意它只声明 location 0——下面那个 VAO 多开的 normal 槽（location 1）在没有
;; 对应 in 的 program 里会被 GL 忽略，灯照画不误（08 课末尾预告过这件事）。
(glsl-shader lamp-vert-src
  (version 330 core)
  (layout (location 0) in vec-3 pos)
  (uniform mat-4 u-model)
  (uniform mat-4 u-view)
  (uniform mat-4 u-proj)
  (define (main) void
    (set! gl-position (* u-proj (* u-view (* u-model (vec-4 pos 1.0)))))))

(glsl-shader lamp-frag-src
  (version 330 core)
  (uniform vec-3 u-light-color)
  (out vec-4 frag-color)
  (define (main) void
    (set! frag-color (vec-4 u-light-color 1.0))))

;; ------------------------------------------------------------
;; 三、立方体几何：24 个顶点，每个带位置 + 法线
;; ------------------------------------------------------------
;; 和 06 / 08 课是同一个立方体、同样的 24 顶点 / 36 索引（06 课解释过为什么不是 8 个：
;; 每个面得摊自己的一份顶点）。本课一个顶点多带一条法线：法线是"这个面朝哪边"，
;; 三项光照全靠它。同一个空间角落在不同的面上法线不同（前面 (0,0,1)、右面 (1,0,0)…），
;; 所以还是 24 个顶点、每个面 4 个。
;;
;; 布局不再手写：shader-struct 把 shader 的 struct 镜像到 CPU 侧——字段名、类型、顺序
;; 和 shader 里一致，步长和偏移就自动有了。03/04 课的 shader-stride-bytes /
;; shader-byte-size 已经消灭了字节字面量，这一步再进一步：偏移不再按"第几个字段"数，
;; 而是按名字自动对齐（见下面 init 里的 glsl-vertex-layout!）。
(shader-struct vertex (vec-3 pos) (vec-3 normal))

(define cube-verts
  (list
   ;; 后面 z = -0.5，法线朝 -z
   (vertex (vec-3 -0.5 -0.5 -0.5) (vec-3 0.0 0.0 -1.0))
   (vertex (vec-3  0.5 -0.5 -0.5) (vec-3 0.0 0.0 -1.0))
   (vertex (vec-3  0.5  0.5 -0.5) (vec-3 0.0 0.0 -1.0))
   (vertex (vec-3 -0.5  0.5 -0.5) (vec-3 0.0 0.0 -1.0))
   ;; 前面 z = +0.5，法线朝 +z
   (vertex (vec-3 -0.5 -0.5  0.5) (vec-3 0.0 0.0 1.0))
   (vertex (vec-3  0.5 -0.5  0.5) (vec-3 0.0 0.0 1.0))
   (vertex (vec-3  0.5  0.5  0.5) (vec-3 0.0 0.0 1.0))
   (vertex (vec-3 -0.5  0.5  0.5) (vec-3 0.0 0.0 1.0))
   ;; 左面 x = -0.5，法线朝 -x
   (vertex (vec-3 -0.5  0.5  0.5) (vec-3 -1.0 0.0 0.0))
   (vertex (vec-3 -0.5  0.5 -0.5) (vec-3 -1.0 0.0 0.0))
   (vertex (vec-3 -0.5 -0.5 -0.5) (vec-3 -1.0 0.0 0.0))
   (vertex (vec-3 -0.5 -0.5  0.5) (vec-3 -1.0 0.0 0.0))
   ;; 右面 x = +0.5，法线朝 +x
   (vertex (vec-3  0.5  0.5  0.5) (vec-3 1.0 0.0 0.0))
   (vertex (vec-3  0.5  0.5 -0.5) (vec-3 1.0 0.0 0.0))
   (vertex (vec-3  0.5 -0.5 -0.5) (vec-3 1.0 0.0 0.0))
   (vertex (vec-3  0.5 -0.5  0.5) (vec-3 1.0 0.0 0.0))
   ;; 底面 y = -0.5，法线朝 -y
   (vertex (vec-3 -0.5 -0.5 -0.5) (vec-3 0.0 -1.0 0.0))
   (vertex (vec-3  0.5 -0.5 -0.5) (vec-3 0.0 -1.0 0.0))
   (vertex (vec-3  0.5 -0.5  0.5) (vec-3 0.0 -1.0 0.0))
   (vertex (vec-3 -0.5 -0.5  0.5) (vec-3 0.0 -1.0 0.0))
   ;; 顶面 y = +0.5，法线朝 +y
   (vertex (vec-3 -0.5  0.5 -0.5) (vec-3 0.0 1.0 0.0))
   (vertex (vec-3  0.5  0.5 -0.5) (vec-3 0.0 1.0 0.0))
   (vertex (vec-3  0.5  0.5  0.5) (vec-3 0.0 1.0 0.0))
   (vertex (vec-3 -0.5  0.5  0.5) (vec-3 0.0 1.0 0.0))))

;; 每个顶点铺平成一个 f32vector（6 个 float = 3 位置 + 3 法线），24 个接起来就是
;; 整条 VBO。一个字节字面量都没有：交错格式由 shader-struct 的字段表决定。
(define cube-data (apply concat-vecs (map vertex->f32vector cube-verts)))

;; 每个面两个三角形、6 个下标，六个面合计 36（同 06 / 08 课）。
(define indices
  (u16-array 0 3 2   2 1 0        ; 后面
             4 5 6   6 7 4        ; 前面
             8 9 10  10 11 8      ; 左面
             12 14 13  14 12 15   ; 右面
             16 17 18  18 19 16   ; 底面
             20 22 21  22 20 23)) ; 顶面

;; ------------------------------------------------------------
;; 四、场景：一个物体 + 一盏灯 + 一台不动的相机
;; ------------------------------------------------------------
(define object-color (vec-3 1.0 0.5 0.31))      ; 物体本色：橙（同 08 课）
(define light-color (vec-3 1.0 1.0 1.0))        ; 光的颜色：白
(define light-pos (vec-3 1.2 1.0 2.0))          ; 光在世界里的位置：物体右上前方
(define eye-pos (vec-3 0.0 0.0 3.0))            ; 相机位置：本课不动

;; model 是个固定的倾斜姿态：先绕 x 轴 -12°，再绕 y 轴 15°（从右往左作用）。
;; 为什么不干脆摆成单位矩阵：相机和立方体中心一样高，轴对齐时会看到前 / 上 / 右
;; 三个面，而这三个面恰好都朝着光——"背光面只剩环境光"就看不见了。稍微转一下：
;;   正面    法线 ≈ (0.25, 0.21, 0.95)，正好落在"光与眼睛的中间方向"上 → 中央一块高光
;;   左面    法线转成 (-0.97, 0, 0.26)，背对光 → 只剩 ambient，但还朝着镜头
;;   底面    也翻到朝着镜头的一侧、同样背对光 → 也只剩 ambient
;; 于是 ambient / diffuse / specular 三项在同一帧里都看得见。
(define model
  (mat4-mul (m-rotate-y (degrees->radians 15.0))
            (m-rotate-x (degrees->radians -12.0))))

(define object-program #f)
(define lamp-program #f)
(define vao #f)

;; 下面用宏写法；裸写法（每个属性一组 gl-vertex-attrib-pointer、每个 uniform 手查位置）
;; 见教程 9.4。宏只负责生成这些裸调用，不改变 OpenGL 语义。
;;
;; uniform 位置不用手查了：glsl-uniform-setter! 从 shader 的编译期接口表按名字生成
;; setter（类型也自动推）。两个 program 各加前缀，避免同名 uniform 撞。
;; 裸写法： (define obj-u-model (uniform-location object-program "u_model"))
;;          (uniform-set! obj-u-model 'mat-4 model)        ; draw 里
;; glsl-uniform-setter! 把这两步合成 (set-obj-u-model! model)；拼错名字编译期报错。
(glsl-uniform-setter! object-program vert-src frag-src #:prefix "obj-")
(glsl-uniform-setter! lamp-program lamp-vert-src lamp-frag-src #:prefix "lamp-")

(define (init)
  (set! object-program (own! 'program (build-program (gl-vertex-shader vert-src)
                                                     (gl-fragment-shader frag-src))))
  (set! lamp-program (own! 'program (build-program (gl-vertex-shader lamp-vert-src)
                                                   (gl-fragment-shader lamp-frag-src))))

  ;; 画立体的东西就得开深度测试（06 课）。
  (gl-enable gl-depth-test)

  ;; ---- VAO / VBO / EBO ----
  (set! vao (own! 'vertex-array (vertex-array-new)))
  (gl-bind-vertex-array vao)

  (define vbo (own! 'buffer (buffer-new)))
  (gl-bind-buffer gl-array-buffer vbo)
  (buffer-upload gl-array-buffer cube-data gl-static-draw)
  ;; 属性布局：shader 的 in 名和 shader-struct 字段名同名（pos / normal），
  ;; glsl-vertex-layout! 按名字自动对齐：slot 取自 shader 的 location，
  ;; 分量/类型/步长/偏移取自 struct 字段表；改名/类型不符会在编译期报错。
  ;; 裸写法：
  ;;   (gl-vertex-attrib-pointer 0 3 gl-float #f (vertex-stride) (vertex-field-offset 'pos))
  ;;   (gl-enable-vertex-attrib-array 0)
  ;;   (gl-vertex-attrib-pointer 1 3 gl-float #f (vertex-stride) (vertex-field-offset 'normal))
  ;;   (gl-enable-vertex-attrib-array 1)
  (glsl-vertex-layout! vert-src vertex)

  (define ebo (own! 'buffer (buffer-new)))
  (gl-bind-buffer gl-element-array-buffer ebo)
  (buffer-upload gl-element-array-buffer indices gl-static-draw)

  (gl-bind-vertex-array 0))

(define (draw)
  (gl-clear-color 0.10 0.12 0.20 1.0)
  ;; 颜色和深度一起擦（掩码，01 课）。
  (gl-clear (bitwise-ior gl-color-buffer-bit gl-depth-buffer-bit))

  ;; 相机固定 (0,0,3) 看向原点（同 08 课）。u-view-pos 就是它的位置——片元算高光要用。
  (define view (m-look-at eye-pos (vec-3 0.0 0.0 0.0) (vec-3 0.0 1.0 0.0)))
  (define proj (m-perspective (degrees->radians 45.0) aspect 0.1 100.0))

  (gl-bind-vertex-array vao)

  ;; ---- 物体：逐片元算 ambient + diffuse + specular ----
  (program-use object-program)
  ;; setter 的名字和类型都取自 shader 接口；不用再手查 uniform location。
  (set-obj-u-model! model)
  (set-obj-u-view! view)
  (set-obj-u-proj! proj)
  ;; 法线矩阵是 mat-3，setter 内部自动选 matrix-3fv 且 transpose 固定 #f：
  ;; lib/matrix.rkt 的矩阵都是列主序，正好是 GL 要的，传 #t 会让它转错。
  (set-obj-u-normal-matrix! (m-normal-matrix model))
  (set-obj-u-light-pos! light-pos)
  (set-obj-u-light-color! light-color)
  (set-obj-u-view-pos! eye-pos)
  (set-obj-u-object-color! object-color)
  (gl-draw-elements gl-triangles 36 gl-unsigned-short 0)

  ;; ---- 灯：同一份 VAO、换个 program，片元只输出光色（同 08 课） ----
  (program-use lamp-program)
  ;; 灯比物体小：先缩到 0.2 倍，再搬到 light-pos（从右往左作用在顶点上）。
  (set-lamp-u-model! (mat4-mul (m-translate light-pos) (m-scale (vec-3 0.2 0.2 0.2))))
  (set-lamp-u-view! view)
  (set-lamp-u-proj! proj)
  (set-lamp-u-light-color! light-color)
  (gl-draw-elements gl-triangles 36 gl-unsigned-short 0)

  (gl-bind-vertex-array 0))

(define-values (frame canvas)
  (make-window #:title "09 基础光照"
               #:width win-w
               #:height win-h
               #:init init
               #:draw draw
               ;; 窗口尺寸变化、viewport 重设之后被调用，参数是新的客户区宽高。
               #:on-size (lambda (cw ch) (set! aspect (/ cw ch)))
               #:on-close release-all!))
(send frame show #t)

;; 本课没有 timer：立方体是静姿态，没有东西在动，就不必每 16ms 请求一次重绘（同 08 课）。
;; canvas 也用不着 focus——本课不收键盘。

;; ------------------------------------------------------------
;; 试试看（改完重跑）：
;;   · ambient 从 0.1 改成 0.5：背光那两面立刻被抬亮，整块立方体的明暗对比变平。
;;   · shininess 从 32.0 改成 4.0：高光变大一整片、边缘发糊——指数越小越发散。
;;   · light-pos 改成 (vec-3 0.0 3.0 0.0)（正上方）：能看见的三个面里没有一个正对光，
;;     diff 几乎全变成 0，整块立方体暗下来只剩 ambient（正对光的顶面这会儿正好背向镜头，
;;     看不见）。把 light-pos 再改成 (vec-3 -2.0 -0.5 2.0)（左下前方）就反过来：现在
;;     亮的那面变暗、现在暗的左下面变亮，高光也跟着挪到那两面上。
;;   · 给 model 加一个非等比缩放，例如
;;       (define scaled (mat4-mul (m-scale (vec-3 1.5 1.0 1.0)) model))
;;     现在上传的还是 (m-normal-matrix scaled)，法线仍垂直于表面，画面正常。
;;     再把这一行换成"只取左上 3x3、不逆转置"的 naive 版本——照 lib/matrix.rkt
;;     里 m-normal-matrix 的写法，把 mat3-inverse / mat3-transpose 两层去掉：
;;       (mat-3 (mat4-ref scaled 0 0) (mat4-ref scaled 0 1) (mat4-ref scaled 0 2)
;;             (mat4-ref scaled 1 0) (mat4-ref scaled 1 1) (mat4-ref scaled 1 2)
;;             (mat4-ref scaled 2 0) (mat4-ref scaled 2 1) (mat4-ref scaled 2 2))
;;     法线被 1.5 倍放大带偏，正面的高光立刻暗下去、位置也挪了（高光对法线方向
;;     的误差最敏感）；把缩放比 1.5 换成 4.0，连背光面的明暗都会跟着变。
;; ============================================================