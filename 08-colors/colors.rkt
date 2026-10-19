#lang glsl-opengl
;; ============================================================
;; 08 · 颜色
;; 运行：racket 08-colors/colors.rkt
;; 上一课：07-camera/camera.rkt
;; ============================================================
;; 目标：光照章的第一课，但还没有方向、没有衰减——先只回答"颜色从哪来"。
;; 一件东西看起来是什么颜色，取决于两件事：它自己是什么颜色、照它的光是什么颜色。
;; 本课全部的"物理"就是一行乘法（逐分量）：
;;     看到的颜色 = 物体色 × 光色
;; 物体色说的是"这份光里我能反射回去多少"：橙 (1.0, 0.5, 0.31) 的意思是
;; 红全反射、绿反一半、蓝几乎不反射。白光 (1,1,1) 乘上去等于没乘，所以白光下
;; 它显橙；换成绿光 (0,1,0)，三个分量里活下来的只有绿那一份，物体立刻发黑。
;; 场景里两个立方体：被照的物体（橙），和灯自己的替身（白、小方块）。
;; 它们用两个 program 画——物体的片元做乘法，灯的片元只输出光色，见下。

(require racket/class
         racket/math                     ; degrees->radians
         "../lib/window.rkt"
         "../lib/gpu.rkt"
         "../lib/matrix.rkt")            ; m-look-at / m-perspective / m-translate / m-scale

;; ------------------------------------------------------------
;; 一、窗口尺寸与 aspect（同 06 / 07 课）
;; ------------------------------------------------------------
(define win-w 800)
(define win-h 600)
;; 投影要一个 aspect = 宽/高，必须和窗口客户区同源，否则画面会被拉伸。
(define aspect (/ win-w win-h))

;; ------------------------------------------------------------
;; 二、着色器
;; ------------------------------------------------------------
;; 顶点着色器两个 program 共用同一份源码：都只声明 location 0 的位置属性，
;; 都只要 model / view / proj 三个矩阵。两个 program 的差别全在片元那一段。
(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec-3 a-pos)     ; 属性 0：局部坐标
        (uniform mat-4 u-model)                  ; 局部 → 世界
        (uniform mat-4 u-view)                   ; 世界 → 观察
        (uniform mat-4 u-proj)                   ; 观察 → 裁剪
        (define (main) void
          (set! gl-position (* u-proj (* u-view (* u-model (vec-4 a-pos 1.0))))))))

;; 物体的片元：本课全部的"物理"就是这一行逐分量乘法。
;; u-object-color 不是颜料，而是反射率：光里的每个分量各被反射回去多少。
;; 白光 (1,1,1) 照到橙 (1.0, 0.5, 0.31)，乘出来还是 (1.0, 0.5, 0.31)——它显橙。
(define object-frag-src
  (glsl (version 330 core)
        (uniform vec-3 u-object-color)            ; 物体本色：它反射哪些分量、各反射多少
        (uniform vec-3 u-light-color)             ; 光的颜色：照过来时各分量各有多少
        (out vec-4 frag-color)
        (define (main) void
          (set! frag-color (vec-4 (* u-object-color u-light-color) 1.0)))))

;; 灯的片元：只把光色原样输出，不做乘法。
;; 灯是光本身，不是"被光照到的物体"。要是也乘一遍，白光乘白灯还算凑合，
;; 换成绿光乘白灯，灯自己就被调暗了——那等于说"灯也被谁照着"，是错的。
(define lamp-frag-src
  (glsl (version 330 core)
        (uniform vec-3 u-light-color)
        (out vec-4 frag-color)
        (define (main) void
          (set! frag-color (vec-4 u-light-color 1.0)))))

;; ------------------------------------------------------------
;; 三、立方体数据：24 个顶点 + 36 个索引
;; ------------------------------------------------------------
;; 和 06 / 07 课是同一个立方体、同样的 24 顶点 / 36 索引（06 课解释过为什么不是
;; 8 个：每个面要摊自己的一份顶点）。区别是本课只要位置：一个顶点 12 个字节，
;; 没有 UV，也没有法向——法向要到 09 课才回来。
(define vertex-data
  (concat-vecs
   ;; 后面 z = -0.5
   (vec-3 -0.5 -0.5 -0.5)
   (vec-3  0.5 -0.5 -0.5)
   (vec-3  0.5  0.5 -0.5)
   (vec-3 -0.5  0.5 -0.5)
   ;; 前面 z = +0.5
   (vec-3 -0.5 -0.5  0.5)
   (vec-3  0.5 -0.5  0.5)
   (vec-3  0.5  0.5  0.5)
   (vec-3 -0.5  0.5  0.5)
   ;; 左面 x = -0.5
   (vec-3 -0.5  0.5  0.5)
   (vec-3 -0.5  0.5 -0.5)
   (vec-3 -0.5 -0.5 -0.5)
   (vec-3 -0.5 -0.5  0.5)
   ;; 右面 x = +0.5
   (vec-3  0.5  0.5  0.5)
   (vec-3  0.5  0.5 -0.5)
   (vec-3  0.5 -0.5 -0.5)
   (vec-3  0.5 -0.5  0.5)
   ;; 底面 y = -0.5
   (vec-3 -0.5 -0.5 -0.5)
   (vec-3  0.5 -0.5 -0.5)
   (vec-3  0.5 -0.5  0.5)
   (vec-3 -0.5 -0.5  0.5)
   ;; 顶面 y = +0.5
   (vec-3 -0.5  0.5 -0.5)
   (vec-3  0.5  0.5 -0.5)
   (vec-3  0.5  0.5  0.5)
   (vec-3 -0.5  0.5  0.5)))

;; 每个面两个三角形、6 个下标，六个面合计 36（同 06 课）。
(define indices
  (u16-array 0 3 2   2 1 0        ; 后面
             4 5 6   6 7 4        ; 前面
             8 9 10  10 11 8      ; 左面
             12 14 13  14 12 15   ; 右面
             16 17 18  18 19 16   ; 底面
             20 22 21  22 20 23)) ; 顶面

;; ------------------------------------------------------------
;; 四、场景：一个物体 + 一盏灯
;; ------------------------------------------------------------
;; 物体摆在世界原点；灯摆在它的右上方前方一点，用 0.2 倍的模型缩成小方块。
;; 相机固定站在 (0,0,3) 看向原点——本课镜头不动，重点全在颜色。
;; 灯的 z 取 0（和物体差不多深）是为了构图：灯离相机越近，越容易被画面边缘挤出去；
;; 现在它稳稳落在右上角，而物体正面在 z = +0.5、离相机更近，看着更大。
(define object-pos (vec-3 0.0 0.0 0.0))
(define lamp-pos (vec-3 1.2 1.0 0.0))
(define object-color (vec-3 1.0 0.5 0.31))      ; 物体的本色：橙
(define light-color (vec-3 1.0 1.0 1.0))        ; 光的颜色：白

(define object-program #f)
(define lamp-program #f)
(define vao #f)
;; uniform 位置是"每个 program 各有一份"的：两个 program 里都叫 u-model 的 uniform
;; 在 GPU 上是两个槽位，位置值不保证相同，所以两套各存一套、不混用。
(define obj-u-model #f)
(define obj-u-view #f)
(define obj-u-proj #f)
(define obj-u-object-color #f)
(define obj-u-light-color #f)
(define lamp-u-model #f)
(define lamp-u-view #f)
(define lamp-u-proj #f)
(define lamp-u-light-color #f)

(define (init)
  ;; 同一份顶点源码，配两份不同的片元，链接出两个 program。
  (set! object-program (own! 'program (build-program (gl-vertex-shader vert-src)
                                                     (gl-fragment-shader object-frag-src))))
  (set! lamp-program (own! 'program (build-program (gl-vertex-shader vert-src)
                                                   (gl-fragment-shader lamp-frag-src))))

  ;; 画立体的东西就得开深度测试，否则后画的片元盖住先画的、与远近无关（06 课）。
  (gl-enable gl-depth-test)

  ;; ---- VAO / VBO / EBO ----
  ;; VAO 里只开 0 号槽：位置。两个 program 的顶点着色器都只声明 layout 0，
  ;; 所以这一个 VAO 喂给谁都对得上。
  (set! vao (own! 'vertex-array (vertex-array-new)))
  (gl-bind-vertex-array vao)

  (define vbo (own! 'buffer (buffer-new)))
  (gl-bind-buffer gl-array-buffer vbo)
  (buffer-upload gl-array-buffer vertex-data gl-static-draw)
  ;; 只有一个属性：步长就是 vec-3 的大小、偏移 0，都从布局算出来，不写字面量。
  (gl-vertex-attrib-pointer 0 3 gl-float #f (shader-byte-size 'vec-3) 0)
  (gl-enable-vertex-attrib-array 0)

  (define ebo (own! 'buffer (buffer-new)))
  (gl-bind-buffer gl-element-array-buffer ebo)
  (buffer-upload gl-element-array-buffer indices gl-static-draw)

  (gl-bind-vertex-array 0)

  ;; 链接完把两个 program 的 uniform 位置各查一次存起来，draw 里直接用。
  (set! obj-u-model (uniform-location object-program "u_model"))
  (set! obj-u-view (uniform-location object-program "u_view"))
  (set! obj-u-proj (uniform-location object-program "u_proj"))
  (set! obj-u-object-color (uniform-location object-program "u_object_color"))
  (set! obj-u-light-color (uniform-location object-program "u_light_color"))
  (set! lamp-u-model (uniform-location lamp-program "u_model"))
  (set! lamp-u-view (uniform-location lamp-program "u_view"))
  (set! lamp-u-proj (uniform-location lamp-program "u_proj"))
  (set! lamp-u-light-color (uniform-location lamp-program "u_light_color")))

(define (draw)
  (gl-clear-color 0.10 0.12 0.20 1.0)
  ;; 颜色和深度一起擦（掩码，01 课）：只擦颜色的话，上一帧的深度还留着，
  ;; 这一帧的遮挡判断就全错了（06 课）。
  (gl-clear (bitwise-ior gl-color-buffer-bit gl-depth-buffer-bit))

  ;; 相机固定站在 (0,0,3) 看向原点，本课不移动它。proj 每帧重算，于是窗口
  ;; 被拖动时 aspect 一改，下一帧的投影就用上新值（#:on-size，见文件末尾）。
  (define view (m-look-at (vec-3 0.0 0.0 3.0) (vec-3 0.0 0.0 0.0) (vec-3 0.0 1.0 0.0)))
  (define proj (m-perspective (degrees->radians 45.0) aspect 0.1 100.0))

  ;; 同一个 VAO 全程 bind 着，中间只换 program。
  (gl-bind-vertex-array vao)

  ;; ---- 物体：片元做 物体色 × 光色 ----
  (program-use object-program)
  (uniform-set! obj-u-model 'mat-4 (m-translate object-pos))
  (uniform-set! obj-u-view 'mat-4 view)
  (uniform-set! obj-u-proj 'mat-4 proj)
  (uniform-set! obj-u-object-color 'vec-3 object-color)
  (uniform-set! obj-u-light-color 'vec-3 light-color)
  (gl-draw-elements gl-triangles 36 gl-unsigned-short 0)

  ;; ---- 灯：同一个 VAO、换个 program，片元只输出光色 ----
  ;; VAO 只记"怎么从缓冲里读出一个顶点"，不记"读给哪个 program 用"，所以换了
  ;; program 连 bind 都不用重来。09 课的 VAO 会多开一个 normal 槽（location 1），
  ;; 而灯这个 program 依旧只声明 location 0——多开的属性槽在没有对应 in 的
  ;; program 里被 GL 忽略，不会报错，灯照画不误。
  (program-use lamp-program)
  ;; 灯比物体小：先缩到 0.2 倍，再搬到 lamp-pos（从右往左作用在顶点上）。
  (uniform-set! lamp-u-model 'mat-4 (mat4-mul (m-translate lamp-pos) (m-scale (vec-3 0.2 0.2 0.2))))
  (uniform-set! lamp-u-view 'mat-4 view)
  (uniform-set! lamp-u-proj 'mat-4 proj)
  (uniform-set! lamp-u-light-color 'vec-3 light-color)
  (gl-draw-elements gl-triangles 36 gl-unsigned-short 0)

  (gl-bind-vertex-array 0))

(define-values (frame canvas)
  (make-window #:title "08 颜色"
               #:width win-w
               #:height win-h
               #:init init
               #:draw draw
               ;; 窗口尺寸变化、viewport 重设之后被调用，参数是新的客户区宽高。
               #:on-size (lambda (cw ch) (set! aspect (/ cw ch)))
               #:on-close release-all!))
(send frame show #t)

;; 本课没有 timer：场景是静的、没有东西在动，就不必每 16ms 请求一次重绘（接 01 课）。
;; 窗口被遮住再露出来时，系统会让画布重绘，draw 自然会再跑一遍。
;; canvas 也用不着 focus——本课不收键盘。

;; ------------------------------------------------------------
;; 试试看（改完重跑）：
;;   · light-color 改成 (vec-3 0.0 1.0 0.0)（纯绿光）：橙物体只剩绿分量
;;     (0.0 0.5 0.0)，整个立方体明显发暗发黑——和白光下那张图对上了（8.1）。
;;   · object-color 改成 (vec-3 1.0 1.0 1.0)：三个分量全反射，物体变成
;;     "光是什么色它就是什么色"，白光下就是白的。
;;   · 把灯的片元也改成做乘法（给它加一个 u-object-color、上传个偏暗的值，
;;     比如 (vec-3 0.5 0.5 0.5)）：灯自己跟着变暗——这就是灯必须另起一个
;;     program 的原因，它是光，不是被光照到的东西。
;;   · lamp-pos 改成 (vec-3 1.2 1.0 -2.0)（挪到物体背后）：画面里物体的颜色
;;     一点没变。本课的光没有方向、没有距离，物体色乘光色与灯在哪无关；
;;     方向要等 09 课（法向与点乘），衰减要等 11 课。
;; ============================================================