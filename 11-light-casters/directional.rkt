#lang glsl-opengl
;; ============================================================
;; 11 · 光从哪来（一）：方向光
;; 运行：racket 11-light-casters/directional.rkt
;; 上一课：10-lighting-maps/lighting-maps.rkt
;; ============================================================
;; 到 10 课为止，灯一直是"世界里的某个点"：uniform 给出它的位置，片元拿位置算方向。
;; 现实里的光不止这一种。11 课把"光从哪来"拆成三种，本节是第一种：
;;   directional（方向光）——太阳这类离得足够远的光源，到地面的光线已经平行
;; 既然所有光线都平行，"灯在哪"就没有意义了：把太阳挪远一点、挪高一点，照到立方体
;; 上的方向几乎不变。所以本课把 u-light-pos 换成 u-light-dir——只留方向。
;;
;; 11 课一节三个文件（directional / point / spot），共用同一套骨架：
;;   · pos + normal 的 shader-struct 顶点布局、法线矩阵、深度测试、相机（同 09 课）
;;   · 四个立方体、一份 VAO、一个 program、不画灯立方体（位置光才有"灯在哪"）
;; 差别只在片元那几行。本文件与 point.rkt 的差别只有三处：
;;   ① uniform：u-light-dir（一个方向）顶掉 point 的 u-light-pos + 三个衰减系数
;;   ② 片元里 light-dir 是常量取反，不再依赖 v-frag-pos——每个片元看到的光方向一样
;;   ③ 没有 attenuation 那项乘法（距离不进公式）
;; 其余逐行相同，和 point.rkt diff 着读，正好看清方向光丢掉了什么。

(require racket/class
         racket/math                     ; degrees->radians
         "../lib/window.rkt"
         "../lib/matrix.rkt")            ; m-look-at / m-perspective / m-normal-matrix / …

;; ------------------------------------------------------------
;; 一、窗口尺寸与 aspect（同 06 / 09 课）
;; ------------------------------------------------------------
(define win-w 800)
(define win-h 600)
(define aspect (/ win-w win-h))

;; ------------------------------------------------------------
;; 二、着色器
;; ------------------------------------------------------------
;; 顶点着色器与 09 课逐行相同：算好裁剪坐标，把世界坐标和世界法线传给片元。
;; （v-frag-pos 本课只有高光用得上——diffuse 那边不再需要"这一点在哪"。）
(glsl-shader vert-src
  (version 330 core)
        (layout (location 0) in vec-3 pos)      ; 属性 0：局部坐标
        (layout (location 1) in vec-3 normal)   ; 属性 1：法线
        (uniform mat-4 u-model)
        (uniform mat-4 u-view)
        (uniform mat-4 u-proj)
        (uniform mat-3 u-normal-matrix)            ; 法线专用的 mat-3（见 09 课 9.2）
        (out vec-3 v-normal)
        (out vec-3 v-frag-pos)
        (define (main) void
          (set! v-normal (* u-normal-matrix normal))
          (set! v-frag-pos (vec-3 (* u-model (vec-4 pos 1.0))))
          (set! gl-position (* u-proj (* u-view (* u-model (vec-4 pos 1.0)))))))

;; 物体的片元：与 09 课的三项相比，只动了 light-dir 这一行，再去掉衰减。
;;
;; u-light-dir 是光"往哪走"（这里是往下、往 -z 偏），而三项公式要的是"从这一点指向
;; 光源"，取反即可。取反之后它就是个常量：不碰 v-frag-pos，更不碰任何距离。
;; 这就是"太阳离得足够远"的数学样子——整个场景共用同一个光方向。
;;
;; （这里用 Racket 风格的连字符；racket-shader 会换成 GLSL 的 _，见 09 课的说明。）
(glsl-shader frag-src
  (version 330 core)
        (in vec-3 v-normal)
        (in vec-3 v-frag-pos)
        (uniform vec-3 u-light-dir)               ; 光的传播方向（不是"指向光"）
        (uniform vec-3 u-light-color)             ; 光的颜色
        (uniform vec-3 u-view-pos)                ; 相机位置——高光要它
        (uniform vec-3 u-object-color)            ; 物体本色
        (out vec-4 frag-color)
        (define (main) void
          (vec-3 norm (normalize v-normal))
          (vec-3 light-dir (normalize (- u-light-dir)))
          (float ambient 0.1)
          (float diff (max (dot norm light-dir) 0.0))
          (vec-3 reflect-dir (reflect (- light-dir) norm))
          (vec-3 view-dir (normalize (- u-view-pos v-frag-pos)))
          (float spec (* (pow (max (dot view-dir reflect-dir) 0.0) 32.0) 0.5))
          ;; 这里没有 attenuation：光走到哪儿都是同一个强度，距离不进公式。
          (set! frag-color (vec-4 (* (+ ambient diff spec) u-light-color u-object-color) 1.0))))

;; ------------------------------------------------------------
;; 三、立方体几何：24 个顶点，每个带位置 + 法线（同 09 课）
;; ------------------------------------------------------------
;; 同一个立方体、同样的 24 顶点 / 36 索引：每个面摊自己的一份顶点，因为每个面的
;; 法线不同（前面 (0,0,1)、右面 (1,0,0)…）。向量布局用 shader-struct 镜像到 CPU 侧：
;; 步长与字段偏移取自字段表（不用数字节），location / 分量数 / 类型在 init 里手写。
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

(define cube-data (apply concat-vecs (map vertex->f32vector cube-verts)))

(define indices
  (u16-array 0 3 2   2 1 0        ; 后面
             4 5 6   6 7 4        ; 前面
             8 9 10  10 11 8      ; 左面
             12 14 13  14 12 15   ; 右面
             16 17 18  18 19 16   ; 底面
             20 22 21  22 20 23)) ; 顶面

;; ------------------------------------------------------------
;; 四、场景：四个立方体 + 一台不动的相机
;; ------------------------------------------------------------
(define object-color (vec-3 1.0 0.5 0.31))      ; 物体本色：橙
(define light-color (vec-3 1.0 1.0 1.0))        ; 光的颜色：白
;; 光传播的方向：往下、往 -z 偏一点，像是下午偏西的太阳。三个数不必自己归一化，
;; 片元里的 normalize 会处理。
(define light-dir (vec-3 -0.2 -1.0 -0.3))

;; 四个立方体排在 z = 0.5 / -1 / -3 / -5：离相机 6.4 / 7.5 / 8.9 / 10.3，
;; 最近的和最远的大约差一倍半。摆这一排就是为了看一件事——四个立方体都只平移、
;; 不旋转，所以"同一个朝向的面"在四处都存在；跑起来会看到它们亮得一模一样
;; （后面验收里量过：四个顶面的像素值完全相同）。
;; 方向光下距离根本不进公式（片元里没有 v-frag-pos 参与 diff），点光那节用的是
;; 同一张位置表、同一台相机，四个立方体却会亮成四个档次——两份文件对着看最清楚。
(define cube-positions
  (list (vec-3 0.0 0.0  0.5)
        (vec-3 0.0 0.0 -1.0)
        (vec-3 0.0 0.0 -3.0)
        (vec-3 0.0 0.0 -5.0)))

;; 相机偏在右上方看向这一排的中段：四个立方体因此错开排进画面，不会互相遮住。
(define eye-pos (vec-3 5.0 3.0 4.0))
(define look-at-pos (vec-3 0.0 0.0 -2.0))

(define program 0)
;; uniform 位置：链接后查一次（名字用 GLSL 里的写法），之后每帧直接 uniform-set!
(define u-model-loc 0)
(define u-view-loc 0)
(define u-proj-loc 0)
(define u-normal-matrix-loc 0)
(define u-light-dir-loc 0)
(define u-light-color-loc 0)
(define u-view-pos-loc 0)
(define u-object-color-loc 0)
(define vao 0)
;; 这些缓冲句柄也放在顶层：关窗时 shutdown 要能拿到（0 = 还没创建）
(define vbo 0)
(define ebo 0)

(define (init)
  (set! program (build-program (gl-vertex-shader vert-src)
                               (gl-fragment-shader frag-src)))
  (set! u-model-loc (uniform-location program "u_model"))
  (set! u-view-loc (uniform-location program "u_view"))
  (set! u-proj-loc (uniform-location program "u_proj"))
  (set! u-normal-matrix-loc (uniform-location program "u_normal_matrix"))
  (set! u-light-dir-loc (uniform-location program "u_light_dir"))
  (set! u-light-color-loc (uniform-location program "u_light_color"))
  (set! u-view-pos-loc (uniform-location program "u_view_pos"))
  (set! u-object-color-loc (uniform-location program "u_object_color"))

  ;; 画立体的东西就得开深度测试（06 课）。
  (gl-enable gl-depth-test)

  ;; ---- VAO / VBO / EBO ----
  (set! vao (vertex-array-new))
  (gl-bind-vertex-array vao)

  (set! vbo (buffer-new))
  (gl-bind-buffer gl-array-buffer vbo)
  (buffer-upload gl-array-buffer cube-data gl-static-draw)
  ;; 顶点属性：手写 location / 分量 / 类型 / 步长 / 偏移（pos 0、normal 12）
  (gl-vertex-attrib-pointer 0 3 gl-float #f (vertex-stride-bytes) (vertex-field-offset 'pos))
  (gl-enable-vertex-attrib-array 0)
  (gl-vertex-attrib-pointer 1 3 gl-float #f (vertex-stride-bytes) (vertex-field-offset 'normal))
  (gl-enable-vertex-attrib-array 1)

  (set! ebo (buffer-new))
  (gl-bind-buffer gl-element-array-buffer ebo)
  (buffer-upload gl-element-array-buffer indices gl-static-draw)

  (gl-bind-vertex-array 0)

)

(define (draw)
  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear (bitwise-ior gl-color-buffer-bit gl-depth-buffer-bit))

  (define view (m-look-at eye-pos look-at-pos (vec-3 0.0 1.0 0.0)))
  (define proj (m-perspective (degrees->radians 45.0) aspect 0.1 100.0))

  (gl-bind-vertex-array vao)
  (program-use program)

  ;; 光参数和两个矩阵整帧只算一份，循环外上传一次；model 每个立方体不同，循环里传。
  (uniform-set! u-view-loc 'mat-4 view)
  (uniform-set! u-proj-loc 'mat-4 proj)
  (uniform-set! u-light-dir-loc 'vec-3 light-dir)
  (uniform-set! u-light-color-loc 'vec-3 light-color)
  (uniform-set! u-view-pos-loc 'vec-3 eye-pos)
  (uniform-set! u-object-color-loc 'vec-3 object-color)

  (for ([pos (in-list cube-positions)])
    ;; 只平移、不旋转：四个立方体的朝向完全一样，"同一个面"才可比较。
    (define model (m-translate pos))
    (uniform-set! u-model-loc 'mat-4 model)
    ;; 只含平移时法线矩阵就是单位阵；照 09 课的规矩照样上传，给某个立方体加旋转时
    ;; 不用改别处（见文件末尾"试试看"）。
    (uniform-set! u-normal-matrix-loc 'mat-3 (m-normal-matrix model))
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
  (program-delete program))

(define-values (frame canvas)
  (make-window #:title "11 方向光"
               #:width win-w
               #:height win-h
               #:init init
               #:draw draw
               ;; 窗口尺寸变化、viewport 重设之后被调用，参数是新的客户区宽高。
               #:on-size (lambda (cw ch) (set! aspect (/ cw ch)))
               #:on-close shutdown))
(send frame show #t)

;; 没有 timer：四个立方体是静姿态，没有东西在动，就不必每 16ms 请求一次重绘。

;; ------------------------------------------------------------
;; 试试看（改完重跑）：
;;   · light-dir 改成 (vec-3 0.0 -1.0 0.0)（正午太阳，光垂直往下）：只有顶面亮，
;;     四个立方体的前面和右侧面一起暗下去只剩 ambient。
;;   · light-dir 改成 (vec-3 -1.0 -0.2 -0.3)（快落山的夕阳）：光几乎平行于地面，
;;     顶面变暗、朝着 -x 的那一面亮起来——同一个立方体换了个面亮。
;;   · 把某个立方体的 model 换成 (mat4-mul (m-translate pos) (m-rotate-y …))：
;;     它自己转起来，于是"它哪一面亮"和另外三个错开——方向光下只有朝向管用。
;; ============================================================