#lang glsl-opengl
;; ============================================================
;; 11 · 光从哪来（三）：聚光
;; 运行：racket 11-light-casters/spot.rkt
;; 上一份：11-light-casters/point.rkt
;; ============================================================
;; 点光向四周辐射，四面八方一样亮。手电筒、路灯、舞台追光不是这样：光被罩住，
;; 只朝一个锥里射。本节在点光上加一个锥：
;;   spot   ① 有位置、有衰减（照抄 point）——手电筒还是"一盏灯"
;;          ② 多一个方向：光锥的轴
;;          ③ 多一个锥角：内外两个角，边界用 smoothstep 做软过渡
;; 本文件与 point.rkt 的差别只有三处（其余逐行相同，可以 diff 着读）：
;;   ① uniform：多出 u-light-dir / u-cos-inner / u-cos-outer
;;   ② 片元多算 cos-theta 与 intensity 两行
;;   ③ diffuse / specular 乘的是 intensity × attenuation，而不是单独一个 attenuation
;;
;; 相机这次在动：手电筒挂在相机上（位置 = 相机位置、方向 = 相机朝向），
;; 相机绕着场景慢慢转圈，光锥跟着扫过一排立方体。为此本课第一次在 11 课里用了 timer。

(require racket/class
         racket/gui                      ; timer%
         racket/math                     ; degrees->radians / cos
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
;; 顶点着色器与 point.rkt 逐行相同。
(glsl-shader vert-src
  (version 330 core)
        (layout (location 0) in vec-3 pos)
        (layout (location 1) in vec-3 normal)
        (uniform mat-4 u-model)
        (uniform mat-4 u-view)
        (uniform mat-4 u-proj)
        (uniform mat-3 u-normal-matrix)
        (out vec-3 v-normal)
        (out vec-3 v-frag-pos)
        (define (main) void
          (set! v-normal (* u-normal-matrix normal))
          (set! v-frag-pos (vec-3 (* u-model (vec-4 pos 1.0))))
          (set! gl-position (* u-proj (* u-view (* u-model (vec-4 pos 1.0)))))))

;; 物体的片元：在 point 的"位置 + 衰减"上再加一段锥的判定。
;;
;; cos-theta 是"从灯指向这一点的方向"与"锥轴"的夹角余弦：锥轴上为 1，
;; 越往锥的外沿越小。拿它和两个门限比，就知道这个片元在锥里、锥外、还是边上：
;;   cos-theta ≥ u-cos-inner   在锥心里，intensity = 1
;;   cos-theta ≤ u-cos-outer   在锥外，intensity = 0
;;   两者之间              边上，smoothstep 给一段平滑过渡（不是硬切）
;; 传进来的门限是余弦而不是角度：因为这里比的就是 cos-theta，两边都化成余弦就不用
;; 在片元里做反三角（见页面上 11.3 的 note.key）。
(glsl-shader frag-src
  (version 330 core)
        (in vec-3 v-normal)
        (in vec-3 v-frag-pos)
        (uniform vec-3 u-light-pos)               ; 灯的（也就是相机的）位置
        (uniform vec-3 u-light-dir)               ; 锥轴：相机朝向
        (uniform float u-cos-inner)              ; 内锥角的余弦
        (uniform float u-cos-outer)              ; 外锥角的余弦
        (uniform float u-kc)
        (uniform float u-kl)
        (uniform float u-kq)
        (uniform vec-3 u-light-color)
        (uniform vec-3 u-view-pos)
        (uniform vec-3 u-object-color)
        (out vec-4 frag-color)
        (define (main) void
          (vec-3 norm (normalize v-normal))
          (vec-3 light-dir (normalize (- u-light-pos v-frag-pos)))
          (float distance (length (- u-light-pos v-frag-pos)))
          (float attenuation (/ 1.0 (+ u-kc (* u-kl distance) (* u-kq distance distance))))
          (float cos-theta (dot (normalize (- v-frag-pos u-light-pos)) u-light-dir))
          (float intensity (smoothstep u-cos-outer u-cos-inner cos-theta))
          ;; 两件事一起打折扣：锥外 intensity 是 0，整盏灯就照不到；
          ;; 锥内还要按距离衰减。ambient 照旧不乘（它不是这盏灯给的光）。
          (float spot (* intensity attenuation))
          (float ambient 0.1)
          (float diff (* (max (dot norm light-dir) 0.0) spot))
          (vec-3 reflect-dir (reflect (- light-dir) norm))
          (vec-3 view-dir (normalize (- u-view-pos v-frag-pos)))
          (float spec (* (* (pow (max (dot view-dir reflect-dir) 0.0) 32.0) 0.5) spot))
          (set! frag-color (vec-4 (* (+ ambient diff spec) u-light-color u-object-color) 1.0))))

;; ------------------------------------------------------------
;; 三、立方体几何：与 point.rkt 逐行相同（含 shader-struct 布局）
;; ------------------------------------------------------------
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
;; 四、场景：06 课那片立方体世界 + 一台绕圈的相机
;; ------------------------------------------------------------
(define object-color (vec-3 1.0 0.5 0.31))
(define light-color (vec-3 1.0 1.0 1.0))

;; 06 课那张位置表原样搬来（当时配的是棋盘纹理，这里配光照）。十个立方体散布在
;; 半径 2 - 15 的范围里，相机绕一圈就能看见不同的立方体轮流进出光锥。
;; 这次不给它们自转（06 / 07 课是转的）：本课要看的是"谁被照到"，静止的立方体
;; 更容易对照。
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

;; 相机绕世界原点转圈（同 07 课的 orbit 写法，半径 4、高 1.5，慢了约一倍）：
;;   eye = (4·cos(0.25t), 1.5, 4·sin(0.25t))
;; 一圈约 25 秒。高 1.5 让镜头略高于场景中心，有一点俯视感。
(define orbit-radius 4.0)
(define orbit-height 1.5)
(define orbit-speed 0.25)                      ; 弧度/秒
(define scene-center (vec-3 0.0 0.0 0.0))

;; 内外锥角：内 12.5°、外 17.5°（learnopengl 那套）。两个角相差 5°，
;; 中间那圈就是 smoothstep 的软边。
;;
;; 为什么在 Racket 侧算好余弦再上传：片元里比的是 dot 出来的 cos-theta，
;; 门限跟着化成余弦，片元就不用每像素做一次反三角（见页面上 11.3 的 note.key）。
(define inner-deg 12.5)
(define outer-deg 17.5)
(define cos-inner (cos (degrees->radians inner-deg)))
(define cos-outer (cos (degrees->radians outer-deg)))

;; 衰减三系数与 point.rkt 同：手电筒也是"从一点射出、越远越弱"的灯。
(define k-constant 1.0)
(define k-linear 0.09)
(define k-quadratic 0.032)

(define program 0)
;; uniform 位置：链接后查一次（名字用 GLSL 里的写法），之后每帧直接 uniform-set!
(define u-model-loc 0)
(define u-view-loc 0)
(define u-proj-loc 0)
(define u-normal-matrix-loc 0)
(define u-light-pos-loc 0)
(define u-light-dir-loc 0)
(define u-cos-inner-loc 0)
(define u-cos-outer-loc 0)
(define u-kc-loc 0)
(define u-kl-loc 0)
(define u-kq-loc 0)
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
  (set! u-light-pos-loc (uniform-location program "u_light_pos"))
  (set! u-light-dir-loc (uniform-location program "u_light_dir"))
  (set! u-cos-inner-loc (uniform-location program "u_cos_inner"))
  (set! u-cos-outer-loc (uniform-location program "u_cos_outer"))
  (set! u-kc-loc (uniform-location program "u_kc"))
  (set! u-kl-loc (uniform-location program "u_kl"))
  (set! u-kq-loc (uniform-location program "u_kq"))
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
  (define t (/ (current-inexact-monotonic-milliseconds) 1000.0))

  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear (bitwise-ior gl-color-buffer-bit gl-depth-buffer-bit))

  ;; ---- 相机：绕世界原点转圈（同 07 课）----
  (define eye (vec-3 (* orbit-radius (cos (* t orbit-speed)))
                    orbit-height
                    (* orbit-radius (sin (* t orbit-speed)))))
  (define view (m-look-at eye scene-center (vec-3 0.0 1.0 0.0)))
  ;; 视场这里开大到 70°（其余各课都是 45°）：光锥全角只有 35°，而手电筒又挂在相机上，
  ;; 用 45° 的话锥几乎铺满画面、只剩四个角是黑的；开大之后锥才是一块边界清楚的圆斑。
  (define proj (m-perspective (degrees->radians 70.0) aspect 0.1 100.0))

  ;; 手电筒挂在相机上：光的位置就是相机位置，锥轴就是相机朝向（eye → 场景中心）。
  ;; 相机每帧都在动，所以 u-light-dir 也得每帧重算——不像方向光的 u-light-dir 是个常量，
  ;; 也不像 point 的 u-light-pos 一动不动。相机转，光锥跟着一起转，扫过这排立方体。
  (define light-dir (vnormalize (vsub scene-center eye)))

  (gl-bind-vertex-array vao)
  (program-use program)

  (uniform-set! u-view-loc 'mat-4 view)
  (uniform-set! u-proj-loc 'mat-4 proj)
  (uniform-set! u-light-pos-loc 'vec-3 eye)
  (uniform-set! u-light-dir-loc 'vec-3 light-dir)
  (uniform-set! u-cos-inner-loc 'float cos-inner)
  (uniform-set! u-cos-outer-loc 'float cos-outer)
  (uniform-set! u-kc-loc 'float k-constant)
  (uniform-set! u-kl-loc 'float k-linear)
  (uniform-set! u-kq-loc 'float k-quadratic)
  (uniform-set! u-light-color-loc 'vec-3 light-color)
  ;; u-view-pos 就是相机位置，和 u-light-pos 是同一个点；两项都上传，片元各自取用
  ;; （高光要的 v 和锥判定要的 l 从此处出发，方向刚好相反）。
  (uniform-set! u-view-pos-loc 'vec-3 eye)
  (uniform-set! u-object-color-loc 'vec-3 object-color)

  (for ([pos (in-list cube-positions)])
    (define model (m-translate pos))
    (uniform-set! u-model-loc 'mat-4 model)
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
  (make-window #:title "11 聚光"
               #:width win-w
               #:height win-h
               #:init init
               #:draw draw
               #:on-size (lambda (cw ch) (set! aspect (/ cw ch)))
               #:on-close shutdown))
(send frame show #t)

;; 相机在转，需要定时器不断请求重绘（同 07 课）。定时器只负责"每隔 16ms 请求重绘"，
;; 绘制永远发生在 draw 里。
(define timer
  (new timer% [interval 16]
              [notify-callback (lambda () (send canvas refresh))]))

;; ------------------------------------------------------------
;; 试试看（改完重跑）：
;;   · 把 intensity 那一行换成手写的硬阶跃：
;;       (float intensity (if (> cos-theta u-cos-inner) 1.0 0.0))
;;     光斑的边立刻从软变硬——smoothstep 那 5° 的过渡没了。
;;   · inner-deg / outer-deg 改成 5.0 / 8.0：光斑收窄成一小块，只有画面正中那个
;;     立方体亮。
;;   · 把 inner-deg / outer-deg 改成 60.0 / 90.0：锥开到接近半球，画面跟 point
;;     那节差不多——点光就是"锥开到 180° 的聚光"（见 11.4）。
;;   · orbit-speed 从 0.25 改成 0.8：镜头转快，光锥扫得快，更容易看清它是怎么扫的。
;; ============================================================