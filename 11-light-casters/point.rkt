#lang glsl-opengl
;; ============================================================
;; 11 · 光从哪来（二）：点光与衰减
;; 运行：racket 11-light-casters/point.rkt
;; 上一份：11-light-casters/directional.rkt
;; ============================================================
;; 方向光（directional.rkt）只留了方向，因为它假设光源远到光线已经平行。
;; 灯泡不是这样的：它就挂在房间里，光从这一点向四周辐射，越远越弱。
;; 本节把"位置"和"衰减"两件东西加回来：
;;   point   ① 有位置——u-light-pos 回来了，光方向逐片元算
;;           ② 有衰减——能量摊在半径 d 的球面上，球面积随 d² 涨，于是
;;                        attenuation = 1 / (kc + kl·d + kq·d²)
;; 本文件与 directional.rkt 的差别只有三处（其余逐行相同，可以 diff 着读）：
;;   ① uniform：u-light-pos + u-kc / u-kl / u-kq 顶掉 directional 的 u-light-dir
;;   ② 片元多算 distance 与 attenuation 两行
;;   ③ diffuse 与 specular 各乘一次 attenuation（ambient 不乘，见片元里的注释）
;; 场景也换了：四个立方体从"离相机 0/2/4/6"改成"离灯 1.5/3/5/7"——
;; 同样的四个立方体，这次会因为距离亮成四个档次。

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
;; 顶点着色器与 directional.rkt 逐行相同。
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

;; 物体的片元：在 directional 的三项上加了 distance 与 attenuation。
;;
;; light-dir 这次要逐片元算——"从这一点指向灯"的方向，每个像素都不一样，
;; 这正是方向光省掉的那件事（它整帧共用一个方向）。
(glsl-shader frag-src
  (version 330 core)
        (in vec-3 v-normal)
        (in vec-3 v-frag-pos)
        (uniform vec-3 u-light-pos)               ; 灯在世界里的位置
        (uniform float u-kc)                    ; 衰减的常数项
        (uniform float u-kl)                    ; 衰减的线性项
        (uniform float u-kq)                    ; 衰减的二次项
        (uniform vec-3 u-light-color)
        (uniform vec-3 u-view-pos)
        (uniform vec-3 u-object-color)
        (out vec-4 frag-color)
        (define (main) void
          (vec-3 norm (normalize v-normal))
          (vec-3 light-dir (normalize (- u-light-pos v-frag-pos)))
          (float distance (length (- u-light-pos v-frag-pos)))
          ;; 衰减：分母里的三项各管一段距离（见页面上那张曲线与参数表）。
          ;; d = 0 时分母退化成 kc，所以 kc 是"贴着灯时的保底"——它不该是 0。
          (float attenuation (/ 1.0 (+ u-kc (* u-kl distance) (* u-kq distance distance))))
          (float ambient 0.1)
          ;; diffuse 和 specular 都是"这盏灯送来的光"，所以两处都乘 attenuation；
          ;; ambient 不乘：环境光是场景里来回弹的那份光，不是这盏灯给的，
          ;; 乘上去会让远处的背光面越来越黑，与事实不符。
          (float diff (* (max (dot norm light-dir) 0.0) attenuation))
          (vec-3 reflect-dir (reflect (- light-dir) norm))
          (vec-3 view-dir (normalize (- u-view-pos v-frag-pos)))
          (float spec (* (* (pow (max (dot view-dir reflect-dir) 0.0) 32.0) 0.5) attenuation))
          (set! frag-color (vec-4 (* (+ ambient diff spec) u-light-color u-object-color) 1.0))))

;; ------------------------------------------------------------
;; 三、立方体几何：与 directional.rkt 逐行相同（含 shader-struct 布局）
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
;; 四、场景：一盏灯 + 四个立方体 + 一台不动的相机
;; ------------------------------------------------------------
(define object-color (vec-3 1.0 0.5 0.31))      ; 物体本色：橙
(define light-color (vec-3 1.0 1.0 1.0))        ; 光的颜色：白
;; 灯在 z = 2 的轴上，不画它的替身（本课看的是"亮度随距离怎么变"，不是"灯在哪"）。
(define light-pos (vec-3 0.0 0.0 2.0))

;; 衰减系数取 learnopengl 那一套经典值：kc = 1.0 / kl = 0.09 / kq = 0.032，
;; 大致覆盖 50 个单位以内的距离（参数表见页面上 11.2 节）。
;;   kc 常数项：d = 0 时分母只剩它，避免近处除以 0、亮度爆表
;;   kl 线性项：中段距离的主力，让亮度稳稳地掉
;;   kq 二次项：远段的主力，d 一大就压过前两项
(define k-constant 1.0)
(define k-linear 0.09)
(define k-quadratic 0.032)

;; 四个立方体排在灯正前方的轴上，中心离灯 1.5 / 3 / 5 / 7：
;;   z = 0.5 → 距灯 1.5；z = -1 → 3；z = -3 → 5；z = -5 → 7
;; 和 directional.rkt 同一份几何、同样的朝向，这次画面上会亮成四个档次：
;; 最近的明显亮、最远的只剩一点橙色——距离这次进公式了。
(define cube-positions
  (list (vec-3 0.0 0.0  0.5)
        (vec-3 0.0 0.0 -1.0)
        (vec-3 0.0 0.0 -3.0)
        (vec-3 0.0 0.0 -5.0)))

;; 相机偏到右上方（5, 3, 4）看向这一排的中段：四个立方体因此错开排进画面，
;; 不会一个躲在前一个身后。
(define eye-pos (vec-3 5.0 3.0 4.0))
(define look-at-pos (vec-3 0.0 0.0 -2.0))

(define program 0)
;; uniform 位置：链接后查一次（名字用 GLSL 里的写法），之后每帧直接 uniform-set!
(define u-model-loc 0)
(define u-view-loc 0)
(define u-proj-loc 0)
(define u-normal-matrix-loc 0)
(define u-light-pos-loc 0)
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
  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear (bitwise-ior gl-color-buffer-bit gl-depth-buffer-bit))

  (define view (m-look-at eye-pos look-at-pos (vec-3 0.0 1.0 0.0)))
  (define proj (m-perspective (degrees->radians 45.0) aspect 0.1 100.0))

  (gl-bind-vertex-array vao)
  (program-use program)

  ;; 灯的位置与三个系数整帧只上传一次：它们不随立方体变。
  (uniform-set! u-view-loc 'mat-4 view)
  (uniform-set! u-proj-loc 'mat-4 proj)
  (uniform-set! u-light-pos-loc 'vec-3 light-pos)
  (uniform-set! u-kc-loc 'float k-constant)
  (uniform-set! u-kl-loc 'float k-linear)
  (uniform-set! u-kq-loc 'float k-quadratic)
  (uniform-set! u-light-color-loc 'vec-3 light-color)
  (uniform-set! u-view-pos-loc 'vec-3 eye-pos)
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
  (make-window #:title "11 点光"
               #:width win-w
               #:height win-h
               #:init init
               #:draw draw
               #:on-size (lambda (cw ch) (set! aspect (/ cw ch)))
               #:on-close shutdown))
(send frame show #t)

;; 没有 timer：灯和四个立方体都不动，画面上没有东西在变。

;; ------------------------------------------------------------
;; 试试看（改完重跑）：
;;   · k-quadratic 从 0.032 改成 0.007：远段掉得慢多了，最远那个立方体明显抬亮。
;;   · k-constant 从 1.0 改成 0.0：分母失去保底，最近的立方体直接爆亮成一片白。
;;   · light-pos 的 z 从 2.0 改成 0.0：灯挪到第二个立方体旁边，近的两个更亮、
;;     远的两个更暗——把"距离"这个变量整体挪一挪，梯度跟着挪。
;; ============================================================