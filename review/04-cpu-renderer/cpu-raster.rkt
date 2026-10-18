#lang glsl
;; ============================================================
;; 复习 04 · 用 CPU 走一遍渲染管线
;; 运行：racket review/04-cpu-renderer/cpu-raster.rkt
;; ============================================================
;; 这个脚本不创建窗口、不调用任何 gl-* 函数，只用 Racket 把 14 课
;; 的同一只茶壶画出来。它存在的目的只有一个：
;;
;;   把 GPU 替你做的事，一行一行地写出来。
;;
;; 对照 14-model-loading.rkt 的 shader：
;;
;;   gl_Position = u_proj * u_view * u_model * vec4(pos, 1.0)   ; 顶点着色器
;;   frag_color  = Phong(v_normal, v_frag_pos)                   ; 片元着色器
;;
;; 这里同样两步，只是第二步外面多了一圈"决定哪个像素归哪个三角形"的循环——
;; 那一圈就是 GPU 固定管线里的「光栅化 + 深度测试」，平时看不见，所以最容易
;; 被忽略。看懂这个文件，就知道 gl-draw-elements 一行背后到底发生了什么。
;;
;; 输出：
;;   review/04-cpu-renderer/cpu-teapot-<宽>x<高>.png / .ppm
;;   终端里的统计数字（顶点数、片元数、分阶段耗时）
;;
;; 注意：这是教学用渲染器，不是产品级实现。没有做：
;;   近平面裁剪、纹理、抗锯齿、多光源、混合、gamma。
;; 这些都留给 GPU 版本，正好说明"为什么要用 GPU"。

(require racket/class
         racket/draw
         racket/file
         racket/math
         "../../lib/matrix.rkt"
         "../../lib/obj.rkt")

;; 命名：vec-3 / vec-4 / mat-4 / vx / vdot / vnormalize 这些名字来自
;; #lang glsl（racket-shader），不是 racket/math；racket/math 只提供
;; degrees->radians 这类数学函数。生成 GLSL 时 vec-3 才写成 vec3。

;; ------------------------------------------------------------
;; 一、帧缓冲：CPU 版的"显存"
;; ------------------------------------------------------------
;; GPU 上有 color buffer 和 depth buffer；这里就是两块 Racket 向量。
;; 彩色缓冲存 0..1 的浮点 RGB，最后再量化成 0..255，写进 PNG / PPM。
;; 分辨率可以从命令行改：racket cpu-raster.rkt 800 600
;; 用来看“片元数（像素数）涨上去以后，CPU 的耗时会怎么变”。
(define args (vector->list (current-command-line-arguments)))
(define W (if (>= (length args) 1) (string->number (list-ref args 0)) 320))
(define H (if (>= (length args) 2) (string->number (list-ref args 1)) 240))

(define color-buffer (make-vector (* W H 3) 0.0))
(define depth-buffer (make-vector (* W H) +inf.0))   ; 越小越近

;; 背景色（对应 gl-clear-color 0.08 0.10 0.16）
(define (clear!)
  (vector-fill! color-buffer 0.0)
  (vector-fill! depth-buffer +inf.0)
  ;; 背景不是纯黑：把 0.08/0.10/0.16 铺满
  (for ([i (in-range 0 (* W H 3) 3)])
    (vector-set! color-buffer i 0.08)
    (vector-set! color-buffer (+ i 1) 0.10)
    (vector-set! color-buffer (+ i 2) 0.16)))

(define (put-pixel! x y r g b)
  (define i (* 3 (+ (* y W) x)))
  (vector-set! color-buffer i r)
  (vector-set! color-buffer (+ i 1) g)
  (vector-set! color-buffer (+ i 2) b))

;; ------------------------------------------------------------
;; 二、场景参数：和 14 课完全一致
;; ------------------------------------------------------------
(define eye (vec-3 4.5 3.0 6.5))
(define target (vec-3 0.0 1.2 0.0))
(define up (vec-3 0.0 1.0 0.0))

(define view (m-look-at eye target up))
(define proj (m-perspective (degrees->radians 45.0) (/ W H) 0.1 100.0))
(define model
  (mat4-mul (m-scale (vec-3 0.8 0.8 0.8))
            (m-rotate-y 0.6)))          ; 固定角度，保证每次跑出来一样

(define object-color (vec-3 0.82 0.55 0.35))
(define light-pos (vec-3 4.0 5.0 4.0))
(define light-color (vec-3 1.0 1.0 1.0))
(define view-pos eye)

;; MVP 在 CPU 上一次乘好，等于 GPU 侧把三个矩阵作为 uniform 传进去；
;; 差别只是：GPU 对每个顶点做 proj*view*model*pos，CPU 在这里先合并成矩阵。
(define mvp (mat4-mul proj (mat4-mul view model)))
(define normal-matrix (m-normal-matrix model))

;; ------------------------------------------------------------
;; 三、顶点着色器：每个顶点跑一次
;; ------------------------------------------------------------
;; GPU 版（14 课的 model-vert 里）：
;;   v-normal   = u-normal-matrix * normal
;;   v-frag-pos = (u-model * vec4(pos, 1.0)).xyz
;;   gl-position = u-proj * u-view * u-model * vec4(pos, 1.0)
;;
;; 这里返回一个结构，把这三样带上；后面光栅化阶段负责在三角形内部插值。
(struct v-out (clip ndc world normal) #:transparent)

(define (vertex-shader v)
  (define p (vec-4 (vx (obj-vertex-pos v))
                   (vy (obj-vertex-pos v))
                   (vz (obj-vertex-pos v))
                   1.0))
  (define world4 (mat4-mul-vec model p))
  (define clip (mat4-mul-vec mvp p))
  (define w (vw clip))
  (v-out clip
         (vec-3 (/ (vx clip) w) (/ (vy clip) w) (/ (vz clip) w))
         (vec-3 (vx world4) (vy world4) (vz world4))
         (mat3-mul-vec normal-matrix (obj-vertex-normal v))))

;; 视口变换：NDC (-1..1) → 像素坐标 (0..W, 0..H)。
;; GPU 在光栅化之前自动做这一步；注意 y 要翻转，因为 NDC 的 +y 朝上、
;; 图片的行号朝下。
(define (ndc->screen ndc)
  (values (* (+ (vx ndc) 1.0) 0.5 (- W 1))
          (* (- 1.0 (vy ndc)) 0.5 (- H 1))
          (vz ndc)))                       ; z 直接留给深度测试

;; ------------------------------------------------------------
;; 四、片元着色器：每个通过深度测试的像素跑一次
;; ------------------------------------------------------------
;; 和 14 课的 model-frag 是同一个公式，一个字都没改。
(define (fragment-shader frag-pos normal)
  (define norm (vnormalize normal))
  (define light-dir (vnormalize (vsub light-pos frag-pos)))
  (define view-dir (vnormalize (vsub view-pos frag-pos)))
  (define reflect-dir (vreflect (vneg light-dir) norm))

  (define ambient 0.15)
  (define diff (max (vdot norm light-dir) 0.0))
  (define spec (* (expt (max (vdot view-dir reflect-dir) 0.0) 32.0) 0.4))
  (define lit (* (+ ambient diff spec) 1.0))
  (vmul (vmul light-color lit) object-color))

;; ------------------------------------------------------------
;; 五、光栅化：GPU 固定管线替你做的部分
;; ------------------------------------------------------------
;; 一个三角形在屏幕上覆盖一块像素。对每个像素，要先回答两个问题：
;;   1. 这个像素在三角形里面吗？  → 边函数 / 重心坐标
;;   2. 它离相机比已经画过的更近吗？→ 深度测试
;; 都通过，才算出一个片元，交给片元着色器。
;;
;; 这里用重心坐标 (b0, b1, b2)：
;;   p = b0*p0 + b1*p1 + b2*p2,  b0+b1+b2 = 1
;; 三个 b 都 ≥ 0 就在三角形内。

(define (edge ax ay bx by px py)
  (- (* (- bx ax) (- py ay))
     (* (- by ay) (- px ax))))

;; 透视校正插值：
;; 屏幕空间的重心坐标不能直接插值世界坐标/法线——因为远处的顶点在屏幕上
;; 被压缩了。正确做法是先插值 attr/w 和 1/w，再除掉 1/w。
;; GPU 对每个 varying 自动做这件事，这里手动写一遍。
(define (interp3 a b c b0 b1 b2 inv-w0 inv-w1 inv-w2)
  (define inv-w (+ (* b0 inv-w0) (* b1 inv-w1) (* b2 inv-w2)))
  (define (f x0 x1 x2)
    (/ (+ (* b0 inv-w0 x0) (* b1 inv-w1 x1) (* b2 inv-w2 x2)) inv-w))
  (vec-3 (f (vx a) (vx b) (vx c))
         (f (vy a) (vy b) (vy c))
         (f (vz a) (vz b) (vz c))))

(struct stats (triangles culled fragments shaded) #:mutable)

(define (rasterize-triangle! v0 v1 v2 st)
  (define w0 (vw (v-out-clip v0)))
  (define w1 (vw (v-out-clip v1)))
  (define w2 (vw (v-out-clip v2)))
  ;; 近平面之后的顶点先跳过（GPU 会做裁剪，这里简化）
  (unless (or (<= w0 0.0) (<= w1 0.0) (<= w2 0.0))
    (define-values (x0 y0 z0) (ndc->screen (v-out-ndc v0)))
    (define-values (x1 y1 z1) (ndc->screen (v-out-ndc v1)))
    (define-values (x2 y2 z2) (ndc->screen (v-out-ndc v2)))

    (define area (edge x0 y0 x1 y1 x2 y2))
    (cond
      [(< (abs area) 1e-9) (void)]        ; 退化成线段，没有面积
      [else
       ;; 背面剔除：屏幕空间里顺时针/逆时针，对应 GPU 的 gl-cull-face。
       ;; 注意这里 y 轴已经翻过，所以“逆时针”的符号和世界空间相反；
       ;; 保留 area < 0 的一面，正好是朝外的那一半。
       (if (>= area 0.0)
           (set-stats-culled! st (add1 (stats-culled st)))
           (let ()
             (set-stats-triangles! st (add1 (stats-triangles st)))

             ;; 包围盒：只扫三角形盖住的那一小块，不扫整个屏幕
             (define min-x (max 0 (inexact->exact (floor (min x0 x1 x2)))))
             (define max-x (min (sub1 W) (inexact->exact (ceiling (max x0 x1 x2)))))
             (define min-y (max 0 (inexact->exact (floor (min y0 y1 y2)))))
             (define max-y (min (sub1 H) (inexact->exact (ceiling (max y0 y1 y2)))))

             (define inv-w0 (/ 1.0 w0))
             (define inv-w1 (/ 1.0 w1))
             (define inv-w2 (/ 1.0 w2))

             (for* ([y (in-range min-y (add1 max-y))]
                    [x (in-range min-x (add1 max-x))])
               (define px (+ x 0.5))
               (define py (+ y 0.5))

               ;; 重心坐标 = 三个边函数 / 总面积
               (define b0 (/ (edge x1 y1 x2 y2 px py) area))
               (define b1 (/ (edge x2 y2 x0 y0 px py) area))
               (define b2 (/ (edge x0 y0 x1 y1 px py) area))

               (when (and (>= b0 0.0) (>= b1 0.0) (>= b2 0.0))
                 (set-stats-fragments! st (add1 (stats-fragments st)))

                 ;; 深度：z_ndc 在屏幕空间是线性的，直接插值即可
                 (define z (+ (* b0 z0) (* b1 z1) (* b2 z2)))
                 (define dx (+ (* y W) x))
                 (when (< z (vector-ref depth-buffer dx))
                   (vector-set! depth-buffer dx z)
                   (set-stats-shaded! st (add1 (stats-shaded st)))

                   ;; 插值出这一像素的世界位置和法线
                   (define frag-pos
                     (interp3 (v-out-world v0) (v-out-world v1) (v-out-world v2)
                              b0 b1 b2 inv-w0 inv-w1 inv-w2))
                   (define normal
                     (interp3 (v-out-normal v0) (v-out-normal v1) (v-out-normal v2)
                              b0 b1 b2 inv-w0 inv-w1 inv-w2))
                   (define c (fragment-shader frag-pos normal))
                   (put-pixel! x y (vx c) (vy c) (vz c)))))))])))

;; ------------------------------------------------------------
;; 六、主流程：读模型 → 顶点变换 → 三角形装配 → 光栅化
;; ------------------------------------------------------------
(define (now) (current-inexact-monotonic-milliseconds))
(define (secs t0) (/ (- (now) t0) 1000.0))

(define (main)
  (define t-start (now))
  (clear!)

  ;; 0. 读模型：一次性成本，GPU 版本也要做
  (define t-parse (now))
  (define mesh (read-obj (build-path (current-directory)
                                     "assets/models/teapot.obj")))
  (define parse-s (secs t-parse))
  (define verts (obj-mesh-vertices mesh))
  (define indices (obj-mesh-indices mesh))

  ;; 1. 顶点着色器：1202 个顶点，每个跑一次
  (define t-vertex (now))
  (define transformed
    (for/vector ([v (in-vector verts)])
      (vertex-shader v)))
  (define vertex-s (secs t-vertex))

  ;; 2. 图元装配 + 光栅化：2256 个三角形
  (define t-raster (now))
  (define st (stats 0 0 0 0))
  (for ([i (in-range 0 (vector-length indices) 3)])
    (rasterize-triangle! (vector-ref transformed (vector-ref indices i))
                         (vector-ref transformed (vector-ref indices (+ i 1)))
                         (vector-ref transformed (vector-ref indices (+ i 2)))
                         st))
  (define raster-s (secs t-raster))

  ;; 3. 写图：PPM 是最简单的无依赖格式；同时用 racket/draw 存一份 PNG，
  ;;    方便直接打开 / 嵌到网页里看。
  (define t-write (now))
  (define base (build-path (current-directory)
                           (format "review/04-cpu-renderer/cpu-teapot-~ax~a" W H)))
  (define ppm-path (path-add-extension base ".ppm"))
  (define png-path (path-add-extension base ".png"))

  ;; 量化成 0..255，PPM 和 PNG 共用
  (define rgb (make-bytes (* W H 3)))
  (for ([i (in-range (* W H 3))])
    (define v (exact->inexact (vector-ref color-buffer i)))
    (bytes-set! rgb i
                (inexact->exact
                 (round (* 255.0 (max 0.0 (min 1.0 v)))))))

  (with-output-to-file ppm-path #:exists 'replace
    (lambda ()
      (display (format "P6\n~a ~a\n255\n" W H))
      (write-bytes rgb)))

  ;; PNG 要 ARGB（每像素 4 字节），把 RGB 摊开、alpha 固定 255
  (define argb (make-bytes (* W H 4)))
  (for ([i (in-range (* W H))])
    (bytes-set! argb (+ (* i 4) 0) 255)
    (bytes-set! argb (+ (* i 4) 1) (bytes-ref rgb (+ (* i 3) 0)))
    (bytes-set! argb (+ (* i 4) 2) (bytes-ref rgb (+ (* i 3) 1)))
    (bytes-set! argb (+ (* i 4) 3) (bytes-ref rgb (+ (* i 3) 2))))
  (define bm (make-bitmap W H))
  (define dc (new bitmap-dc% [bitmap bm]))
  (send dc set-argb-pixels 0 0 W H argb)
  (send bm save-file png-path 'png)
  (send dc set-bitmap #f)

  (define write-s (secs t-write))
  (define total-s (secs t-start))

  ;; 4. 报告：CPU 做了多少事
  (printf "CPU 渲染 · Utah Teapot · ~ax~a\n\n" W H)
  (printf "模型         ~a 顶点 / ~a 三角形（~a 个索引）\n"
          (obj-mesh-vertex-count mesh)
          (/ (obj-mesh-index-count mesh) 3)
          (obj-mesh-index-count mesh))
  (printf "顶点变换     ~a 次（for/vector，串行）\n"
          (vector-length transformed))
  (printf "通过剔除     ~a 个三角形\n" (stats-triangles st))
  (printf "背面剔除     ~a 个三角形\n" (stats-culled st))
  (printf "候选片元     ~a 个（包围盒内、且落在三角形内）\n"
          (stats-fragments st))
  (printf "通过深度测试 ~a 个（这些才真正跑片元着色器）\n\n"
          (stats-shaded st))

  (printf "分阶段耗时：\n")
  (printf "  解析 OBJ     ~a s\n" (real->decimal-string parse-s 3))
  (printf "  顶点变换     ~a s\n" (real->decimal-string vertex-s 3))
  (printf "  光栅化+着色  ~a s   ← 和片元数成正比\n" (real->decimal-string raster-s 3))
  (printf "  写文件       ~a s\n" (real->decimal-string write-s 3))
  (printf "  合计         ~a s\n\n" (real->decimal-string total-s 3))

  (printf "屏幕上一共 ~a 个像素，其中 ~a 个被三角形盖住（覆盖率 ~a%）。\n"
          (* W H) (stats-shaded st)
          (real->decimal-string (* 100.0 (/ (* 1.0 (stats-shaded st)) (* W H))) 1))
  (printf "每个被盖住的像素平均产生 ~a 个片元（多出来的部分就是 overdraw）。\n"
          (real->decimal-string (/ (* 1.0 (stats-fragments st)) (stats-shaded st)) 2))
  (printf "分辨率翻倍（边长 ×2）→ 像素数 ×4 → 光栅化阶段大约也要 ×4。\n")
  (printf "这是 CPU 版的成本模型：一次顶点变换 + 每个像素一次着色。\n\n")

  (printf "GPU 版本（14 课）做的事：\n")
  (printf "  1. CPU 上传 ~a 个顶点 + ~a 个索引（一次性）\n"
          (vector-length verts) (vector-length indices))
  (printf "  2. gl-draw-elements gl-triangles ~a gl-unsigned-short 0 —— 一次调用\n"
          (vector-length indices))
  (printf "  3. 同样的顶点着色器 / 片元着色器公式，由 GPU 并行执行\n")
  (printf "  4. 光栅化和深度测试由固定管线完成，不需要你写\n\n")
  (printf "图片已写入：\n  ~a\n  ~a\n" png-path ppm-path))

(main)
