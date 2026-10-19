#lang glsl-opengl
;; ============================================================
;; 15 · 精灵图与 HD 3D 场景
;; 运行：racket 15-sprite-atlas/sprite-atlas.rkt
;; 上一课：14-model-loading/model-loading.rkt
;; ============================================================
;; 目标：
;;   1. 从一张 4×4 的精灵图（sprite sheet）加载 16 帧动画
;;   2. 用帧号算出这一帧在图集里的 UV 范围
;;   3. 把精灵做成 billboard，放进一个 1280×720 的 3D 场景
;;   4. 场景里有渐变天空、网格地面、Utah Teapot、叠加发光的 λ 符印
;;
;; 精灵图：assets/sprites/racket-sigil.png
;;   1024×1024，4 列 × 4 行，每帧 256×256，帧 0 在左上角。
;;
;; 计算位置：
;;   PNG 读取/上传、场景矩阵、相机在 CPU；
;;   图集 UV、纹理采样、billboard 变换、加法混合在 GPU。

(require racket/class
         racket/gui                     ; timer%
         racket/math                    ; degrees->radians / pi
         "../lib/window.rkt"
         "../lib/gpu.rkt"
         "../lib/matrix.rkt"
         "../lib/obj.rkt"
         "../lib/sprite-sheet.rkt")

;; ------------------------------------------------------------
;; 一、CPU：模型 + 精灵图路径
;; ------------------------------------------------------------
(define mesh
  (read-obj (build-path (current-directory) "assets/models/teapot.obj")))

(define sprite-path
  (build-path (current-directory) "assets/sprites/racket-sigil.png"))
(define sprite-cols 4)
(define sprite-rows 4)

(shader-struct teapot-vertex (vec-3 pos) (vec-3 normal))

(define teapot-data
  (apply concat-vecs
         (for/list ([v (in-vector (obj-mesh-vertices mesh))])
           (teapot-vertex->f32vector
            (teapot-vertex (obj-vertex-pos v)
                           (obj-vertex-normal v))))))

(define teapot-indices
  (apply u16-array (vector->list (obj-mesh-indices mesh))))
(define teapot-index-count (obj-mesh-index-count mesh))

;; ------------------------------------------------------------
;; 二、着色器
;; ------------------------------------------------------------

;; ---- 2.1 天空：全屏四边形，只有 UV，没有矩阵 ----
(glsl-shader sky-vert
  (version 330 core)
  (layout (location 0) in vec-2 pos)
  (out vec-2 v-uv)
  (define (main) void
    (set! v-uv (* (+ pos (vec-2 1.0 1.0)) 0.5))
    (set! gl-position (vec-4 pos 0.0 1.0))))

(glsl-shader sky-frag
  (version 330 core)
  (in vec-2 v-uv)
  (out vec-4 frag-color)
  (define (main) void
    (vec-3 top (vec-3 0.012 0.022 0.055))
    (vec-3 bottom (vec-3 0.075 0.13 0.20))
    (float k (smoothstep 0.0 1.0 (y v-uv)))
    (vec-3 col (mix bottom top k))
    ;; 地平线附近加一点冷色辉光
    (float dy (- (y v-uv) 0.42))
    (float ady (max dy (- dy)))
    (float glow (exp (- (* 9.0 ady))))
    (set! col (+ col (vec-3 (* 0.04 glow) (* 0.10 glow) (* 0.16 glow))))
    (set! frag-color (vec-4 col 1.0))))

;; ---- 2.2 地面：一个巨大的四边形 + 程序化网格 ----
(glsl-shader ground-vert
  (version 330 core)
  (layout (location 0) in vec-3 pos)
  (uniform mat-4 u-view)
  (uniform mat-4 u-proj)
  (out vec-3 v-world)
  (define (main) void
    (set! v-world pos)
    (set! gl-position (* u-proj (* u-view (vec-4 pos 1.0))))))

(glsl-shader ground-frag
  (version 330 core)
  (in vec-3 v-world)
  (uniform vec-3 u-bg-color)
  (out vec-4 frag-color)
  (define (main) void
    (vec-2 p (vec-2 (x v-world) (z v-world)))
    ;; 每 2 个世界单位一条网格线
    (vec-2 f (fract (* p 0.5)))
    (vec-2 q (vec-2 (max (- (x f) 0.5) (- 0.5 (x f)))
                    (max (- (y f) 0.5) (- 0.5 (y f)))))
    (float line (smoothstep 0.45 0.5 (max (x q) (y q))))
    (vec-3 base (vec-3 0.015 0.025 0.045))
    (vec-3 grid (vec-3 0.04 0.20 0.28))
    (vec-3 col (mix base grid (* line 0.65)))
    ;; 中心附近有一点冷光
    (float d (length p))
    (float glow (exp (- (* d 0.18))))
    (set! col (+ col (vec-3 (* 0.03 glow) (* 0.12 glow) (* 0.16 glow))))
    ;; 远处淡出到天空色，避免地面边缘出现硬线
    (float fog (smoothstep 10.0 42.0 d))
    (set! col (mix col u-bg-color fog))
    (set! frag-color (vec-4 col 1.0))))

;; ---- 2.3 茶壶：Phong + 边缘光 ----
(glsl-shader teapot-vert
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

(glsl-shader teapot-frag
  (version 330 core)
  (in vec-3 v-normal)
  (in vec-3 v-frag-pos)
  (uniform vec-3 u-base-color)
  (uniform vec-3 u-rim-color)
  (uniform vec-3 u-light-pos)
  (uniform vec-3 u-light-color)
  (uniform vec-3 u-view-pos)
  (out vec-4 frag-color)
  (define (main) void
    (vec-3 n (normalize v-normal))
    (vec-3 l (normalize (- u-light-pos v-frag-pos)))
    (vec-3 v (normalize (- u-view-pos v-frag-pos)))
    (vec-3 r (reflect (- l) n))

    (float ambient 0.16)
    (float diff (max (dot n l) 0.0))
    (float spec (* (pow (max (dot v r) 0.0) 64.0) 0.55))
    ;; 边缘光：视线越贴着表面，边缘越亮
    (float rim (* (pow (- 1.0 (max (dot n v) 0.0)) 3.0) 0.45))

    (vec-3 lit (* (+ ambient diff) u-light-color))
    (vec-3 col (+ (* u-base-color lit)
                  (* spec u-light-color)
                  (* rim u-rim-color)))
    (set! frag-color (vec-4 col 1.0))))

;; ---- 2.4 精灵：图集 UV + billboard + 加法混合 ----
(glsl-shader sprite-vert
  (version 330 core)
  (layout (location 0) in vec-3 pos)
  (layout (location 1) in vec-2 uv)
  (uniform mat-4 u-model)
  (uniform mat-4 u-view)
  (uniform mat-4 u-proj)
  (out vec-2 v-uv)
  (define (main) void
    (set! v-uv uv)
    (set! gl-position (* u-proj (* u-view (* u-model (vec-4 pos 1.0)))))))

(glsl-shader sprite-frag
  (version 330 core)
  (in vec-2 v-uv)
  (uniform sampler-2d u-atlas)
  (uniform float u-frame)             ; 0..15
  (uniform float u-cols)              ; 4
  (uniform float u-rows)              ; 4
  (uniform vec-3 u-tint)
  (out vec-4 frag-color)
  (define (main) void
    ;; 帧号 → 列/行。PNG 的帧 0 在左上角，而 GL 的 v=0 在底部，
    ;; 所以行号要翻：row = rows - 1 - rowTop。
    (float col (mod u-frame u-cols))
    (float row-top (floor (/ u-frame u-cols)))
    (float row (- u-rows 1.0 row-top))
    (vec-2 cell (vec-2 (/ 1.0 u-cols) (/ 1.0 u-rows)))
    (vec-2 uv (* (vec-2 (+ col (x v-uv)) (+ row (y v-uv))) cell))

    (vec-4 tex (texture u-atlas uv))
    (set! frag-color (vec-4 (* (vec-3 tex) u-tint) (.w tex)))))

;; ------------------------------------------------------------
;; 三、几何：天空、地面、精灵四边形
;; ------------------------------------------------------------
(shader-struct sky-vertex (vec-2 pos))
(define sky-data
  (concat-vecs (vec-2 -1.0 -1.0) (vec-2 1.0 -1.0) (vec-2 1.0 1.0)
               (vec-2 -1.0 -1.0) (vec-2 1.0 1.0) (vec-2 -1.0 1.0)))

(shader-struct ground-vertex (vec-3 pos))
(define ground-data
  (concat-vecs (vec-3 -60.0 0.0 -60.0) (vec-3 60.0 0.0 -60.0)
               (vec-3 60.0 0.0 60.0)
               (vec-3 -60.0 0.0 -60.0) (vec-3 60.0 0.0 60.0)
               (vec-3 -60.0 0.0 60.0)))

;; 单位四边形：局部 XY 平面，UV (0,0) 在左下角
(shader-struct sprite-vertex (vec-3 pos) (vec-2 uv))
(define sprite-quad
  (concat-vecs (vec-3 -0.5 -0.5 0.0) (vec-2 0.0 0.0)   ; 左下
               (vec-3  0.5 -0.5 0.0) (vec-2 1.0 0.0)   ; 右下
               (vec-3  0.5  0.5 0.0) (vec-2 1.0 1.0)   ; 右上
               (vec-3 -0.5  0.5 0.0) (vec-2 0.0 1.0))) ; 左上
(define sprite-indices (u16-array 0 1 2  0 2 3))

;; ------------------------------------------------------------
;; 四、GL 对象
;; ------------------------------------------------------------
(define sky-program #f)
(define ground-program #f)
(define teapot-program #f)
(define sprite-program #f)

(define sky-vao #f)
(define ground-vao #f)
(define teapot-vao #f)
(define sprite-vao #f)
(define sheet #f)

(glsl-uniform-setter! sky-program sky-vert sky-frag #:prefix "sky-")
(glsl-uniform-setter! ground-program ground-vert ground-frag #:prefix "ground-")
(glsl-uniform-setter! teapot-program teapot-vert teapot-frag #:prefix "teapot-")
(glsl-uniform-setter! sprite-program sprite-vert sprite-frag #:prefix "sprite-")

(define (init)
  (set! sky-program (own! 'program (build-program (gl-vertex-shader sky-vert)
                                                  (gl-fragment-shader sky-frag))))
  (set! ground-program (own! 'program (build-program (gl-vertex-shader ground-vert)
                                                     (gl-fragment-shader ground-frag))))
  (set! teapot-program (own! 'program (build-program (gl-vertex-shader teapot-vert)
                                                     (gl-fragment-shader teapot-frag))))
  (set! sprite-program (own! 'program (build-program (gl-vertex-shader sprite-vert)
                                                     (gl-fragment-shader sprite-frag))))

  (gl-enable gl-depth-test)

  ;; 天空 VAO
  (set! sky-vao (own! 'vertex-array (vertex-array-new)))
  (gl-bind-vertex-array sky-vao)
  (define sky-vbo (own! 'buffer (buffer-new)))
  (gl-bind-buffer gl-array-buffer sky-vbo)
  (buffer-upload gl-array-buffer sky-data gl-static-draw)
  (glsl-vertex-layout! sky-vert sky-vertex)
  (gl-bind-vertex-array 0)

  ;; 地面 VAO
  (set! ground-vao (own! 'vertex-array (vertex-array-new)))
  (gl-bind-vertex-array ground-vao)
  (define ground-vbo (own! 'buffer (buffer-new)))
  (gl-bind-buffer gl-array-buffer ground-vbo)
  (buffer-upload gl-array-buffer ground-data gl-static-draw)
  (glsl-vertex-layout! ground-vert ground-vertex)
  (gl-bind-vertex-array 0)

  ;; 茶壶 VAO
  (set! teapot-vao (own! 'vertex-array (vertex-array-new)))
  (gl-bind-vertex-array teapot-vao)
  (define teapot-vbo (own! 'buffer (buffer-new)))
  (gl-bind-buffer gl-array-buffer teapot-vbo)
  (buffer-upload gl-array-buffer teapot-data gl-static-draw)
  (glsl-vertex-layout! teapot-vert teapot-vertex)
  (define teapot-ebo (own! 'buffer (buffer-new)))
  (gl-bind-buffer gl-element-array-buffer teapot-ebo)
  (buffer-upload gl-element-array-buffer teapot-indices gl-static-draw)
  (gl-bind-vertex-array 0)

  ;; 精灵 VAO
  (set! sprite-vao (own! 'vertex-array (vertex-array-new)))
  (gl-bind-vertex-array sprite-vao)
  (define sprite-vbo (own! 'buffer (buffer-new)))
  (gl-bind-buffer gl-array-buffer sprite-vbo)
  (buffer-upload gl-array-buffer sprite-quad gl-static-draw)
  (glsl-vertex-layout! sprite-vert sprite-vertex)
  (define sprite-ebo (own! 'buffer (buffer-new)))
  (gl-bind-buffer gl-element-array-buffer sprite-ebo)
  (buffer-upload gl-element-array-buffer sprite-indices gl-static-draw)
  (gl-bind-vertex-array 0)

  ;; 加载精灵图（必须在 GL 上下文里）
  (set! sheet (load-sprite-sheet sprite-path sprite-cols sprite-rows))

  (program-use sprite-program)
  (gl-active-texture gl-texture0)
  (gl-bind-texture gl-texture-2d (sprite-sheet-id sheet))
  (set-sprite-u-atlas! 0)
  (set-sprite-u-cols! (exact->inexact (sprite-sheet-cols sheet)))
  (set-sprite-u-rows! (exact->inexact (sprite-sheet-rows sheet))))

;; ------------------------------------------------------------
;; 五、窗口 / 相机 / 场景
;; ------------------------------------------------------------
(define win-w 1280)
(define win-h 720)
(define aspect (/ win-w win-h))

;; 场景配色
(define bg-color (vec-3 0.03 0.05 0.10))
(define teapot-base (vec-3 0.10 0.45 0.52))
(define teapot-rim (vec-3 0.15 0.85 1.00))

;; 三个精灵色调，让光环有颜色变化
(define sprite-tints
  (list (vec-3 0.35 0.90 1.00)
        (vec-3 1.00 0.55 0.24)
        (vec-3 0.45 1.00 0.60)))

(define sprite-count 12)

(define (draw)
  (gl-clear-color (vx bg-color) (vy bg-color) (vz bg-color) 1.0)
  (gl-clear (bitwise-ior gl-color-buffer-bit gl-depth-buffer-bit))

  (define t (/ (current-inexact-monotonic-milliseconds) 1000.0))

  ;; 相机缓慢绕圈，并轻微上下浮动
  (define eye (vec-3 (* 7.5 (cos (* 0.22 t)))
                     (+ 3.2 (* 0.5 (sin (* 0.37 t))))
                     (* 7.5 (sin (* 0.22 t)))))
  (define target (vec-3 0.0 1.25 0.0))
  (define view (m-look-at eye target (vec-3 0.0 1.0 0.0)))
  (define proj (m-perspective (degrees->radians 45.0) aspect 0.1 200.0))

  ;; billboard 基：right / up 在屏幕平面里，normal 朝向相机
  (define forward (vnormalize (vsub target eye)))
  (define right (vnormalize (vcross forward (vec-3 0.0 1.0 0.0))))
  (define up (vcross right forward))
  (define normal (vcross right up))

  ;; ---- 1. 天空：先画，不写深度 ----
  (gl-disable gl-depth-test)
  (gl-depth-mask #f)
  (program-use sky-program)
  (gl-bind-vertex-array sky-vao)
  (gl-draw-arrays gl-triangles 0 6)

  ;; ---- 2. 地面：写深度 ----
  (gl-depth-mask #t)
  (gl-enable gl-depth-test)
  (program-use ground-program)
  (gl-bind-vertex-array ground-vao)
  (set-ground-u-view! view)
  (set-ground-u-proj! proj)
  (set-ground-u-bg-color! bg-color)
  (gl-draw-arrays gl-triangles 0 6)

  ;; ---- 3. 茶壶：写深度 ----
  (program-use teapot-program)
  (gl-bind-vertex-array teapot-vao)
  (define teapot-model
    (mat4-mul (m-translate (vec-3 0.0 0.03 0.0))
              (mat4-mul (m-scale (vec-3 0.85 0.85 0.85))
                        (m-rotate-y (* 0.18 t)))))
  (set-teapot-u-model! teapot-model)
  (set-teapot-u-view! view)
  (set-teapot-u-proj! proj)
  (set-teapot-u-normal-matrix! (m-normal-matrix teapot-model))
  (set-teapot-u-base-color! teapot-base)
  (set-teapot-u-rim-color! teapot-rim)
  (set-teapot-u-light-pos! (vec-3 4.0 7.0 5.0))
  (set-teapot-u-light-color! (vec-3 1.0 0.96 0.88))
  (set-teapot-u-view-pos! eye)
  (gl-draw-elements gl-triangles teapot-index-count gl-unsigned-short 0)

  ;; ---- 4. 精灵：深度测试开、深度写入关、加法混合 ----
  (program-use sprite-program)
  (gl-bind-vertex-array sprite-vao)
  (gl-active-texture gl-texture0)
  (gl-bind-texture gl-texture-2d (sprite-sheet-id sheet))
  (set-sprite-u-view! view)
  (set-sprite-u-proj! proj)
  (set-sprite-u-atlas! 0)

  (gl-enable gl-blend)
  (gl-blend-func gl-src-alpha gl-one)     ; 加法混合：光叠加，不会把背景压暗
  (gl-depth-mask #f)

  (for ([i (in-range sprite-count)])
    (define a (+ (* 0.35 t) (* i (/ (* 2.0 pi) sprite-count))))
    (define r (+ 2.7 (* 0.55 (sin (+ (* 1.7 t) i)))))
    (define h (+ 0.9
                 (* 2.2 (/ i sprite-count))
                 (* 0.25 (sin (+ (* 2.3 t) i)))))
    (define center (vec-3 (* r (cos a)) h (* r (sin a))))
    (define size (+ 0.42 (* 0.12 (sin (+ (* 1.3 t) i)))))
    (define frame (modulo (+ (inexact->exact (floor (* t 12.0))) (* i 3))
                          (sprite-sheet-frame-count sheet)))
    (define tint (list-ref sprite-tints (modulo i (length sprite-tints))))
    (define model
      (mat-4 (vec-4 (* (vx right) size) (* (vy right) size) (* (vz right) size) 0.0)
             (vec-4 (* (vx up) size) (* (vy up) size) (* (vz up) size) 0.0)
             (vec-4 (vx normal) (vy normal) (vz normal) 0.0)
             (vec-4 (vx center) (vy center) (vz center) 1.0)))
    (set-sprite-u-model! model)
    (set-sprite-u-frame! (exact->inexact frame))
    (set-sprite-u-tint! tint)
    (gl-draw-elements gl-triangles 6 gl-unsigned-short 0))

  (gl-depth-mask #t)
  (gl-disable gl-blend)
  (gl-bind-vertex-array 0))

(define-values (frame canvas)
  (make-window #:title "15 精灵图与 HD 3D 场景"
               #:width win-w
               #:height win-h
               #:multisample 4
               #:init init
               #:draw draw
               #:on-size (lambda (cw ch) (set! aspect (/ cw ch)))
               #:on-close release-all!))
(send frame show #t)

(define timer
  (new timer% [interval 16]
              [notify-callback (lambda () (send canvas refresh))]))

;; ------------------------------------------------------------
;; 试试看（改完重跑）：
;;   · 把 sprite-cols / sprite-rows 改成 2 / 2，再换一张 2×2 的图：
;;     帧数、UV 格子、动画都会跟着变。
;;   · 把 gl-blend-func 改成 gl-src-alpha / gl-one-minus-src-alpha：
;;     精灵从“发光”变成普通透明贴纸，颜色会暗很多。
;;   · 把 gl-depth-mask #f 改成 #t：精灵会互相遮挡，光环前后关系变硬。
;;   · 把 sprite-count 从 12 改成 32：draw call 变多，但纹理只绑定一次。
;;   · 把 #:multisample 4 改成 0：地面网格和精灵边缘会出现锯齿。
;; ============================================================
