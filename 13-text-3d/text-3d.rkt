#lang glsl-opengl
;; ============================================================
;; 13 · 3D 文字
;; 运行：racket 13-text-3d/text-3d.rkt
;; 上一课：12-multiple-lights/multiple-lights.rkt
;; ============================================================
;; 目标：把文字放进 3D 世界，而不是画在屏幕角落。
;;
;; raylib 的文字本质是「font atlas + 每个字一个纹理四边形」。
;; 这里先做最小版本：整段字符串 → 一张纹理 → 一个 3D 四边形。
;;   1. lib/text-texture.rkt 用 racket/draw 把文字画成透明 bitmap
;;   2. 上传成 GL 纹理
;;   3. 在 3D 里画一个 unit quad，用 billboard 让它始终面向相机
;;
;; 场景：三个线框立方体 + 每个立方体上方的文字标签。
;; 相机绕场景转，文字始终面向相机（billboard）。
;;
;; 想扩展成 raylib 那种逐字绘制：把“整串纹理”换成“字形 atlas”，
;; 每个字符生成一个 quad 即可；其余 shader / 混合 / 深度处理完全一样。

(require racket/class
         racket/gui                     ; timer%
         racket/math                    ; degrees->radians
         "../lib/window.rkt"
         "../lib/matrix.rkt"
         "../lib/text-texture.rkt")

;; ------------------------------------------------------------
;; 一、着色器
;; ------------------------------------------------------------

;; 线框立方体：只需要位置；颜色用 uniform
(glsl-shader line-vert
  (version 330 core)
  (layout (location 0) in vec-3 a-pos)
  (uniform mat-4 u-model)
  (uniform mat-4 u-view)
  (uniform mat-4 u-proj)
  (define (main) void
    (set! gl-position (* u-proj (* u-view (* u-model (vec-4 a-pos 1.0)))))))

(glsl-shader line-frag
  (version 330 core)
  (uniform vec-4 u-color)
  (out vec-4 frag-color)
  (define (main) void
    (set! frag-color u-color)))

;; 文字：和普通纹理四边形一样，只是片元里丢弃透明像素
(glsl-shader text-vert
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

(glsl-shader text-frag
  (version 330 core)
  (in vec-2 v-uv)
  (uniform sampler-2d u-tex)
  (uniform vec-4 u-color)
  (out vec-4 frag-color)
  (define (main) void
    (vec-4 tex (texture u-tex v-uv))
    ;; 透明像素直接丢弃，避免透明边缘写深度 / 出现黑边
    (when (< (.w tex) 0.05)
      (discard))
    (set! frag-color
          (vec-4 (* (x tex) (x u-color))
                 (* (y tex) (y u-color))
                 (* (z tex) (z u-color))
                 (* (.w tex) (.w u-color))))))

;; ------------------------------------------------------------
;; 二、几何
;; ------------------------------------------------------------

;; 线框立方体：8 个角 + 12 条边。每条边拆成两个端点，直接 draw-arrays。
(define cube-corners
  (list (vec-3 -0.5 -0.5 -0.5)
        (vec-3  0.5 -0.5 -0.5)
        (vec-3  0.5  0.5 -0.5)
        (vec-3 -0.5  0.5 -0.5)
        (vec-3 -0.5 -0.5  0.5)
        (vec-3  0.5 -0.5  0.5)
        (vec-3  0.5  0.5  0.5)
        (vec-3 -0.5  0.5  0.5)))

(define cube-edges
  '((0 1) (1 2) (2 3) (3 0)       ; 后面
    (4 5) (5 6) (6 7) (7 4)       ; 前面
    (0 4) (1 5) (2 6) (3 7)))     ; 连接前后

(define line-data
  (apply concat-vecs
         (append*
          (for/list ([e (in-list cube-edges)])
            (list (list-ref cube-corners (car e))
                  (list-ref cube-corners (cadr e)))))))

(define line-vertex-count (* 2 (length cube-edges)))

;; 文字四边形：局部 XY 平面，单位大小；UV 的 v=0 对应 bitmap 顶部
(shader-struct text-quad (vec-3 pos) (vec-2 uv))

(define text-quad-verts
  (list (text-quad (vec-3 -0.5  0.5 0.0) (vec-2 0.0 0.0))   ; 左上
        (text-quad (vec-3  0.5  0.5 0.0) (vec-2 1.0 0.0))   ; 右上
        (text-quad (vec-3  0.5 -0.5 0.0) (vec-2 1.0 1.0))   ; 右下
        (text-quad (vec-3 -0.5 -0.5 0.0) (vec-2 0.0 1.0)))) ; 左下

(define text-quad-data
  (apply concat-vecs (map text-quad->f32vector text-quad-verts)))
(define text-quad-indices (u16-array 0 1 2  0 2 3))

;; ------------------------------------------------------------
;; 三、场景数据
;; ------------------------------------------------------------

(struct label (tex center scale color) #:transparent)

(define line-program 0)
(define text-program 0)
;; uniform 位置：链接后查一次（名字用 GLSL 里的写法），之后每帧直接 uniform-set!
(define line-u-model-loc 0)
(define line-u-view-loc 0)
(define line-u-proj-loc 0)
(define line-u-color-loc 0)
(define text-u-model-loc 0)
(define text-u-view-loc 0)
(define text-u-proj-loc 0)
(define text-u-color-loc 0)
(define text-u-tex-loc 0)
(define line-vao 0)
(define text-vao 0)
(define labels '())

;; 三个线框立方体：位置 + 颜色
(define cubes
  (list (list (vec-3 -1.8 0.0 0.0) (vec-4 0.25 0.75 1.0 1.0))
        (list (vec-3  0.0 0.0 0.0) (vec-4 1.0 0.65 0.25 1.0))
        (list (vec-3  1.8 0.0 0.0) (vec-4 0.35 1.0 0.55 1.0))))

;; uniform 位置：链接后查一次（line-* / text-* 两套，避免同名撞）
;; 这些缓冲句柄也放在顶层：关窗时 shutdown 要能拿到（0 = 还没创建）
(define line-vbo 0)
(define text-vbo 0)
(define text-ebo 0)

;; ------------------------------------------------------------
;; 四、初始化
;; ------------------------------------------------------------
(define (init)
  (set! line-program (build-program (gl-vertex-shader line-vert)
                                    (gl-fragment-shader line-frag)))
  (set! line-u-model-loc (uniform-location line-program "u_model"))
  (set! line-u-view-loc (uniform-location line-program "u_view"))
  (set! line-u-proj-loc (uniform-location line-program "u_proj"))
  (set! line-u-color-loc (uniform-location line-program "u_color"))
  (set! text-program (build-program (gl-vertex-shader text-vert)
                                    (gl-fragment-shader text-frag)))
  (set! text-u-model-loc (uniform-location text-program "u_model"))
  (set! text-u-view-loc (uniform-location text-program "u_view"))
  (set! text-u-proj-loc (uniform-location text-program "u_proj"))
  (set! text-u-color-loc (uniform-location text-program "u_color"))
  (set! text-u-tex-loc (uniform-location text-program "u_tex"))

  (gl-enable gl-depth-test)

  ;; ---- 线框立方体 VAO/VBO ----
  (set! line-vao (vertex-array-new))
  (gl-bind-vertex-array line-vao)
  (set! line-vbo (buffer-new))
  (gl-bind-buffer gl-array-buffer line-vbo)
  (buffer-upload gl-array-buffer line-data gl-static-draw)
  (gl-vertex-attrib-pointer 0 3 gl-float #f 0 0)
  (gl-enable-vertex-attrib-array 0)
  (gl-bind-vertex-array 0)

  ;; ---- 文字 quad VAO/VBO/EBO ----
  (set! text-vao (vertex-array-new))
  (gl-bind-vertex-array text-vao)
  (set! text-vbo (buffer-new))
  (gl-bind-buffer gl-array-buffer text-vbo)
  (buffer-upload gl-array-buffer text-quad-data gl-static-draw)
  ;; gl-vertex-attrib-pointer 的参数依次是：location、分量数、分量类型、
  ;; normalized、步长、偏移。pos：location 0、3 个 float、偏移 0；
  ;; uv：location 1、2 个 float、偏移 12；步长 20 字节（一个顶点 5 个 float）。
  (gl-vertex-attrib-pointer 0 3 gl-float #f (text-quad-stride-bytes) (text-quad-field-offset 'pos))
  (gl-enable-vertex-attrib-array 0)
  (gl-vertex-attrib-pointer 1 2 gl-float #f (text-quad-stride-bytes) (text-quad-field-offset 'uv))
  (gl-enable-vertex-attrib-array 1)
  (set! text-ebo (buffer-new))
  (gl-bind-buffer gl-element-array-buffer text-ebo)
  (buffer-upload gl-element-array-buffer text-quad-indices gl-static-draw)
  (gl-bind-vertex-array 0)

  ;; ---- 文字纹理：在 GL 上下文里创建 ----
  ;; 每个标签一张纹理；真实项目里如果文字很多，应改成共享字形 atlas。
  (set! labels
        (list (label (make-text-texture "Racket" #:font-size 64)
                     (vec-3 -1.8 1.0 0.0) 0.45
                     (vec-4 0.65 0.85 1.0 1.0))
              (label (make-text-texture "3D Text" #:font-size 64)
                     (vec-3 0.0 1.0 0.0) 0.45
                     (vec-4 1.0 0.85 0.55 1.0))
              (label (make-text-texture "OpenGL" #:font-size 64)
                     (vec-3 1.8 1.0 0.0) 0.45
                     (vec-4 0.65 1.0 0.75 1.0)))))

;; ------------------------------------------------------------
;; 五、窗口 / 相机
;; ------------------------------------------------------------
(define win-w 800)
(define win-h 600)
(define aspect (/ win-w win-h))

(define (draw)
  (gl-clear-color 0.08 0.10 0.16 1.0)
  (gl-clear (bitwise-ior gl-color-buffer-bit gl-depth-buffer-bit))

  ;; 相机绕场景转：文字用 billboard，所以始终面向相机
  (define t (/ (current-inexact-monotonic-milliseconds) 1000.0))
  (define eye (vec-3 (* 5.0 (cos (* t 0.35)))
                     2.6
                     (* 5.0 (sin (* t 0.35)))))
  (define target (vec-3 0.0 0.5 0.0))
  (define view (m-look-at eye target (vec-3 0.0 1.0 0.0)))
  (define proj (m-perspective (degrees->radians 45.0) aspect 0.1 100.0))

  ;; billboard 基：right / up 都在屏幕平面里，normal 朝向相机
  (define forward (vnormalize (vsub target eye)))
  (define right (vnormalize (vcross forward (vec-3 0.0 1.0 0.0))))
  (define up (vcross right forward))
  (define normal (vcross right up))      ; = -forward，指向相机

  ;; ---- 线框立方体 ----
  (program-use line-program)
  (gl-bind-vertex-array line-vao)
  (for ([cube (in-list cubes)])
    (define pos (car cube))
    (define color (cadr cube))
    (uniform-set! line-u-model-loc 'mat-4 (m-translate pos))
    (uniform-set! line-u-view-loc 'mat-4 view)
    (uniform-set! line-u-proj-loc 'mat-4 proj)
    (uniform-set! line-u-color-loc 'vec-4 color)
    (gl-draw-arrays gl-lines 0 line-vertex-count))

  ;; ---- 文字：透明，放在不透明/线框之后画 ----
  (gl-enable gl-blend)
  (gl-blend-func gl-src-alpha gl-one-minus-src-alpha)
  (gl-depth-mask #f)                     ; 文字不写深度，避免透明边缘互相遮挡

  (program-use text-program)
  (gl-bind-vertex-array text-vao)
  (gl-active-texture gl-texture0)
  (uniform-set! text-u-view-loc 'mat-4 view)
  (uniform-set! text-u-proj-loc 'mat-4 proj)
  (uniform-set! text-u-tex-loc 'sampler-2d 0)

  (for ([l (in-list labels)])
    (define tex (label-tex l))
    (define scale (label-scale l))
    (define sx (* scale (text-texture-aspect tex)))
    ;; 单位 quad 的局部 X 缩放到文字宽高比，Y 缩放到 scale
    (define model
      (mat-4 (vec-4 (* (vx right) sx) (* (vy right) sx) (* (vz right) sx) 0.0)
             (vec-4 (* (vx up) scale) (* (vy up) scale) (* (vz up) scale) 0.0)
             (vec-4 (vx normal) (vy normal) (vz normal) 0.0)
             (vec-4 (vx (label-center l)) (vy (label-center l)) (vz (label-center l)) 1.0)))
    (uniform-set! text-u-model-loc 'mat-4 model)
    (uniform-set! text-u-color-loc 'vec-4 (label-color l))
    (gl-bind-texture gl-texture-2d (text-texture-id tex))
    (gl-draw-elements gl-triangles 6 gl-unsigned-short 0))

  (gl-depth-mask #t)
  (gl-disable gl-blend)
  (gl-bind-vertex-array 0))

;; ------------------------------------------------------------
;; 关窗前释放：这些句柄归调用方，不删就一直占着显存
;; ------------------------------------------------------------
;; 顺序：先解绑当前状态，再删引用方（VAO）→ 缓冲 → 纹理 → program。
;; 初始值是 0：0 不是有效对象，GL 的 delete 会忽略它（init 没跑完也能安全关窗）。
(define (shutdown)
  (gl-bind-vertex-array 0)      ; 别删正在用的
  (gl-use-program 0)
  (vertex-array-delete line-vao)
  (vertex-array-delete text-vao)
  (buffer-delete line-vbo)
  (buffer-delete text-vbo)
  (buffer-delete text-ebo)
  ;; 文字纹理是 init 里用 make-text-texture 建的三张，逐个删
  (for ([l (in-list labels)])
    (delete-text-texture (label-tex l)))
  (program-delete line-program)
  (program-delete text-program))

(define-values (frame canvas)
  (make-window #:title "13 3D 文字"
               #:width win-w
               #:height win-h
               #:init init
               #:draw draw
               #:on-size (lambda (cw ch) (set! aspect (/ cw ch)))
               #:on-close shutdown))
(send frame show #t)

;; 相机在动，需要定时器不断请求重绘
(define timer
  (new timer% [interval 16]
              [notify-callback (lambda () (send canvas refresh))]))
