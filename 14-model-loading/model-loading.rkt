#lang glsl-opengl
;; ============================================================
;; 14 · 模型加载：从 OBJ 文件到 GPU
;; 运行：racket 14-model-loading/model-loading.rkt
;; 上一课：13-text-3d/text-3d.rkt
;; ============================================================
;; 目标：不再手写顶点数据，而是从 assets/models/teapot.obj 读一个模型。
;;
;; 这是经典的 Utah Teapot：1202 个顶点 / 2256 个三角形，带法线，
;; 但没有 UV。用它正好说明：模型加载不只是“读位置”，还要处理法线，
;; 而 UV / 材质是另一层数据，缺了也能先用纯材质色 + 光照渲染。
;;
;; OBJ 文件里常见三张表：
;;   v  顶点位置
;;   vt UV
;;   vn 法线
;; 面用 f 引用它们：f v/vt/vn ...
;;
;; CPU 侧要做的事：
;;   1. 解析 OBJ（lib/obj.rkt）
;;   2. 把 obj-vertex 转成渲染用的交错顶点（model-vertex）
;;   3. 上传 VBO + EBO
;;   4. 用 model / view / proj / normal-matrix 画出来
;;
;; 想加载别的 OBJ：把路径换成自己的文件即可；复杂模型还需要材质、
;; 多 group、索引去重优化等，见页面最后的“拓展方向”。

(require racket/class
         racket/gui                     ; timer%
         racket/math                    ; degrees->radians
         "../lib/window.rkt"
         "../lib/matrix.rkt"
         "../lib/obj.rkt")

;; ------------------------------------------------------------
;; 一、读 OBJ
;; ------------------------------------------------------------
(define mesh (read-obj (build-path (current-directory)
                                   "assets/models/teapot.obj")))

;; 渲染用顶点布局：位置 + 法线。
;; 字段名只是 CPU 侧的标签；和 shader 的 in 对上靠 init 里手写的 location。
;; （loader 仍然支持 UV；这个 teapot 文件没有 vt，所以这里不需要。）
(shader-struct model-vertex (vec-3 pos) (vec-3 normal))

(define model-data
  (apply concat-vecs
         (for/list ([v (in-vector (obj-mesh-vertices mesh))])
           (model-vertex->f32vector
            (model-vertex (obj-vertex-pos v)
                          (obj-vertex-normal v))))))

(define model-indices
  (apply u16-array (vector->list (obj-mesh-indices mesh))))

(define model-index-count (obj-mesh-index-count mesh))

;; ------------------------------------------------------------
;; 二、着色器：纯材质色 + 简单 Phong
;; ------------------------------------------------------------
(glsl-shader model-vert
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

(glsl-shader model-frag
  (version 330 core)
  (in vec-3 v-normal)
  (in vec-3 v-frag-pos)
  (uniform vec-3 u-object-color)
  (uniform vec-3 u-light-pos)
  (uniform vec-3 u-light-color)
  (uniform vec-3 u-view-pos)
  (out vec-4 frag-color)
  (define (main) void
    (vec-3 norm (normalize v-normal))
    (vec-3 light-dir (normalize (- u-light-pos v-frag-pos)))
    (vec-3 view-dir (normalize (- u-view-pos v-frag-pos)))
    (vec-3 reflect-dir (reflect (- light-dir) norm))

    (float ambient 0.15)
    (float diff (max (dot norm light-dir) 0.0))
    (float spec (* (pow (max (dot view-dir reflect-dir) 0.0) 32.0) 0.4))
    (vec-3 lit (* (+ ambient diff spec) u-light-color))
    (set! frag-color (vec-4 (* lit u-object-color) 1.0))))

;; ------------------------------------------------------------
;; 三、GL 对象
;; ------------------------------------------------------------
(define model-program 0)
;; uniform 位置：链接后查一次（名字用 GLSL 里的写法），之后每帧直接 uniform-set!
(define model-u-model-loc 0)
(define model-u-view-loc 0)
(define model-u-proj-loc 0)
(define model-u-normal-matrix-loc 0)
(define model-u-object-color-loc 0)
(define model-u-light-pos-loc 0)
(define model-u-light-color-loc 0)
(define model-u-view-pos-loc 0)
(define vao 0)
;; 这些缓冲句柄也放在顶层：关窗时 shutdown 要能拿到（0 = 还没创建）
(define vbo 0)
(define ebo 0)

(define (init)
  (set! model-program (build-program (gl-vertex-shader model-vert)
                                     (gl-fragment-shader model-frag)))
  (set! model-u-model-loc (uniform-location model-program "u_model"))
  (set! model-u-view-loc (uniform-location model-program "u_view"))
  (set! model-u-proj-loc (uniform-location model-program "u_proj"))
  (set! model-u-normal-matrix-loc (uniform-location model-program "u_normal_matrix"))
  (set! model-u-object-color-loc (uniform-location model-program "u_object_color"))
  (set! model-u-light-pos-loc (uniform-location model-program "u_light_pos"))
  (set! model-u-light-color-loc (uniform-location model-program "u_light_color"))
  (set! model-u-view-pos-loc (uniform-location model-program "u_view_pos"))

  (gl-enable gl-depth-test)

  ;; ---- 模型 VAO / VBO / EBO ----
  (set! vao (vertex-array-new))
  (gl-bind-vertex-array vao)

  (set! vbo (buffer-new))
  (gl-bind-buffer gl-array-buffer vbo)
  (buffer-upload gl-array-buffer model-data gl-static-draw)
  ;; shader 的 in 名 = model-vertex 字段名：pos / normal / uv
  ;; 顶点属性：手写 location / 分量 / 类型 / 步长 / 偏移（pos 0、normal 12）
  (gl-vertex-attrib-pointer 0 3 gl-float #f (model-vertex-stride-bytes) (model-vertex-field-offset 'pos))
  (gl-enable-vertex-attrib-array 0)
  (gl-vertex-attrib-pointer 1 3 gl-float #f (model-vertex-stride-bytes) (model-vertex-field-offset 'normal))
  (gl-enable-vertex-attrib-array 1)

  (set! ebo (buffer-new))
  (gl-bind-buffer gl-element-array-buffer ebo)
  (buffer-upload gl-element-array-buffer model-indices gl-static-draw)

  (gl-bind-vertex-array 0))

;; ------------------------------------------------------------
;; 五、窗口 / 绘制
;; ------------------------------------------------------------
(define win-w 800)
(define win-h 600)
(define aspect (/ win-w win-h))

(define (draw)
  (gl-clear-color 0.08 0.10 0.16 1.0)
  (gl-clear (bitwise-ior gl-color-buffer-bit gl-depth-buffer-bit))

  (define t (/ (current-inexact-monotonic-milliseconds) 1000.0))
  (define eye (vec-3 4.5 3.0 6.5))
  (define view (m-look-at eye (vec-3 0.0 1.2 0.0) (vec-3 0.0 1.0 0.0)))
  (define proj (m-perspective (degrees->radians 45.0) aspect 0.1 100.0))

  ;; 模型自己绕 y 轴转，方便看背面和侧面
  (define model-m
    (mat4-mul (m-scale (vec-3 0.8 0.8 0.8))
              (m-rotate-y t)))

  (program-use model-program)
  (gl-bind-vertex-array vao)

  (uniform-set! model-u-model-loc 'mat-4 model-m)
  (uniform-set! model-u-view-loc 'mat-4 view)
  (uniform-set! model-u-proj-loc 'mat-4 proj)
  (uniform-set! model-u-normal-matrix-loc 'mat-3 (m-normal-matrix model-m))
  (uniform-set! model-u-object-color-loc 'vec-3 (vec-3 0.82 0.55 0.35))
  (uniform-set! model-u-light-pos-loc 'vec-3 (vec-3 4.0 5.0 4.0))
  (uniform-set! model-u-light-color-loc 'vec-3 (vec-3 1.0 1.0 1.0))
  (uniform-set! model-u-view-pos-loc 'vec-3 eye)

  (gl-draw-elements gl-triangles model-index-count gl-unsigned-short 0)
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
  (program-delete model-program))

(define-values (frame canvas)
  (make-window #:title "14 模型加载"
               #:width win-w
               #:height win-h
               #:init init
               #:draw draw
               #:on-size (lambda (cw ch) (set! aspect (/ cw ch)))
               #:on-close shutdown))
(send frame show #t)

;; 模型在转，需要定时器不断请求重绘
(define timer
  (new timer% [interval 16]
              [notify-callback (lambda () (send canvas refresh))]))
