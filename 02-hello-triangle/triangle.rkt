#lang glsl-opengl
;; ============================================================
;; 02 · 第一个三角形
;; 运行：racket 02-hello-triangle/triangle.rkt
;; 上一课：01-window/window.rkt
;; ============================================================
;; 目标：把三个顶点变成屏幕上的三角形。顺序是
;;   顶点数据 → VBO（显存）→ 两段着色器 → program → VAO → 发绘制命令
;; 下面按这个顺序写，走到哪讲到哪。
;;
;; 阅读顺序：先看教程 2.0 的 raw OpenGL 流程（以及 compare/raw-triangle.rkt），
;; 理解 shader 编译、buffer 上传、VAO 属性描述分别对应哪些 gl-* 调用；
;; 本文件为了聚焦概念，使用 racket-shader-opengl 的便利层（build-program / buffer-new …）。

(require racket/class
         "../lib/window.rkt")

;; 三个顶点。一个顶点至少要有位置；vec-3 = 三个 float = (x y z)。
;; 坐标是标准坐标（-1..1，中心 0）。vec-array 把三个顶点打包成一条连续缓冲。
(define vertices
  (vec-array (vec-3 -0.5 -0.5 0.0)     ; 左下
          (vec-3  0.5 -0.5 0.0)     ; 右下
          (vec-3  0.0  0.5 0.0)))   ; 上中

;; 顶点着色器：每个顶点跑一次，必须给 gl-position 赋值——顶点最终落在哪。
(define vert-src
  (glsl (version 330 core)
        ;; layout(location = 0) 表示"从 0 号属性槽读"，下面描述顶点格式时要用同一个 0
        (layout (location 0) in vec-3 a-pos)
        (define (main) void
          (set! gl-position (vec-4 a-pos 1.0)))))   ; 补 w=1，凑成 vec-4

;; 片元着色器：每个像素跑一次，必须给 out 变量赋值——像素什么颜色。
(define frag-src
  (glsl (version 330 core)
        (out vec-4 frag-color)
        (define (main) void
          (set! frag-color (vec-4 1.0 0.5 0.2 1.0)))))  ; 橙色

(define program 0)   ; 编译链接好的着色器程序
(define vao 0)       ; "说明书"：记录顶点数据怎么读
;; 这些缓冲句柄也放在顶层：关窗时 shutdown 要能拿到（0 = 还没创建）
(define vbo 0)

;; init 只跑一次（窗口第一次绘制、GL 上下文就绪后）。
(define (init)

  ;; ① 编译两段着色器并链接成一个 program。
  ;;    racket-shader 的工具：自动查编译/链接状态，失败时给出报错行。
  (set! program (build-program (gl-vertex-shader vert-src)
                               (gl-fragment-shader frag-src)))

  ;; ② 把顶点数据搬进显存。
  ;;    这一步只依赖 gl-array-buffer 这个绑定，和 VAO 无关（VAO 没绑也能上传）。
  ;;    gl-gen-buffers(n)：生成 n 个缓冲，返回装 id 的向量；
  ;;    裸写法： (u32vector-ref (gl-gen-buffers 1) 0)
  ;;    buffer-new：取单个缓冲 id（= glGenBuffers(1) 的便利封装）。
  (set! vbo (buffer-new))
  ;; gl-bind-buffer(目标, id)：把这块缓冲绑到“顶点数据”目标上。
  ;;   gl-array-buffer 是 OpenGL 内置常量（C 里叫 GL_ARRAY_BUFFER），表示顶点数据用途。
  ;;   其他缓冲目标（UBO/SSBO/像素缓冲…）和用法常量见教程 2.2 的表。
  (gl-bind-buffer gl-array-buffer vbo)
  ;; buffer-upload(目标, 数据, 用法)：申请空间并把数据拷进显存。
  ;;   数据直接给 vec-array/ffi 向量；gl-static-draw 表示基本不变。
  ;;   裸写法： (gl-buffer-data gl-array-buffer
  ;;                            (gl-vector-sizeof data)   ; 字节数
  ;;                            data                      ; 裸 ffi 向量
  ;;                            gl-static-draw)
  ;;   —— 需先 (vec-array-data vertices) 取 data、再算字节；buffer-upload 把这两步包起来。
  (buffer-upload gl-array-buffer vertices gl-static-draw)

  ;; ③ 建 VAO 并绑定，然后描述“怎么读”。这两步才依赖 VAO。
  ;;    VAO = 顶点数组对象，是“数据说明书”，记下每个属性槽怎么从缓冲里读。
  (set! vao (vertex-array-new))
  (gl-bind-vertex-array vao)
  ;; gl-vertex-attrib-pointer(槽号, 分量数, 类型, 归一化, 步长, 偏移)
  ;;   = 0 号槽 / 每顶点 3 个分量 / 类型 float / 不归一化 / 紧密排列(0) / 从 0 开始
  ;;   ★它没有“缓冲”参数，抓的是当前绑在 gl-array-buffer 上的那块（即②的 vbo，仍绑着）。
  (gl-vertex-attrib-pointer 0 3 gl-float #f 0 0)
  ;; 属性槽默认关闭，必须打开；不打开时 shader 读到的是常量 (0,0,0,1)。
  (gl-enable-vertex-attrib-array 0)

  (gl-bind-vertex-array 0))   ; 解绑，避免后面误改

;; 【哪些东西属于 VAO】
;;   VAO 状态：每个属性的 开关/格式/源 VBO 引用；ELEMENT_ARRAY_BUFFER 绑定。
;;   全局状态（不属于 VAO）：ARRAY_BUFFER 绑定、当前 program、viewport 等。
;;   所以上传数据不依赖 VAO；只有 attrib-pointer / enable / 绑 EBO 才依赖。

;; draw 每帧跑一次。这里只是"挂上要用的东西，然后画"。
(define (draw)
  (gl-clear-color 0.10 0.12 0.20 1.0)   ; 设清屏色（只记住，不画）
  (gl-clear gl-color-buffer-bit)        ; 用记住的颜色擦掉颜色缓冲

  (program-use program)             ; 选程序
  (gl-bind-vertex-array vao)        ; 挂上说明书
  ;; gl-draw-arrays(图元, 起始顶点, 顶点数)：触发整条管线
  (gl-draw-arrays gl-triangles 0 3) ; 第 0 个顶点起画 3 个，每 3 个一组三角形
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
  (program-delete program))

(define-values (frame canvas)
  (make-window #:title "02 第一个三角形"
               #:init init
               #:draw draw
               #:on-close shutdown))
(send frame show #t)

;; 复盘：VBO 存数据，VAO 记"怎么读"，program 是 GPU 上的程序，draw 用它们画图。
;; 规律：先绑，再操作；尺寸用字节数；槽号两边要对齐，属性槽要先 enable。
