#lang racket/base
;; ============================================================
;; 复习 01 · API 调用 ↔ 它触发的计算
;; 运行：racket review/01-api-flow/api-review.rkt
;; ============================================================
;; 这不是一个渲染程序，而是一张可打印的复习表：
;; 把前面用过的 API 按「传数据 / 设状态 / 启动计算」重新排一遍。
;;
;; 记住一件事：API 本身不做矩阵乘法、不算光照；
;; 它把数据送到 GPU、设置状态、然后启动 shader。

(require racket/format)

(struct row (api action who direction) #:transparent)

(define rows
  (list
   (row "gl-create-shader / gl-shader-source / gl-compile-shader"
        "创建 shader 对象，把 GLSL 文本交给驱动编译"
        "驱动 / GPU 前端"
        "CPU → GPU")
   (row "gl-create-program / gl-attach-shader / gl-link-program"
        "把多个 shader 链接成一个 program"
        "驱动 / GPU"
        "GPU 内部")
   (row "gl-gen-buffers / gl-bind-buffer / gl-buffer-data"
        "创建 VBO/EBO，把顶点/索引字节拷进显存"
        "CPU 准备数据，GPU 保存"
        "CPU → GPU")
   (row "gl-gen-vertex-arrays / gl-vertex-attrib-pointer"
        "创建 VAO，记录“从哪个 buffer、按什么格式读顶点”"
        "CPU 设状态，GPU 记录"
        "CPU → GPU 状态")
   (row "gl-create-texture / gl-tex-image-2d"
        "创建纹理，把像素字节传到显存"
        "CPU 准备像素，GPU 保存"
        "CPU → GPU")
   (row "gl-use-program"
        "切换当前 program"
        "CPU 设状态"
        "CPU → GPU 状态")
   (row "gl-bind-vertex-array"
        "切换当前 VAO"
        "CPU 设状态"
        "CPU → GPU 状态")
   (row "gl-uniform-* / uniform-set!"
        "把 uniform 值写到当前 program"
        "CPU 传值，GPU 保存"
        "CPU → GPU")
   (row "gl-draw-arrays / gl-draw-elements"
        "启动一次绘制：GPU 跑顶点/片元 shader"
        "GPU 真正计算"
        "GPU → 屏幕")
   (row "gl-clear / gl-enable / gl-blend-func"
        "设清屏色、开关深度/混合等状态"
        "CPU 设状态"
        "CPU → GPU 状态")))

(printf "~a\n\n" "复习 01 · API 调用 ↔ 计算")
(for ([r (in-list rows)])
  (printf "API：~a\n" (row-api r))
  (printf "  动作：~a\n" (row-action r))
  (printf "  谁在算：~a\n" (row-who r))
  (printf "  数据方向：~a\n\n" (row-direction r)))

(printf "一句话：API 只负责“把计算放到 GPU 上并启动它”；公式在 shader 或 CPU 代码里。\n")
