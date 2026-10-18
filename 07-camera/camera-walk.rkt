#lang glsl-opengl
;; ============================================================
;; 07 · 相机（自由相机）：按键状态 + 连续移动
;; 运行：racket 07-camera/camera-walk.rkt
;; 同课对照：07-camera/camera.rkt（相机绕圈版）
;; ============================================================
;; 自由相机 = 三件事：
;;   ① 输入      #:on-key 记录"按下"，#:on-key-up 记录"松开"，维护一个 held 集合
;;   ② 每帧      draw 里读 held 集合，按 dt 更新相机位置/朝向
;;   ③ 朝向      yaw / pitch 两个欧拉角合成 forward
;; 不依赖系统"按键自动重复"：长按立即连续走，W+D 可以斜走，松开明确停止。
;; 这就是 learnopengl 用 glfwGetKey 轮询"哪些键正被按住"的等价物，
;; 只不过 Racket 这边是事件推送，我们自己把按下/松开拼成状态。

(require racket/class
         racket/gui                     ; timer%
         racket/math                    ; degrees->radians
         "../lib/window.rkt"
         "../lib/matrix.rkt")           ; m-look-at / m-translate / m-rotate-x / m-perspective

;; ------------------------------------------------------------
;; 一、窗口尺寸与 aspect
;; ------------------------------------------------------------
(define win-w 800)
(define win-h 600)
;; aspect 必须和窗口实际的宽高比一致，否则画面被拉伸。它做成可变的：窗口被拖成别的
;; 比例时，下面 #:on-size 回调会把它重算（06 课没处理的这个点，在这里补上）。
(define aspect (/ win-w win-h))

;; ------------------------------------------------------------
;; 二、相机状态（模块级、可变）
;; ------------------------------------------------------------
;; pos   相机在世界里的位置
;; yaw   方位角：绕世界 y 轴、在地面平面里转，以世界 +x 为 0、往 +z 一侧增加
;; pitch 俯仰角：抬头为正、低头为负
;; 两个角都用弧度。yaw 起步取 -90°：这时 forward 正好落在 -z 上、朝着原点那堆立方体
;; （公式见第三节；learnopengl 的相机初始朝向也是 -z）。
(define pos (vec-3 0.0 0.0 3.0))
(define yaw (- (/ pi 2.0)))
(define pitch 0.0)

;; 上一次更新相机状态的时刻（毫秒）。每帧 draw 里用它算 dt。
(define last-frame-ms (current-inexact-monotonic-milliseconds))

;; 当前正被按住的键：key -> #t。
;; 按下时写入，松开时删除；draw 每帧只看这个集合，不再依赖系统自动重复。
(define held (make-hash))
(define (held? key) (hash-has-key? held key))
(define (press-key! key) (hash-set! held key #t))
(define (release-key! key) (hash-remove! held key))

;; 每步的基准量：平移每秒几格、转每秒几弧度。真正走多远还要乘 dt（第四节）。
(define move-speed 2.5)
(define turn-speed 1.5)

;; pitch 两端钳在 ±89°。为什么不给 ±90°：到 ±90° 时 forward 竖直、与 up 平行，
;; look-at 里的 f × up 就退化成零向量，归一化它得 NaN，画面整个崩——7.5 让读者亲手
;; 撞过一次的退化，真相机就靠这一行把它挡在门外。
(define pitch-limit (degrees->radians 89.0))

;; ------------------------------------------------------------
;; 三、欧拉角：由 yaw / pitch 算出 forward 与 right
;; ------------------------------------------------------------
;; 把朝向拆成两个角来看：
;;   yaw   在地面平面里转（绕世界 y 轴），决定朝东南西北哪边
;;   pitch 抬头 / 低头，决定朝上看还是朝下看
;; 两个角一合成就是前向单位向量：
;;   forward = (cos pitch · cos yaw, sin pitch, cos pitch · sin yaw)
;; cos pitch 同时缩着地面里的两个分量：越抬头，水平分量越小、竖直分量 sin pitch 越大。
(define (camera-forward)
  (vec-3 (* (cos pitch) (cos yaw))
        (sin pitch)
        (* (cos pitch) (sin yaw))))

;; right 是相机的右手边：forward 与世界的"上" (0,1,0) 叉乘再归一化。
;; 用世界的上而不是相机的 up，是因为 yaw 只在地面平面里转——这样 right 永远躺在
;; 地面上，横移（A/D）才不会把人带得上下漂。
(define (camera-right)
  (vnormalize (vcross (camera-forward) (vec-3 0.0 1.0 0.0))))

;; ------------------------------------------------------------
;; 四、键盘输入：按下/松开维护 held，draw 每帧更新
;; ------------------------------------------------------------
;; Racket 的键盘是事件推送：按下一次送一次，松开一次送一次。
;; 我们不需要"每次事件走一步"，而是把事件翻译成状态：
;;   #:on-key     → (hash-set! held key #t)
;;   #:on-key-up  → (hash-remove! held key)
;; 然后 draw 每帧看 held 里有哪些键，乘 dt 更新相机。
;; 这样长按、组合键（W+D）、松开都自然工作，不依赖操作系统的自动重复。
(define move-keys (list #\w #\s #\a #\d 'left 'right 'up 'down))

(define (on-key-down key)
  (when (memq key move-keys)
    (press-key! key)))

(define (on-key-up key)
  (when (memq key move-keys)
    (release-key! key)))

;; 窗口失去焦点时清空 held：否则按住 W 切走窗口，回来时 held 里还留着 W，
;; 相机会一直往前走（粘键）。
(define (on-focus on?)
  (unless on?
    (hash-clear! held)))

;; draw 每帧调用一次：先算 dt，再按 held 更新位置/朝向。
(define (update-camera)
  (define now (current-inexact-monotonic-milliseconds))
  (define dt (min 0.1 (/ (- now last-frame-ms) 1000.0)))
  (set! last-frame-ms now)

  ;; 先转身，再按新朝向平移：按着 W + ← 时会边走边转。
  (define turn (* turn-speed dt))
  (when (held? 'left)  (set! yaw (- yaw turn)))
  (when (held? 'right) (set! yaw (+ yaw turn)))
  (when (held? 'up)    (set! pitch (min pitch-limit (+ pitch turn))))
  (when (held? 'down)  (set! pitch (max (- pitch-limit) (- pitch turn))))

  (define fwd (camera-forward))
  (define right (camera-right))
  (define step (* move-speed dt))
  (when (held? #\w) (set! pos (vadd pos (vmul fwd step))))
  (when (held? #\s) (set! pos (vsub pos (vmul fwd step))))
  (when (held? #\a) (set! pos (vsub pos (vmul right step))))
  (when (held? #\d) (set! pos (vadd pos (vmul right step)))))

;; ------------------------------------------------------------
;; 五、棋盘纹理（同 04 课：用代码生成，不依赖图片文件）
;; ------------------------------------------------------------
(define tex-size 64)
(define tex-cell 8)

(define (make-checker-texture size cell)
  (define px (make-u8-array (* size size 4) 255))
  (for* ([y (in-range size)] [x (in-range size)])
    (define on? (even? (+ (quotient x cell) (quotient y cell))))
    (define i (* (+ (* y size) x) 4))
    (define r (if on? 230 40))
    (define g (if on? 140 70))
    (define b (if on? 60 130))
    (u8-array-set! px i r)
    (u8-array-set! px (+ i 1) g)
    (u8-array-set! px (+ i 2) b)
    (u8-array-set! px (+ i 3) 255))          ; alpha 不透明
  px)

;; ------------------------------------------------------------
;; 六、着色器：三个矩阵串起来（同 camera.rkt）
;; ------------------------------------------------------------
;; 顶点连着过三道变换，写成矩阵就是从右往左乘：
;;   model：把单位立方体摆到世界里的某处
;;   view ：把世界换进相机坐标——这一次它由 pos / yaw / pitch 算出来
;;   proj ：把相机看到的视锥投影到 NDC
(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec-3 a-pos)
        (layout (location 1) in vec-2 a-uv)
        (uniform mat-4 u-model)
        (uniform mat-4 u-view)
        (uniform mat-4 u-proj)
        (out vec-2 v-uv)
        (define (main) void
          (set! gl-position (* u-proj (* u-view (* u-model (vec-4 a-pos 1.0)))))
          (set! v-uv a-uv))))

(define frag-src
  (glsl (version 330 core)
        (in vec-2 v-uv)
        (uniform sampler-2d u-tex)
        (out vec-4 frag-color)
        (define (main) void
          (set! frag-color (texture u-tex v-uv)))))

;; ------------------------------------------------------------
;; 七、立方体数据：24 个顶点 + 36 索引 + 位置表（同 camera.rkt）
;; ------------------------------------------------------------
;; 立方体 6 个面、每面 4 个角，共 24 个顶点。8 个角点本来可以共用，
;; 但每个面都要贴自己完整的一张纹理，UV 各不相同，所以按面拆开。
;; 尺寸 ±0.5（边长 1），index 每面 6 个（两个三角形），共 36 个。
(define vertex-data
  (concat-vecs
   ;; 后面 z = -0.5
   (vec-3 -0.5 -0.5 -0.5) (vec-2 0.0 0.0)
   (vec-3  0.5 -0.5 -0.5) (vec-2 1.0 0.0)
   (vec-3  0.5  0.5 -0.5) (vec-2 1.0 1.0)
   (vec-3 -0.5  0.5 -0.5) (vec-2 0.0 1.0)
   ;; 前面 z = +0.5
   (vec-3 -0.5 -0.5  0.5) (vec-2 0.0 0.0)
   (vec-3  0.5 -0.5  0.5) (vec-2 1.0 0.0)
   (vec-3  0.5  0.5  0.5) (vec-2 1.0 1.0)
   (vec-3 -0.5  0.5  0.5) (vec-2 0.0 1.0)
   ;; 左面 x = -0.5
   (vec-3 -0.5  0.5  0.5) (vec-2 1.0 0.0)
   (vec-3 -0.5  0.5 -0.5) (vec-2 1.0 1.0)
   (vec-3 -0.5 -0.5 -0.5) (vec-2 0.0 1.0)
   (vec-3 -0.5 -0.5  0.5) (vec-2 0.0 0.0)
   ;; 右面 x = +0.5
   (vec-3  0.5  0.5  0.5) (vec-2 1.0 0.0)
   (vec-3  0.5  0.5 -0.5) (vec-2 1.0 1.0)
   (vec-3  0.5 -0.5 -0.5) (vec-2 0.0 1.0)
   (vec-3  0.5 -0.5  0.5) (vec-2 0.0 0.0)
   ;; 底面 y = -0.5
   (vec-3 -0.5 -0.5 -0.5) (vec-2 0.0 1.0)
   (vec-3  0.5 -0.5 -0.5) (vec-2 1.0 1.0)
   (vec-3  0.5 -0.5  0.5) (vec-2 1.0 0.0)
   (vec-3 -0.5 -0.5  0.5) (vec-2 0.0 0.0)
   ;; 顶面 y = +0.5
   (vec-3 -0.5  0.5 -0.5) (vec-2 0.0 1.0)
   (vec-3  0.5  0.5 -0.5) (vec-2 1.0 1.0)
   (vec-3  0.5  0.5  0.5) (vec-2 1.0 0.0)
   (vec-3 -0.5  0.5  0.5) (vec-2 0.0 0.0)))

(define indices
  (u16-array 0 3 2   2 1 0          ; 后面
             4 5 6   6 7 4          ; 前面
             8 9 10  10 11 8        ; 左面
             12 14 13 14 12 15      ; 右面
             16 17 18 18 19 16      ; 底面
             20 22 21 22 20 23))    ; 顶面

;; 十个立方体在世界里的位置（同 camera.rkt 的散布法：近处零星几个、远处一大片）。
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

(define program #f)
(define vao #f)
(define texture #f)

(define (init)
  (set! program (build-program (gl-vertex-shader vert-src)
                               (gl-fragment-shader frag-src)))

  ;; ---- 棋盘纹理（同 04 课）----
  (set! texture (texture-new))
  (gl-bind-texture gl-texture-2d texture)
  (gl-tex-parameter-i gl-texture-2d gl-texture-wrap-s gl-repeat)
  (gl-tex-parameter-i gl-texture-2d gl-texture-wrap-t gl-repeat)
  (gl-tex-parameter-i gl-texture-2d gl-texture-min-filter gl-linear-mipmap-linear)
  (gl-tex-parameter-i gl-texture-2d gl-texture-mag-filter gl-linear)
  (gl-tex-image-2d gl-texture-2d 0 gl-rgba tex-size tex-size 0
                   gl-rgba gl-unsigned-byte (make-checker-texture tex-size tex-cell))
  (gl-generate-mipmap gl-texture-2d)

  (program-use program)
  (uniform-set! (uniform-location program "u_tex") 'sampler-2d 0)
  (gl-active-texture gl-texture0)
  (gl-bind-texture gl-texture-2d texture)

  ;; ---- VAO / VBO / EBO ----
  (set! vao (vertex-array-new))
  (gl-bind-vertex-array vao)

  (define vbo (buffer-new))
  (gl-bind-buffer gl-array-buffer vbo)
  (buffer-upload gl-array-buffer vertex-data gl-static-draw)
  ;; 步长和偏移都从布局算出来，不写字面量。
  (define stride (shader-stride-bytes 'vec-3 'vec-2))   ; 3 + 2 = 5 个 float = 20 字节
  (define uv-offset (shader-byte-size 'vec-3))         ; 跳过位置那 3 个 float = 12 字节
  (gl-vertex-attrib-pointer 0 3 gl-float #f stride 0)
  (gl-enable-vertex-attrib-array 0)
  (gl-vertex-attrib-pointer 1 2 gl-float #f stride uv-offset)
  (gl-enable-vertex-attrib-array 1)

  (define ebo (buffer-new))
  (gl-bind-buffer gl-element-array-buffer ebo)
  (buffer-upload gl-element-array-buffer indices gl-static-draw)

  (gl-bind-vertex-array 0)

  ;; 画立体的东西就得开深度测试，否则后画的近处立方体会盖住先画的远处立方体。
  (gl-enable gl-depth-test))

(define (draw)
  ;; 每帧先根据 held 集合更新相机，再算 view / proj 并绘制。
  (update-camera)

  (gl-clear-color 0.10 0.12 0.20 1.0)
  ;; 掩码按位或：颜色和深度一起清。
  (gl-clear (bitwise-ior gl-color-buffer-bit gl-depth-buffer-bit))

  ;; ---- 相机：eye 是键盘改过的 pos，看向"自己前方一格"的点 ----
  ;; center 取 pos + forward，朝哪看完全由 yaw / pitch 说了算。
  ;; up 仍是世界的 (0,1,0)：相机不侧倾，头顶永远朝世界正上方。
  (define view (m-look-at pos (vadd pos (camera-forward)) (vec-3 0.0 1.0 0.0)))

  ;; 投影同 06：竖直视场 45°、aspect 跟着窗口（#:on-size 同步）、近远平面 0.1 / 100。
  (define proj (m-perspective (degrees->radians 45.0) aspect 0.1 100.0))

  (program-use program)
  (gl-bind-vertex-array vao)

  ;; view / proj 整帧只算一份，循环外上传一次；model 每个立方体不同，得在循环里传。
  (uniform-set! (uniform-location program "u_view") 'mat-4 view)
  (uniform-set! (uniform-location program "u_proj") 'mat-4 proj)

  ;; model 同 camera.rkt：先让立方体转到自己的姿态，再挪到位置；各立方体随时间自转，
  ;; 于是画面本身也一直在动（相机不动时也有重绘可看）。
  (define t (/ (current-inexact-monotonic-milliseconds) 1000.0))
  (for ([cube-pos (in-list cube-positions)])
    (define model (mat4-mul (m-translate cube-pos)
                            (mat4-mul (m-rotate-x (* t 0.5))
                                      (m-rotate-y t))))
    (uniform-set! (uniform-location program "u_model") 'mat-4 model)
    (gl-draw-elements gl-triangles 36 gl-unsigned-short 0))

  (gl-bind-vertex-array 0))

(define-values (frame canvas)
  (make-window #:title "07 相机（连续移动）"
               #:width win-w
               #:height win-h
               #:init init
               #:draw draw
               #:on-key on-key-down
               #:on-key-up on-key-up
               #:on-focus on-focus
               ;; 窗口尺寸变了 → 重设 viewport 之后回调这里，把 aspect 一起改掉
               #:on-size (lambda (cw ch) (set! aspect (/ cw ch)))))
(send frame show #t)
;; canvas 得先拿到键盘焦点才收得到按键——这行不能少，且 focus 不带参数
;; （写 (send canvas focus #t) 会报 arity 错）。
(send canvas focus)

;; 定时器只负责"每隔 16ms 请求重绘"，绘制永远发生在 draw 里（同 01 课）。
(define timer
  (new timer% [interval 16]
              [notify-callback (lambda () (send canvas refresh))]))

;; ------------------------------------------------------------
;; 试试看（改完重跑）：
;;   · 把 on-key-up 里的 (release-key! key) 注释掉：松开后相机会一直走，
;;     直观看到"只记录按下、不记录松开"的粘键效果——改回来即好
;;   · 按住 W + D：斜着走
;;   · 鼠标转视角：canvas 还能接 on-event 拿到鼠标移动事件，把两次事件的位移喂给
;;     yaw / pitch（pitch 再钳一下）就是第一人称的鼠标视角——思路到此，不展开
;;   · pitch-limit 改成 (degrees->radians 90.0)：抬头到顶，forward 与 up 平行，
;;     f × up = 0，画面崩成 NaN（就是 7.5 那个退化）——改回来即好
;;   · move-speed 从 2.5 改成 25.0：按住 W 一秒就穿出立方体群
;; ============================================================