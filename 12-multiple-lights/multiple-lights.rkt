#lang glsl-opengl
;; ============================================================
;; 12 · 多光源：把"一盏点光"变成一个数组
;; 运行：racket 12-multiple-lights/multiple-lights.rkt
;; 上一课：11-light-casters/spot.rkt
;; ============================================================
;; 11 课的三份文件讲的都是"一盏灯"：position、color，再加三个衰减系数，
;; 五个散着的 uniform。要摆四盏这样的灯，最直接的办法是把这套名字抄四遍——
;; u-light0-pos / u-light0-color / u-light0-kc / …，二十来个 uniform，加一盏灯就要同时改
;; shader 和上传代码两处，还容易漏一处。
;;
;; 本节换一种写法，用的是 GLSL 本来就有的两件东西：
;;   struct Light { vec-3 pos; vec-3 color; }   把"一盏灯"变成一个值
;;   uniform Light[4] u-lights;                把"几盏灯"变成一个数
;; 于是片元里第一次出现循环：绕着数组走一圈，把每盏灯的 diffuse + specular 各算一份
;; 累加起来。循环体不是新公式——它就是 11 课 point.rkt 那三行，只是"那盏灯"从
;; u-light-pos / u-light-color 换成了 u-lights[i]。
;;
;; 代价也是真的：这个循环在每个片元里都要跑四遍。1 盏灯换成 4 盏灯，不是"多写点代码"，
;; 而是每帧多出三倍的逐像素光照计算——多光源唯一的真实成本（页面上 12.4 节算过）。
;;
;; 顺带补上 11 课欠下的一件事：环境光。那里只有一盏灯，ambient 干脆是个常数 0.1；
;; 灯多了以后它该算谁的？本节取"环境光是所有光的残余"这个说法：把四盏灯的颜色加起来
;; 乘一个小的系数（片元里那行 += ambient …）。这是简化，不是物理，注释里再说一遍。
;;
;; 光怎么动：四盏灯在半径 3、高 1.2 的水平圆上绕 y 轴公转，相位各差 π/2，
;; 于是四个颜色池在立方体间一直流动，一眼看得出哪块亮的是哪盏灯。

(require racket/class
         racket/gui                      ; timer%
         racket/math                     ; degrees->radians / pi / cos / sin
         "../lib/window.rkt"
         "../lib/gpu.rkt"
         "../lib/matrix.rkt")            ; m-look-at / m-perspective / m-normal-matrix / …

;; ------------------------------------------------------------
;; 一、窗口尺寸与 aspect（同 06 / 09 / 11 课）
;; ------------------------------------------------------------
(define win-w 800)
(define win-h 600)
(define aspect (/ win-w win-h))

;; ------------------------------------------------------------
;; 二、着色器
;; ------------------------------------------------------------
;; 顶点着色器与 11 课逐行相同：本课动的全在片元里。
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

;; 物体的片元：本课的全部内容。
;;
;; struct 把"一盏灯要哪些量"写在一个地方：位置和颜色。GLSL 的 uniform 数组只认
;; 类型，所以想让数组里装"灯"，先得有一个"灯"的类型——这就是 struct 在这里的用处。
;;
;; 数组大小 4 是编译期常量：u-lights[4] 和下面 for 的上界、以及 CPU 侧生成几盏灯，
;; 三处必须一致（"试试看"第 ① 条就是同时改这三处）。
;;
;; 循环体里每条语句右边都带 u-lights[i]，逐字对上 11 课 point.rkt 的那几行即可：
;;   light-dir / distance / attenuation  → 这一个元素的距离与衰减
;;   diff / spec 累加                   → 这一盏灯送来的光，加到总数里
;;   ambient 累加                       → 环境光是"所有光的残余"，见下面的注释
;; 取数组元素用 (aref u-lights i)，取字段用 (.pos …) / (.color …)：前者翻成 u-lights[i]，
;; 后者翻成 u-lights[i].pos，正是 GLSL 里读 uniform 数组元素该有的样子（页面上贴了真文本）。
;;
;; （这里用 Racket 风格的连字符；racket-shader 会换成 GLSL 的 _，见 09 课的说明。）
(glsl-shader frag-src
  (version 330 core)
        (in vec-3 v-normal)
        (in vec-3 v-frag-pos)
        (struct Light (vec-3 pos) (vec-3 color))
        (uniform (array Light 4) u-lights)
        (uniform float u-kc)                    ; 衰减的三个系数四盏灯共用
        (uniform float u-kl)
        (uniform float u-kq)
        (uniform vec-3 u-view-pos)
        (uniform vec-3 u-object-color)
        (out vec-4 frag-color)
        (define (main) void
          (vec-3 norm (normalize v-normal))
          (vec-3 view-dir (normalize (- u-view-pos v-frag-pos)))
          ;; 三个累加器：四盏灯各算一份，加起来才是这一点的最终光照。
          ;; 11 课这三项是一个常数表达式，本课每一项都由循环攒出来。
          ;; 三项都是 vec-3 而不是 float——因为每盏灯的贡献要乘它自己的颜色（见循环里）。
          (vec-3 ambient (vec-3 0.0 0.0 0.0))
          (vec-3 diff (vec-3 0.0 0.0 0.0))
          (vec-3 spec (vec-3 0.0 0.0 0.0))
          (for (int i 0) (< i 4) (++ i)
            ;; —— 这一段是 11 课 point.rkt 的逐片元计算，只差最后乘颜色那一步 ——
            (vec-3 light-dir (normalize (- (.pos (aref u-lights i)) v-frag-pos)))
            (float distance (length (- (.pos (aref u-lights i)) v-frag-pos)))
            ;; 衰减：kc 常数项贴着灯保底、kl 管中段、kq 管远段（见 11.2 的曲线）。
            (float attenuation (/ 1.0 (+ u-kc (* u-kl distance) (* u-kq distance distance))))
            (float diff-factor (max (dot norm light-dir) 0.0))
            (vec-3 reflect-dir (reflect (- light-dir) norm))
            (float spec-factor (pow (max (dot view-dir reflect-dir) 0.0) 32.0))
            ;; 每盏灯的贡献要乘"这盏灯"的颜色——11 课只有一盏灯，光色是在最后一步
            ;; 统一乘上去的；灯一多就必须在循环里逐盏乘，因为四盏灯颜色各不相同。
            ;; 这一步最容易漏：漏掉它，四盏灯送来的就都是白光，四个颜色池一起变成灰白。
            ;; 它们只累加、不重置：四盏灯的光是相加的，这正是"多光源"的含义。
            (+= diff (* (* diff-factor attenuation) (.color (aref u-lights i))))
            (+= spec (* (* (* spec-factor 0.5) attenuation) (.color (aref u-lights i))))
            ;; ambient：环境光不是某一盏灯给的，而是所有光在场景里弹来弹去之后剩下的
            ;; 那层底光。精确算它得做全局光照，这里用一个便宜的替代：每盏灯往底光里
            ;; 添一份自己的颜色，四盏加起来乘 0.05——灯多、颜色杂，底光就更亮更暖。
            ;; 它不乘 attenuation：底光不是从灯的位置直接走过来的，见 11 课同一个注释。
            (+= ambient (* 0.05 (.color (aref u-lights i)))))
          ;; 最后一步与 08 / 09 / 11 课相同：物色。光色已经在循环里逐盏乘过了。
          ;; 物体是浅灰的（三个分量相等），所以它几乎不改灯的颜色——四个池子是什么
          ;; 颜色，就是哪盏灯照的；换成橙色物体，所有池子会被压成橙的深浅（见"试试看"②）。
          (set! frag-color (vec-4 (* (+ ambient diff spec) u-object-color) 1.0))))

;; ------------------------------------------------------------
;; 三、立方体几何：24 个顶点，每个带位置 + 法线（同 09 / 11 课）
;; ------------------------------------------------------------
;; 同一个立方体、同样的 24 顶点 / 36 索引：每个面摊自己的一份顶点，因为每个面的
;; 法线不同（前面 (0,0,1)、右面 (1,0,0)…）。布局仍用 shader-struct 镜像到 CPU 侧，
;; 步长与字段偏移全部由字段表给出，并在编译期按名字对齐（glsl-vertex-layout!），一个字节字面量都没有。
;; （本课不需要 UV，所以属性只有位置和法线。）
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
;; 四、场景：一圈立方体 + 四盏绕圈的点光 + 一台不动的相机
;; ------------------------------------------------------------
;; 物体本色：08 / 09 / 10 / 11 课一直是橙，本课换成浅灰，原因就在最后那一步乘法上。
;; 颜色进公式是"灯色 × 物色"（08 课）：橙色物体的红绿蓝三分量是 (1.0, 0.5, 0.31)，
;; 蓝光 (0.2, 0.2, 1.0) 打上去，蓝分量又被 0.31 一乘，那块就没法比红绿分量亮——
;; 四个颜色池会一起变成橙的深浅，分不出谁是谁。灰物体的三个分量相等，
;; 灯是什么颜色，池子就是什么颜色：这一课要看的是"哪盏灯照到了这里"。
;; 想看橙物体怎么把颜色统一压下去，把这一行改回去就行（见"试试看"）。
(define object-color (vec-3 0.8 0.8 0.8))

;; 四盏灯的颜色：红、绿、蓝、橙。前三个都取"偏纯"的写法（只留一点别的分量，
;; 免得池子发白），第四盏橙。
;; 池子的明暗差别一半来自各自的距离（衰减），一半来自颜色本身：
;; 橙光 (1.0, 0.6, 0.1) 三个分量都高，整块就亮；蓝光 (0.2, 0.2, 1.0) 只有蓝分量，
;; 乘出来那块的亮度天然低一截。
(define light-colors
  (list (vec-3 1.0 0.2 0.2)     ; 红
        (vec-3 0.2 1.0 0.2)     ; 绿
        (vec-3 0.2 0.2 1.0)     ; 蓝
        (vec-3 1.0 0.6 0.1)))   ; 橙

;; 灯的盏数。片元里那两个 4（u-lights[4] 的声明、for 的上界）在 CPU 侧只有这一个
;; 对应物——三处必须一致，少改一处就会"少一盏灯"或"多一盏全黑"（见"试试看"①）。
(define light-count 4)

;; 四盏灯走的是同一个圆：半径 3、高 1.2，绕 y 轴公转，相位各差 π/2。
(define light-radius 3.0)
(define light-height 1.2)
(define orbit-speed 0.6)                       ; 弧度/秒；一圈约 10 秒

;; 衰减系数取 11 课 point.rkt 的同一档：kc = 1.0 / kl = 0.09 / kq = 0.032
;;（大致覆盖 50 个单位以内的距离，参数表见 11.2）。四盏灯共用这三个数——
;; 它们是循环外的普通 uniform，不像位置和颜色那样属于某一盏灯。
;; kc 尤其不能是 0：它贴着灯时分母的保底，也是"距离为 0 时别除以 0"的那道闸。
(define k-constant 1.0)
(define k-linear 0.09)
(define k-quadratic 0.032)

;; 第 i 盏灯在第 t 秒的位置。相位 i·π/2 让四盏灯均匀铺在圆上（差 90°），
;; 于是任意时刻四个方向都有一盏灯，四个颜色池分别落在朝向不同的面上。
(define (light-pos i t)
  (define phase (+ (* i (/ pi 2.0)) (* t orbit-speed)))
  (vec-3 (* light-radius (cos phase)) light-height (* light-radius (sin phase))))

;; 九个立方体散布在原点周围（同 06 课那种世界），都只平移、不旋转：
;; 本课要看的是"光池落在哪、是什么颜色"，立方体自己是静的，动的是光。
;; 位置特意分成几组，大致朝着几个方向摊开，好让四盏灯各自有块地方照。
;; 高度都收在 ±1.2 之内（四盏灯在 y = 1.2 上绕圈，太高的立方体接不到灯的光）。
(define cube-positions
  (list (vec-3  0.0  0.0   0.0)
        (vec-3  2.4 -0.3  -0.2)     ; +x
        (vec-3 -2.3  0.6   0.0)     ; -x
        (vec-3  0.1  1.5   2.4)     ; +z
        (vec-3  0.0 -1.2  -2.4)     ; -z
        (vec-3  1.7  1.7  -2.1)
        (vec-3 -1.9  1.9  -1.6)
        (vec-3  1.6 -1.0   1.8)
        (vec-3 -1.5 -1.1   1.9)))

;; 相机固定不动，停在右上前方斜看原点一带。立方体不旋转，能看出立体感的就只有
;; 这个斜角——和 09 课给那一个立方体摆个倾斜姿态是同一个目的（让两三个面同时可见），
;; 只是这次动的是相机：从 (6.4, 3.6, 5.7) 看过去，+x 面、+z 面和顶面都进画面，
;; 九个立方体（最低的在 y = -1.2）也全都落在取景框里。
(define eye-pos (vec-3 6.4 3.6 5.7))
(define look-at-pos (vec-3 0.0 -0.2 -0.6))

(define program #f)
(define vao #f)
;; 带下标的 uniform 不是一个位置，而是一串：u-lights[0].pos 和 u-lights[1].pos 是
;; 两个独立的位置。所以这两种各存一个向量，下标与 shader 里的 i 一一对应。
(define u-light-pos-locs #f)
(define u-light-color-locs #f)

(glsl-uniform-setter! program vert-src frag-src)

(define (init)
  (set! program (own! 'program (build-program (gl-vertex-shader vert-src)
                                              (gl-fragment-shader frag-src))))

  ;; 画立体的东西就得开深度测试（06 课）。
  (gl-enable gl-depth-test)

  ;; ---- VAO / VBO / EBO ----
  (set! vao (own! 'vertex-array (vertex-array-new)))
  (gl-bind-vertex-array vao)

  (define vbo (own! 'buffer (buffer-new)))
  (gl-bind-buffer gl-array-buffer vbo)
  (buffer-upload gl-array-buffer cube-data gl-static-draw)
  (glsl-vertex-layout! vert-src vertex)

  (define ebo (own! 'buffer (buffer-new)))
  (gl-bind-buffer gl-element-array-buffer ebo)
  (buffer-upload gl-element-array-buffer indices gl-static-draw)

  (gl-bind-vertex-array 0)

  ;; ---- 带下标的 uniform 名字：struct 数组在 GL 那边是平铺的 ----
  ;; GL 里没有"u_lights"这个整体可查：uniform 数组的每个元素、每个字段都摊成
  ;; 一个独立的名字——u-lights[0].pos、u-lights[0].color、u-lights[1].pos……
  ;; 所以名字只能自己用字符串拼出来逐个查；这是 CPU 与 shader 数组对上号的唯一途径。
  ;; 拼错（u-light[0].pos、u-lights[0].position）不会报任何错：uniform-location 返回 -1，
  ;; 之后的 uniform-set! 静默失效，画面上只是那一盏灯不亮（03 课 uniform 的静默坑）。
  (set! u-light-pos-locs
        (for/vector ([i (in-range light-count)])
          (uniform-location program (format "u_lights[~a].pos" i))))
  (set! u-light-color-locs
        (for/vector ([i (in-range light-count)])
          (uniform-location program (format "u_lights[~a].color" i))))

)

(define (draw)
  (define t (/ (current-inexact-monotonic-milliseconds) 1000.0))

  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear (bitwise-ior gl-color-buffer-bit gl-depth-buffer-bit))

  (define view (m-look-at eye-pos look-at-pos (vec-3 0.0 1.0 0.0)))
  (define proj (m-perspective (degrees->radians 45.0) aspect 0.1 100.0))

  (gl-bind-vertex-array vao)
  (program-use program)

  (set-u-view! view)
  (set-u-proj! proj)
  ;; 三个衰减系数四盏灯共用，和 11 课同一档（kc 1.0 / kl 0.09 / kq 0.032），
  ;; 整帧只上传一次——它们在片元里也是循环外的普通 uniform。
  (set-u-kc! k-constant)
  (set-u-kl! k-linear)
  (set-u-kq! k-quadratic)
  (set-u-view-pos! eye-pos)
  (set-u-object-color! object-color)

  ;; 四盏灯：按下标把位置与颜色各传一份。位置每帧都在变（公转），颜色是常数，
  ;; 但两者都走同一个下标——这就是"数组"在 CPU 侧的样子：循环 + 下标。
  (for ([i (in-range light-count)])
    (define p (light-pos i t))
    (uniform-set! (vector-ref u-light-pos-locs i) 'vec-3 p)
    (define c (list-ref light-colors i))
    (uniform-set! (vector-ref u-light-color-locs i) 'vec-3 c))

  (for ([pos (in-list cube-positions)])
    ;; 只平移、不旋转：立方体的朝向整帧不变，颜色池的变化全来自灯在动。
    (define model (m-translate pos))
    (set-u-model! model)
    ;; 只含平移时法线矩阵就是单位阵；照 09 课的规矩照样上传，给某个立方体加旋转时
    ;; 不用改别处（见文件末尾"试试看"）。
    (set-u-normal-matrix! (m-normal-matrix model))
    (gl-draw-elements gl-triangles 36 gl-unsigned-short 0))

  (gl-bind-vertex-array 0))

(define-values (frame canvas)
  (make-window #:title "12 多光源"
               #:width win-w
               #:height win-h
               #:init init
               #:draw draw
               ;; 窗口尺寸变化、viewport 重设之后被调用，参数是新的客户区宽高。
               #:on-size (lambda (cw ch) (set! aspect (/ cw ch)))
               #:on-close release-all!))
(send frame show #t)

;; 灯在公转，颜色池一直在流动，所以要定时器不断请求重绘（同 06 / 11 聚光那节）。
;; 定时器只负责"每隔 16ms 请求重绘"，绘制永远发生在 draw 里。
(define timer
  (new timer% [interval 16]
              [notify-callback (lambda () (send canvas refresh))]))

;; ------------------------------------------------------------
;; 试试看（改完重跑）：
;;   · 把盏数减到 2：片元里 u-lights[4] 改成 u-lights[2]、for 的 (< i 4) 改成 (< i 2)，
;;     Racket 侧 light-count 改成 2，同时 light-colors 只留前两个颜色
;;     （第三、四盏不上传，uniform 里是 0）。两个颜色池消失，画面整体变暗
;;     （底光也少了两份）——"几盏灯"确实就是那一个数。
;;   · 把 object-color 从 (vec-3 0.8 0.8 0.8) 改回 (vec-3 1.0 0.5 0.31)（08–11 课的橙）：
;;     四个颜色池立刻被物体统一压成橙的深浅，蓝那盏照的那块暗下去一大截——
;;     这就是 08 课的乘法，它发生在灯色之后（这也是本课物体用浅灰的原因）。
;;   · 把 for 循环手写展开成四份重复代码（i 换成 0/1/2/3）：画面一模一样，
;;     但再加一盏灯要改的地方从两处变成五处，可读性也全没了。
;;   · 四盏灯的相位改成一样（light-pos 里去掉 (* i (/ pi 2.0))）：四盏灯挤在同一段圆弧上，
;;     四个颜色池叠成一个发白的亮区，朝着它们的那些面一起亮起来。
;; ============================================================