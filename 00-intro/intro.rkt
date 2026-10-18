#lang racket/base
;; ============================================================
;; 00 · 开始之前
;; 运行：racket 00-intro/intro.rkt      （不开窗口，只在终端打印课程目录）
;; 教程页：00-intro/index.html
;; ============================================================
;; 每节课都是「一个目录 = 一页 HTML 教程 + 可直接跑起来的源码」。
;; 教程页讲原理和图示，源码文件里的注释讲这一行在干什么，两边对着读。
;;
;; 读代码之前先知道四件事就够了：
;;
;;   1. CPU（跑 Racket）决定画什么；GPU（显卡）并行地算顶点和像素。
;;   2. OpenGL 是一套给 GPU 下命令的标准，命令都以 gl- 开头。
;;   3. OpenGL 是状态机：先设状态，再发命令（比如先绑缓冲、再传数据）。
;;   4. GPU 上的资源（缓冲、着色器程序、纹理……）叫"对象"，用整数 id 引用。
;;
;; 渲染管线（记住这一条主线就够）：
;;   顶点数据 → [顶点着色器] → 图元装配 → 光栅化 → [片元着色器] → 屏幕
;;   方括号里是可编程的两段小程序，叫 shader，用 GLSL 写。
;;
;; 命名约定：
;;   gl-array-buffer 这类是 OpenGL 内置常量的 Racket 改名（C 里写 GL_ARRAY_BUFFER）；
;;   vec-3 / shader-byte-size 这些来自 racket-shader；
;;   build-program / buffer-new 这些便利层来自 racket-shader-opengl；
;;   vertices / vbo / program / init / draw 才是本教程自己起的名字。

;; 课程表：节号 / 标题 / 源码文件 / 一句话说明。
;; 教程页统一是 <目录>/index.html，所以不单独列。
(define course
  (list (list "00" "OpenGL 是什么"
              (list "00-intro/intro.rkt")
              "CPU 下命令、GPU 并行算；状态机、对象、管线主线")
        (list "01" "开一扇窗口"
              (list "01-window/window.rkt")
              "OpenGL 不提供窗口；上下文、双缓冲、事件驱动的主循环")
        (list "02" "第一个三角形"
              (list "02-hello-triangle/triangle.rkt"
                    "02-hello-triangle/rectangle.rkt")
              "VBO 存数据、VAO 记怎么读、EBO 省下重复顶点")
        (list "03" "着色器"
              (list "03-shaders/shaders.rkt")
              "GLSL 类型、阶段间传值、uniform、多属性交错布局")
        (list "04" "纹理"
              (list "04-textures/textures.rkt")
              "UV 采样、环绕、过滤、mipmap、纹理单元")
        (list "05" "变换"
              (list "05-transformations/transform.rkt")
              "平移/旋转/缩放都是乘矩阵；组合顺序从右往左作用")
        (list "06" "坐标系"
              (list "06-coordinate-systems/coordinates.rkt")
              "model/view/projection 三个空间；深度缓冲与透视除法")
        (list "07" "相机"
              (list "07-camera/camera.rkt"
                    "07-camera/camera-walk.rkt")
              "look-at 三基推导；绕圈、自由相机、按键状态与连续移动")
        (list "08" "颜色"
              (list "08-colors/colors.rkt")
              "看到的颜色 = 物体色 × 光色；lamp shader 与双 program")
        (list "09" "基础光照"
              (list "09-basic-lighting/basic-lighting.rkt")
              "ambient/diffuse/specular 三项；法线与 normal matrix")
        (list "10" "光照贴图"
              (list "10-lighting-maps/lighting-maps.rkt")
              "diffuse map 与 specular map：贴图也是开关")
        (list "11" "光的种类"
              (list "11-light-casters/directional.rkt"
                    "11-light-casters/point.rkt"
                    "11-light-casters/spot.rkt")
              "directional/point/spot 三种光源与距离衰减")
        (list "12" "多光源"
              (list "12-multiple-lights/multiple-lights.rkt")
              "struct 数组与 for 循环累加；成本随灯数线性")
        (list "13" "3D 文字"
              (list "13-text-3d/text-3d.rkt")
              "文字 → 纹理 → billboard quad；raylib 的 font atlas 思路")
        (list "14" "模型加载"
              (list "14-model-loading/model-loading.rkt")
              "OBJ → 顶点/索引 → VBO/EBO；法线、UV、材质与 glTF/骨骼拓展")
        (list "15" "精灵图与 HD 3D 场景"
              (list "15-sprite-atlas/sprite-atlas.rkt")
              "4×4 精灵图、图集 UV、billboard、加法混合；1280×720 HD 场景")))

(define reviews
  (list (list "R1" "API 与数据流"
              (list "review/01-api-flow/api-review.rkt")
              "传数据 / 设状态 / 启动计算；API 本身不做矩阵和光照")
        (list "R2" "矩阵与计算位置"
              (list "review/02-matrices/matrix-review.rkt")
              "CPU 算 vs GPU 顶点着色器算；同一公式、不同数据流")
        (list "R3" "光照：CPU 还是 GPU"
              (list "review/03-lighting/lighting-review.rkt")
              "逐顶点 / 逐片元 / 离线烘焙；三种计算位置")
        (list "R4" "用 CPU 走一遍管线"
              (list "review/04-cpu-renderer/cpu-raster.rkt")
              "整条管线：CPU 光栅化 + 深度测试 + 片元着色；画出 14 课的茶壶")))

(define (print-lesson num title files desc)
  (printf "  ~a  ~a\n" num title)
  (for ([f files])
    (printf "      代码  ~a\n" f))
  (printf "      教程  ~a/index.html\n"
          (regexp-replace #rx"/[^/]*$" (car files) ""))
  (printf "      ~a\n\n" desc))

(printf "study-opengl —— 用 Racket 学 OpenGL（节奏跟 learnopengl）\n\n")
(for ([row course])
  (apply print-lesson row))

(printf "复习章节（读完对应阶段后做）：\n\n")
(for ([row reviews])
  (apply print-lesson row))

(printf "课程首页：index.html\n")
(printf "下一课：racket 01-window/window.rkt\n")