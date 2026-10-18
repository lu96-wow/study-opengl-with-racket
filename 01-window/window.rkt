#lang glsl-opengl
;; ============================================================
;; 01 · 窗口 —— 先给 OpenGL 开一扇窗
;; 运行：racket 01-window/window.rkt
;; 上一课：00-intro.rkt
;; ============================================================
;; OpenGL 自己不提供窗口，它只往"某块画布"上画。所以先解决画布从哪来。
;; 这一课就三件事：拿到画布、画一帧（清屏）、让它动起来。

(require racket/class
         racket/gui       ; 窗口系统：frame% canvas% timer% 等
         racket/draw)     ; gl-config% 在这里

;; ---- 窗口 --------------------------------------------------
;; racket/gui 是事件驱动的：没有 while 主循环。你造好窗口、显示，
;; 系统在"该重画""点了关闭"时回调你写的方法。
;; frame% 是窗口类；这里派生一个子类，只为加一件事：点 X 时真正退出进程
;; （默认只是把窗口藏起来）。
(define gl-frame%
  (class frame%
    ;; on-close：点 X 时系统调的方法。
    ;; define/augment：在父类实现之外追加动作（不替换）。
    (define/augment (on-close)
      (inner (void) on-close)   ; 先走父类原本的关闭流程
      (exit 0))                 ; 再结束进程
    (super-new)))               ; 调用父类构造器

;; ---- GL 上下文 ---------------------------------------------
;; OpenGL 没有全局状态：颜色、缓冲、着色器程序都住在一个叫"上下文"的环境里。
;; 一块画布 = 一个上下文；所有 gl-* 命令都发给"当前上下文"。
(define cfg (new gl-config%))
;; core profile：只用现代可编程管线，不要旧式固定管线（和 shader 里的 330 core 对应）
(send cfg set-legacy? #f)

;; ---- GL 画布 -----------------------------------------------
;; canvas% 加上 'gl 样式，就会在自己身上创建 GL 上下文。
;;   'no-autoclear：别让系统自动擦背景，擦除我们自己用 gl-clear 控制。
(define gl-canvas%
  (class canvas%
    ;; inherit：把父类的方法拿进来，子类里才能直接调用
    (inherit with-gl-context swap-gl-buffers get-gl-client-size)

    (define start-ms (current-inexact-monotonic-milliseconds))  ; 动画计时起点

    ;; on-size：窗口尺寸变化时回调。
    ;; 顶点坐标是标准坐标（-1..1），gl-viewport 决定它贴到窗口的哪块像素矩形；
    ;; 参数是 (左下角 x, 左下角 y, 宽, 高)，这里铺满整个画布。
    (define/override (on-size _w _h)
      (with-gl-context
       (lambda ()
         (define-values (cw ch) (get-gl-client-size))
         (gl-viewport 0 0 cw ch))))

    ;; on-paint：系统需要重画时回调。所有绘制都写这里。
    (define/override (on-paint)
      ;; with-gl-context：先切到本画布的上下文，再执行 lambda。
      ;; 不进去的话，gl-* 不知道命令该发给谁。
      (with-gl-context
       (lambda ()
         ;; 用经过的时间做一个会"呼吸"的颜色。
         ;; 用单调时钟，别数"画了多少帧"——定时器间隔只是约数，会漂移。
         (define t (/ (- (current-inexact-monotonic-milliseconds) start-ms) 1000.0))

         ;; gl-clear-color(r, g, b, a)：设清屏色，只"记住"，不画。
         ;; gl-clear(mask)：真正擦除；gl-color-buffer-bit 表示颜色缓冲
         ;; （它是位掩码常量，以后要同时擦深度缓冲时把掩码"按位或"起来）。
         (gl-clear-color (* 0.5 (+ 1.0 (sin t)))
                         (* 0.5 (+ 1.0 (sin (+ t 2.0))))
                         (* 0.5 (+ 1.0 (sin (+ t 4.0))))
                         1.0)
         (gl-clear gl-color-buffer-bit)

         ;; swap-gl-buffers：双缓冲翻页。我们画在"后台缓冲"，这一句整体翻到屏幕；
         ;; 直接画前台会看到画到一半的样子（闪烁）。
         (swap-gl-buffers))))

    (super-new)))

;; ---- 组装 --------------------------------------------------
(define frame (new gl-frame% [label "01 窗口"] [width 640] [height 480]))
(define canvas (new gl-canvas%
                    [style '(gl no-autoclear)]
                    [gl-config cfg]
                    [parent frame]))
(send frame show #t)

;; ---- 让它动起来 --------------------------------------------
;; 上面只在"需要重画时"画一帧。timer% 每 16ms 调一次回调，
;; 回调里请求重绘，系统随后调 on-paint —— 定时器只请求，绘制永远在 on-paint。
(define timer
  (new timer% [interval 16]
              [notify-callback (lambda () (send canvas refresh))]))
