#lang glsl-opengl
;; ============================================================
;; lib/window.rkt —— 把 01 课的窗口骨架打包成 make-window
;; ============================================================
;; 教程 01.8 解释了这个封装的来历，02 起各课直接用它；
;; 于是代码里不再出现窗口类，只专心写 OpenGL：
;;
;;   (define-values (frame canvas)
;;     (make-window #:title "…"
;;                  #:init (lambda () …)    ; 建 GPU 对象，只跑一次
;;                  #:draw (lambda () …)))  ; 每帧：清屏 + 画
;;   (send frame show #t)
;;   (send canvas focus)                   ; 要收键盘事件的课才有这行，见下
;;
;; 为什么分 #:init 和 #:draw：GPU 对象必须在"已经进入 GL 上下文"之后才能创建，
;; 而上下文要等画布第一次绘制时才存在。所以第一次重绘先跑 #:init，之后每帧跑 #:draw。
;;
;; #:on-key / #:on-key-up 收键盘事件：canvas 必须拿到键盘焦点才收得到，所以在
;; (send frame show #t) 之后要 (send canvas focus)——make-window 返回的第二个值
;; 就是 canvas。key 是 (send e get-key-code) 的结果：字母键是字符（如 #\w），
;; 方向键是符号（如 'left）。按下走 #:on-key，松开走 #:on-key-up；要判断
;; "一直按住"，在按下/松开回调里自己维护一个集合（07.6 就是这么做的）。
;;
;; #:multisample 是可选的多重采样数（默认 0）。15 课的 HD 场景传 4，
;; 其他课不传，行为不变。
;;
;; #:on-size 在窗口尺寸变化、viewport 重设之后被调用，参数是客户区宽高；
;; 用它让投影的 aspect 之类的量跟着窗口走，窗口缩放才不会变形。
;;
;; #:on-focus 在画布获得/失去键盘焦点时调用，参数是布尔值。窗口切走时可以用它
;; 清空按键状态，避免"按住键切窗口，回来还在走"的粘键问题。
;;
;; #:on-close 在用户关窗、进程退出之前调用一次，而且 GL 上下文还活着——
;; GPU 对象只能在这时删（上下文一没，句柄就作废了）。02 课起各课都传
;; 自己的 shutdown（里面是一串显式的 *-delete），把本课创建的对象删干净。
;; 回调里出错只打印，不妨碍退出。

(provide make-window)

(require racket/class
         racket/gui)

(define (make-window #:title [title "study-opengl"]
                     #:width [w 800]
                     #:height [h 600]
                     #:multisample [ms 0]
                     #:init [init void]
                     #:draw [draw void]
                     #:on-key [on-key (lambda (key) (void))]
                     #:on-key-up [on-key-up (lambda (key) (void))]
                     #:on-size [on-size (lambda (cw ch) (void))]
                     #:on-focus [on-focus (lambda (on?) (void))]
                     #:on-close [on-close (lambda () (void))])
  ;; core profile 上下文（只用现代可编程管线）
  (define cfg (new gl-config%))
  (send cfg set-legacy? #f)
  ;; 可选 MSAA：15 课的 HD 场景会传 #:multisample 4；其他课默认 0，行为不变。
  (when (positive? ms)
    (send cfg set-multisample-size ms))

  ;; 关窗前的收尾回调。名字另存成 close-cb：类体里 on-close 是方法名，直接引用会歧义。
  (define close-cb on-close)

  ;; 回调要借助画布的 with-gl-context，但画布是 frame 的子窗口、要等 frame 建好才建，
  ;; 所以先占个位置，下面真正创建时 set! 进来。
  (define canvas #f)

  ;; 在 GL 上下文还活着时跑收尾回调。没初始化成功（画布从没绘制过）时
  ;; with-gl-context 可能抛错——catch 掉，关窗不能被它卡住。
  (define (run-close-cb!)
    (when canvas
      (with-handlers ([exn:fail?
                       (lambda (e)
                         (eprintf "on-close 回调出错（忽略）：~a\n" (exn-message e)))])
        (send canvas with-gl-context close-cb))))

  ;; 点 X 时真正退出进程：先释放 GPU 对象，再退出
  (define closeable-frame%
    (class frame%
      (define/augment (on-close)
        (run-close-cb!)
        (inner (void) on-close)
        (exit 0))
      (super-new)))

  ;; 画布：进入上下文 → 第一次先 init → 每帧 draw → 翻页
  ;; 注意：类体里 on-size 这个名字被同名的方法占住了——写 (on-size cw ch) 是"发给自己"，
  ;; 会无限递归而不是调回调。所以把回调另存成 size-cb，方法里用别名。
  (define size-cb on-size)
  (define focus-cb on-focus)

  (define gl-canvas%
    (class canvas%
      (inherit with-gl-context swap-gl-buffers get-gl-client-size)

      (define inited? #f)

      ;; 窗口尺寸变化 → 重设视口（把 -1..1 的标准坐标贴到新的像素区域）
      ;;                → 再把客户区宽高交给 #:on-size（例如用来重算投影 aspect）
      (define/override (on-size _w _h)
        (with-gl-context
         (lambda ()
           (define-values (cw ch) (get-gl-client-size))
           (gl-viewport 0 0 cw ch)
           (size-cb cw ch))))

      ;; 键盘事件：按下走 #:on-key，松开走 #:on-key-up。
      ;; 松开的 key 在 get-key-release-code 里；get-key-code 此时是 'release。
      (define/override (on-char e)
        (define code (send e get-key-code))
        (if (eq? code 'release)
            (on-key-up (send e get-key-release-code))
            (on-key code)))

      ;; 焦点变化：交给 #:on-focus（例如失去焦点时清空按键集合）。
      (define/override (on-focus on?)
        (focus-cb on?))

      (define init-failed? #f)

      (define/override (on-paint)
        (with-gl-context
         (lambda ()
           ;; 只在第一次尝试 init；成功才标记 inited?，失败就记住、不再重跑。
           ;; 这样 init 报错时不会“没初始化就去 draw”，避免刷屏的级联错（如 program=#f）。
           (unless (or inited? init-failed?)
             (with-handlers ([exn:fail? (lambda (e)
                                          (set! init-failed? #t)
                                          (raise e))])
               (init)
               (set! inited? #t)))
           (when inited?
             (draw)                 ; 每帧内容（自己记得先清屏）
             (swap-gl-buffers)))))

      (super-new)))

  (define frame (new closeable-frame% [label title] [width w] [height h]))
  (set! canvas (new gl-canvas%
                    [style '(gl no-autoclear)]
                    [gl-config cfg]
                    [parent frame]))
  (values frame canvas))
