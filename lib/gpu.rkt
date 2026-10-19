#lang glsl-opengl
;; ============================================================
;; lib/gpu.rkt —— GPU 资源的登记与释放
;; 教程：02.9「谁拥有这些对象」、00-intro 0.8.5
;; ============================================================
;; 为什么需要这一层：
;;   buffer-new / vertex-array-new / texture-new / build-program 返回的都是
;;   **GPU 侧的句柄**——一个整数，对应的内存在驱动里，不在 Racket 堆上。
;;   GC 看不见它，所以永远不会替你回收（为什么不能用 finalizer，见
;;   racket-shader-opengl/DESIGN.md §3.1：删除必须发生在拥有上下文的线程上，
;;   而 GC 线程、时机都不对）。
;;
;;   于是只剩两条路：
;;     1. 每个句柄手写一次 buffer-delete / vertex-array-delete / program-delete …；
;;     2. 分配时登记，退出时一次性排空。
;;   本文件是第 2 条：登记表 + 确定性的释放顺序。
;;
;; 和「自动回收」的区别：
;;   自动回收 = 没人管，GC 猜什么时候可以删（对 GL 不成立）；
;;   这里     = 你明确登记，并在明确的时间点（窗口关闭、上下文还在）删。
;;   时机仍然是确定的，只是把「哪里删」集中到了一处。
;;
;; 用法：
;;   (define vbo (own! 'buffer (buffer-new)))
;;   (set! program (own! 'program (build-program ...)))
;;   (make-window #:init init #:draw draw #:on-close release-all!)
;;
;; 种类名与 delete 函数一一对应：
;;   'buffer → buffer-delete        'vertex-array → vertex-array-delete
;;   'texture → texture-delete      'program      → program-delete
;;   'framebuffer → framebuffer-delete  'renderbuffer → renderbuffer-delete
;; ============================================================

(provide own! release-all! owned-count)

;; 登记表：后登记的放前面。每项是 (种类 . id)。
(define resources '())

(define kinds '(buffer vertex-array texture program framebuffer renderbuffer))

;; 登记一个句柄，返回它本身——这样可以直接套在分配调用外面。
(define (own! kind id)
  (unless (memq kind kinds)
    (error 'own! "未知的资源种类：~s（可用：~s）" kind kinds))
  (unless (and (exact-integer? id) (positive? id))
    (error 'own! "句柄应为正整数，实际 ~s" id))
  (set! resources (cons (cons kind id) resources))
  id)

(define (owned-count) (length resources))

;; 按种类排空登记表。顺序的理由：
;;   · 先把「当前绑定」清掉，避免删掉正在用的对象（GL 允许，但状态更干净）；
;;   · VAO 先于 buffer：VAO 记着 buffer 的引用，先删引用方再删被引用方更好读；
;;   · program 是最外层的「操作者」，最后删也安全。
;; 删完清空表：重复调用（例如关闭时被触发两次）不会重复删除。
(define (release-all!)
  (gl-bind-vertex-array 0)
  (gl-use-program 0)
  (gl-bind-buffer gl-array-buffer 0)
  (for ([kind (in-list '(vertex-array program framebuffer renderbuffer texture buffer))])
    (for ([r (in-list resources)] #:when (eq? (car r) kind))
      (case kind
        [(vertex-array) (vertex-array-delete (cdr r))]
        [(program)      (program-delete (cdr r))]
        [(framebuffer)  (framebuffer-delete (cdr r))]
        [(renderbuffer) (renderbuffer-delete (cdr r))]
        [(texture)      (texture-delete (cdr r))]
        [(buffer)       (buffer-delete (cdr r))])))
  (set! resources '())
  (void))
