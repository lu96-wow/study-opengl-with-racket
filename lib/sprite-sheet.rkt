#lang glsl-opengl
;; ============================================================
;; lib/sprite-sheet.rkt —— 加载精灵图（sprite sheet / texture atlas）
;; ============================================================
;; 一张精灵图把动画的所有帧排在同一张 PNG 里，例如：
;;
;;   +----+----+----+----+
;;   | 0  | 1  | 2  | 3  |   每一格是一帧
;;   +----+----+----+----+
;;   | 4  | 5  | 6  | 7  |
;;   +----+----+----+----+
;;   | 8  | 9  | 10 | 11 |
;;   +----+----+----+----+
;;   | 12 | 13 | 14 | 15 |
;;   +----+----+----+----+
;;
;; 好处：一次 draw call、一次纹理绑定就能画很多帧/很多精灵；
;; 代价：采样时必须把 0..1 的局部 UV 映射到某一格的 UV 范围。
;;
;; 这个文件只负责“把 PNG 变成 GL 纹理，并记住网格尺寸”：
;;   - read-bitmap 读 PNG
;;   - get-argb-pixels 给出 A,R,G,B（从上到下）
;;   - 转成 R,G,B,A，并把行序翻成 OpenGL 的从下到上
;;   - gl-tex-image-2d 上传，clamp + linear，不生成 mipmap
;;
;; 必须在 GL 上下文里调用 load-sprite-sheet（例如 make-window 的 #:init）。

(require racket/class
         racket/draw
         "gpu.rkt")   ; own!：纹理句柄要登记，否则关闭时不会释放

(provide sprite-sheet sprite-sheet?
         sprite-sheet-id sprite-sheet-width sprite-sheet-height
         sprite-sheet-cols sprite-sheet-rows
         sprite-sheet-frame-w sprite-sheet-frame-h
         sprite-sheet-frame-count
         load-sprite-sheet)

(struct sprite-sheet
  (id width height cols rows frame-w frame-h)
  #:transparent)

(define (sprite-sheet-frame-count s)
  (* (sprite-sheet-cols s) (sprite-sheet-rows s)))

;; A,R,G,B -> R,G,B,A
(define (argb->rgba argb width height)
  (define rgba (make-u8vector (* width height 4) 0))
  (for ([i (in-range 0 (bytes-length argb) 4)])
    (u8vector-set! rgba i       (bytes-ref argb (+ i 1))) ; R
    (u8vector-set! rgba (+ i 1) (bytes-ref argb (+ i 2))) ; G
    (u8vector-set! rgba (+ i 2) (bytes-ref argb (+ i 3))) ; B
    (u8vector-set! rgba (+ i 3) (bytes-ref argb i)))      ; A
  rgba)

;; 位图行序（从上到下）→ OpenGL 纹理行序（从下到上）
(define (flip-rows rgba width height)
  (define row-bytes (* width 4))
  (define out (make-u8vector (u8vector-length rgba) 0))
  (for ([y (in-range height)])
    (define src-y (- height 1 y))
    (bytes-copy! out (* y row-bytes)
                 rgba (* src-y row-bytes) (* (add1 src-y) row-bytes)))
  out)

;; load-sprite-sheet : path cols rows -> sprite-sheet
;; PNG 的宽必须能被 cols 整除、高必须能被 rows 整除。
(define (load-sprite-sheet path cols rows)
  (define bmp (read-bitmap path))
  (define w (send bmp get-width))
  (define h (send bmp get-height))
  (unless (and (positive? cols) (positive? rows)
               (zero? (modulo w cols))
               (zero? (modulo h rows)))
    (error 'load-sprite-sheet
           "精灵图尺寸 ~ax~a 不能被 ~ax~a 网格整除" w h cols rows))

  (define argb (make-bytes (* w h 4)))
  (send bmp get-argb-pixels 0 0 w h argb)
  (define rgba (flip-rows (argb->rgba argb w h) w h))

  ;; 登记纹理句柄（lib/gpu.rkt）：sprite-sheet 里只留 id 和尺寸，
  ;; 释放时机交给窗口关闭时的 release-all!。
  (define id (own! 'texture (texture-new)))
  (gl-bind-texture gl-texture-2d id)

  ;; 每个像素 4 字节；宽度不是 4 的倍数时行首也要对齐。
  (gl-pixel-store-i gl-unpack-alignment 1)

  ;; 精灵图是一个整体，UV 不能越界到隔壁格子。
  (gl-tex-parameter-i gl-texture-2d gl-texture-wrap-s gl-clamp-to-edge)
  (gl-tex-parameter-i gl-texture-2d gl-texture-wrap-t gl-clamp-to-edge)
  ;; 不生成 mipmap：mipmap 会把相邻帧混在一起，边缘出现“串帧”。
  (gl-tex-parameter-i gl-texture-2d gl-texture-min-filter gl-linear)
  (gl-tex-parameter-i gl-texture-2d gl-texture-mag-filter gl-linear)

  (gl-tex-image-2d gl-texture-2d 0 gl-rgba w h 0
                   gl-rgba gl-unsigned-byte rgba)
  (gl-bind-texture gl-texture-2d 0)

  (sprite-sheet id w h cols rows (quotient w cols) (quotient h rows)))
