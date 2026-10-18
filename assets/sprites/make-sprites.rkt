#lang racket
;; ============================================================
;; assets/sprites/make-sprites.rkt —— 生成精灵图（sprite sheet）
;; 运行：racket assets/sprites/make-sprites.rkt（在仓库根目录）
;; 输出：assets/sprites/racket-sigil.png
;; ============================================================
;; 4×4 网格，每帧 256×256，共 16 帧；帧 0 在左上角。
;; 内容是一个发光的 Racket λ 符印：射线旋转、核心呼吸、外圈脉冲。
;; 背景透明，方便在 3D 场景里做 additive blending（叠加发光）。
;;
;; 改颜色/帧数时，记得同步 15 课的 cols / rows / 采样公式。

(require racket/class
         racket/draw
         racket/file
         racket/math)

(define frame-size 256)
(define cols 4)
(define rows 4)
(define frames (* cols rows))
(define sheet-w (* cols frame-size))
(define sheet-h (* rows frame-size))
(define out-dir (build-path (current-directory) "assets" "sprites"))

(define (rgba r g b a)
  (make-color r g b (/ a 255.0)))

(define (centered-text dc text font cx cy color)
  (send dc set-font font)
  (send dc set-text-foreground color)
  (define-values (tw th _descent _space) (send dc get-text-extent text font))
  (send dc draw-text text
        (- cx (/ tw 2.0))
        (- cy (/ th 2.0) 4)))

(define (draw-frame! dc frame)
  (define col (modulo frame cols))
  (define row (quotient frame cols))
  (define ox (* col frame-size))
  (define oy (* row frame-size))
  (define cx (+ ox (/ frame-size 2.0)))
  (define cy (+ oy (/ frame-size 2.0)))
  (define phase (/ (* 2.0 pi frame) frames))

  ;; ---- 外发光：从大到小的半透明圆 ----
  (for ([i (in-range 36)])
    (define k (/ i 35.0))                 ; 0 外圈，1 内圈
    (define r (* 96.0 (- 1.0 k)))
    (define a (* 5.0 (+ 1.0 (* 3.0 k))))  ; 0..20 / 255
    (send dc set-pen (rgba 0 0 0 0) 1 'transparent)
    (send dc set-brush (rgba 255 255 255 a) 'solid)
    (send dc draw-ellipse (- cx r) (- cy r) (* 2.0 r) (* 2.0 r)))

  ;; ---- 旋转的射线 ----
  (for ([i (in-range 12)])
    (define ang (+ phase (* i (/ (* 2.0 pi) 12.0))))
    (define r0 34.0)
    (define r1 (+ 88.0 (* 14.0 (sin (+ (* 2.0 phase) (* i 1.7))))))
    (send dc set-pen (rgba 255 255 255 175) 5 'solid)
    (send dc draw-line
          (+ cx (* r0 (cos ang))) (+ cy (* r0 (sin ang)))
          (+ cx (* r1 (cos ang))) (+ cy (* r1 (sin ang)))))

  ;; ---- 反向的短射线 ----
  (for ([i (in-range 8)])
    (define ang (- (* -1.0 phase) (* i (/ (* 2.0 pi) 8.0))))
    (define r0 62.0)
    (define r1 (+ 78.0 (* 5.0 (sin (+ phase (* i 0.9))))))
    (send dc set-pen (rgba 255 255 255 120) 3 'solid)
    (send dc draw-line
          (+ cx (* r0 (cos ang))) (+ cy (* r0 (sin ang)))
          (+ cx (* r1 (cos ang))) (+ cy (* r1 (sin ang)))))

  ;; ---- 外圈 ----
  (define ring-r (+ 52.0 (* 4.0 (sin (* 2.0 phase)))))
  (send dc set-pen (rgba 255 255 255 220) 3 'solid)
  (send dc set-brush (rgba 0 0 0 0) 'solid)
  (send dc draw-ellipse (- cx ring-r) (- cy ring-r) (* 2.0 ring-r) (* 2.0 ring-r))

  ;; ---- 三个绕行的小光点 ----
  (for ([i (in-range 3)])
    (define ang (+ (* 2.0 phase) (* i (/ (* 2.0 pi) 3.0))))
    (define rr 78.0)
    (define x (+ cx (* rr (cos ang))))
    (define y (+ cy (* rr (sin ang))))
    (send dc set-pen (rgba 0 0 0 0) 1 'transparent)
    (send dc set-brush (rgba 255 255 255 230) 'solid)
    (send dc draw-ellipse (- x 5.0) (- y 5.0) 10.0 10.0))

  ;; ---- 核心 ----
  (define core-r (+ 24.0 (* 4.5 (sin (* 2.0 phase)))))
  (send dc set-pen (rgba 0 0 0 0) 1 'transparent)
  (send dc set-brush (rgba 255 255 255 255) 'solid)
  (send dc draw-ellipse (- cx core-r) (- cy core-r) (* 2.0 core-r) (* 2.0 core-r))

  ;; ---- λ ----
  (define f (make-font #:size 64 #:weight 'bold))
  (centered-text dc "λ" f cx cy (rgba 8 42 70 255)))

(define (main)
  (define bmp (make-bitmap sheet-w sheet-h))
  (define dc (new bitmap-dc% [bitmap bmp]))
  (send dc set-smoothing 'smoothed)
  (send dc set-background (rgba 0 0 0 0))
  (send dc clear)
  (for ([frame (in-range frames)])
    (draw-frame! dc frame))
  (make-directory* out-dir)
  (define path (build-path out-dir "racket-sigil.png"))
  (send bmp save-file path 'png)
  (printf "已生成：~a（~a×~a，~a×~a 网格，~a 帧）\n"
          path sheet-w sheet-h cols rows frames))

(main)
