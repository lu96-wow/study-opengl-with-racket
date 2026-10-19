#lang glsl-opengl
;; ============================================================
;; lib/text-texture.rkt —— 把一段文字画成一张 GL 纹理
;; ============================================================
;; 思路和 raylib 的 font atlas 相同，只是这里先做最简单的版本：
;;   1. 用 racket/draw 把整段文字画到透明 bitmap
;;   2. 取出 ARGB 像素，转成 GL 要的 RGBA
;;   3. 上传成一张普通 2D 纹理
;;
;; 返回一个 text-texture：
;;   id     —— GL 纹理 id
;;   width  —— 纹理像素宽
;;   height —— 纹理像素高
;;   aspect —— width / height，画 3D quad 时用它保持文字比例
;;
;; 注意：每个字符串一张纹理，适合标题/标签；大量动态文字应改成字形 atlas。

(require racket/class
         racket/draw)

(provide text-texture text-texture?
         text-texture-id text-texture-width text-texture-height text-texture-aspect
         make-text-texture delete-text-texture)

(struct text-texture (id width height aspect) #:transparent)

;; 把 A,R,G,B 字节序转成 R,G,B,A
(define (argb->rgba argb width height)
  (define rgba (make-u8vector (* width height 4) 0))
  (for ([i (in-range 0 (bytes-length argb) 4)])
    (u8vector-set! rgba i       (bytes-ref argb (+ i 1))) ; R
    (u8vector-set! rgba (+ i 1) (bytes-ref argb (+ i 2))) ; G
    (u8vector-set! rgba (+ i 2) (bytes-ref argb (+ i 3))) ; B
    (u8vector-set! rgba (+ i 3) (bytes-ref argb i)))      ; A
  rgba)

;; 创建纹理。参数都在 CPU 侧；必须在 GL 上下文里调用。
;;   text       —— 要显示的字符串
;;   #:font-size —— 字号（像素高，默认 48）
;;   #:face      —— 字体名（默认 "sans"）
;;   #:padding   —— 四周留白，防止抗锯齿边缘被裁掉（默认 4）
(define (make-text-texture text
                           #:font-size [font-size 48]
                           #:face [face "sans"]
                           #:padding [padding 4])
  (unless (string? text) (error 'make-text-texture "text 应为 string，实际 ~s" text))
  (define font (make-font #:size font-size #:face face))

  ;; 先量文字尺寸。get-text-extent 返回 4 个值：宽、高、下降、空格宽。
  (define probe (make-bitmap 1 1))
  (define probe-dc (new bitmap-dc% [bitmap probe]))
  (send probe-dc set-font font)
  (define-values (text-w text-h _descent _space)
    (send probe-dc get-text-extent text font))

  (define w (+ padding padding (inexact->exact (ceiling text-w))))
  (define h (+ padding padding (inexact->exact (ceiling text-h))))

  ;; 真正画到透明 bitmap 上。文字画成白色，颜色留给 shader 的 u-color 调。
  (define bmp (make-bitmap w h))
  (define dc (new bitmap-dc% [bitmap bmp]))
  (send dc set-background (make-color 0 0 0 0))
  (send dc clear)
  (send dc set-smoothing 'smoothed)
  (send dc set-font font)
  (send dc set-text-foreground (make-color 255 255 255 1.0))
  (send dc draw-text text padding padding)

  ;; 取像素（A,R,G,B）并转成 RGBA。
  (define argb (make-bytes (* w h 4)))
  (send bmp get-argb-pixels 0 0 w h argb)
  (define rgba (argb->rgba argb w h))

  ;; 上传成 GL 纹理。文字不做 mipmap，线性过滤 + clamp 就够了。
  ;; 注意：这里创建的纹理归调用方，用完用 delete-text-texture 删（见 13 课的 shutdown）。
  (define id (texture-new))
  (gl-bind-texture gl-texture-2d id)
  (gl-pixel-store-i gl-unpack-alignment 1)
  (gl-tex-parameter-i gl-texture-2d gl-texture-min-filter gl-linear)
  (gl-tex-parameter-i gl-texture-2d gl-texture-mag-filter gl-linear)
  (gl-tex-parameter-i gl-texture-2d gl-texture-wrap-s gl-clamp-to-edge)
  (gl-tex-parameter-i gl-texture-2d gl-texture-wrap-t gl-clamp-to-edge)
  (gl-tex-image-2d gl-texture-2d 0 gl-rgba w h 0
                   gl-rgba gl-unsigned-byte rgba)
  (gl-bind-texture gl-texture-2d 0)

  (text-texture id w h (/ w h)))

;; 释放纹理（GL 上下文里调用）
(define (delete-text-texture tex)
  (texture-delete (text-texture-id tex)))
