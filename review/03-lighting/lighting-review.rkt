#lang glsl
;; ============================================================
;; 复习 03 · 光照计算：CPU 算，还是 GPU 算？
;; 运行：racket review/03-lighting/lighting-review.rkt
;; ============================================================
;; 这个脚本不创建窗口。它把 08–11 课的 Phong 公式在 CPU 上算一遍，
;; 让你看到片元着色器里那几行到底在算什么。
;;
;; 同一套公式：
;;   ambient  = 0.15
;;   diffuse  = max(dot(n, l), 0)
;;   specular = pow(max(dot(v, r), 0), 32) * 0.4
;;   color    = (ambient + diffuse + specular) * lightColor * objectColor
;;
;; 放在 CPU：逐顶点算（Gouraud）或离线烘焙；
;; 放在 GPU：逐片元算（Phong），每个像素都跑一遍。
;;
;; 两者不是“对/错”，而是质量、成本、数据流量的不同选择。

(require racket/math)

;; 命名：vec-3 / vec-4 / mat-4 / vx / vdot / vnormalize 这些名字来自
;; #lang glsl（racket-shader），不是 racket/math；racket/math 只提供
;; degrees->radians 这类数学函数。生成 GLSL 时 vec-3 才写成 vec3。

(define (v->list v) (f32vector->list v))

;; 固定的场景量
(define light-pos  (vec-3 0.0 0.0 5.0))
(define view-pos   (vec-3 0.0 0.0 5.0))
(define light-color (vec-3 1.0 1.0 1.0))
(define object-color (vec-3 0.8 0.5 0.3))

;; 对表面上一点（位置 + 法线）算 Phong
(define (phong frag-pos normal)
  (define n (vnormalize normal))
  (define l (vnormalize (vsub light-pos frag-pos)))
  (define v (vnormalize (vsub view-pos frag-pos)))
  (define r (vreflect (vneg l) n))

  (define ambient 0.15)
  (define diff (max (vdot n l) 0.0))
  (define spec (* (expt (max (vdot v r) 0.0) 32.0) 0.4))
  (define lit (* (+ ambient diff spec) 1.0))

  ;; 这里 light-color 是白色，所以 lit 直接乘物色；
  ;; 换成彩色光时就是 (lit * light-color) * object-color。
  (define color (vmul (vmul light-color lit) object-color))
  (list n l v r ambient diff spec color))

(define frag (vec-3 0.0 0.0 0.0))

(printf "复习 03 · 光照计算\n\n")
(printf "light-pos = ~a\n" (v->list light-pos))
(printf "view-pos  = ~a\n" (v->list view-pos))
(printf "object    = ~a\n\n" (v->list object-color))

;; 三个不同朝向的表面点：正面、斜面、侧面
(define normals
  (list (vec-3 0.0 0.0 1.0)
        (vnormalize (vec-3 1.0 0.0 1.0))
        (vec-3 1.0 0.0 0.0)))

(for ([nrm (in-list normals)])
  (define-values (n l v r ambient diff spec color)
    (apply values (phong frag nrm)))
  (printf "normal = ~a\n" (v->list nrm))
  (printf "  n = ~a\n" (v->list n))
  (printf "  l = ~a\n" (v->list l))
  (printf "  v = ~a\n" (v->list v))
  (printf "  r = ~a\n" (v->list r))
  (printf "  ambient=~a diffuse=~a specular=~a\n" ambient diff spec)
  (printf "  color = ~a\n\n" (v->list color)))

(printf "如果放在 GPU：\n")
(printf "  1. CPU 传 uniform：light-pos / view-pos / light-color / object-color\n")
(printf "  2. 顶点着色器把 normal 和 frag-pos 插值后传给片元\n")
(printf "  3. 片元着色器对每个像素执行上面的公式\n")
(printf "  4. 像素越多，GPU 的并行优势越明显；但每个像素都要算一遍\n\n")
(printf "如果放在 CPU：\n")
(printf "  1. 逐顶点算（Gouraud）：每个顶点算一次，三角形内部插值颜色\n")
(printf "  2. 或离线烘焙：把光照写进顶点色/lightmap，运行时只采样\n")
(printf "  3. 顶点少、光照静态时很划算；想逐像素高光/动态灯时质量和灵活性会受限\n")
