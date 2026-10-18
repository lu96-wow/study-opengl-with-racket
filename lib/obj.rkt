#lang glsl-opengl
;; ============================================================
;; lib/obj.rkt —— 极简 Wavefront OBJ 读取器
;; ============================================================
;; 只处理模型加载课需要的那部分 OBJ：
;;   v  x y z          顶点位置
;;   vt u v            UV
;;   vn x y z          法线
;;   f  v/vt/vn ...    面（支持三角形和多边形，多边形用扇形三角化）
;;
;; 不处理：材质（usemtl / mtllib）、动画、曲线、负索引之外的复杂情况。
;; 面的索引写法支持：v、v/vt、v//vn、v/vt/vn；也支持负数（相对当前顶点数）。
;;
;; 输出：
;;   obj-mesh   —— 去重后的顶点表 + 三角形索引表
;;   obj-vertex —— 一个顶点：位置 vec-3、法线 vec-3、UV vec-2
;;
;; 去重键是 (v-index, vt-index, vn-index)。同一个位置如果 UV 或法线不同，
;; 会保留成不同顶点——这正是 OBJ 处理硬边/UV 接缝的方式。

(require racket/file
         racket/list
         racket/string)

(provide obj-vertex obj-vertex?
         obj-vertex-pos obj-vertex-normal obj-vertex-uv
         obj-mesh obj-mesh?
         obj-mesh-vertices obj-mesh-indices
         obj-mesh-vertex-count obj-mesh-index-count
         read-obj)

(struct obj-vertex (pos normal uv) #:transparent)
(struct obj-mesh (vertices indices) #:transparent)

(define (obj-mesh-vertex-count m) (vector-length (obj-mesh-vertices m)))
(define (obj-mesh-index-count m) (vector-length (obj-mesh-indices m)))

;; 去掉注释、空行后的 token 行；保留真实行号
(define (obj-lines path)
  (let loop ([lines (string-split (file->string path) "\n")]
             [line-no 0]
             [acc '()])
    (cond
      [(null? lines) (reverse acc)]
      [else
       (define line (car lines))
       (define no-comment (car (string-split line "#")))
       (define toks (string-split (string-trim no-comment)))
       (loop (cdr lines) (add1 line-no)
             (if (pair? toks) (cons (list (add1 line-no) toks) acc) acc))])))

;; 1-based / 负数索引 → 0-based；缺省返回 #f
(define (resolve-index who idx count)
  (cond
    [(not idx) #f]
    [(= idx 0) (error who "OBJ 索引不能是 0")]
    [(> idx 0) (sub1 idx)]
    [else (+ count idx)]))

(define (resolve-checked who idx count)
  (define i (resolve-index who idx count))
  (when (or (not i) (< i 0) (>= i count))
    (error who "OBJ 索引越界：~a（共 ~a 个）" idx count))
  i)

;; 坐标数字：OBJ 经常写 0 / 1 这种整数；vec-3/vec-2 要 flonum
(define (parse-number who s line)
  (define n (string->number s))
  (unless n (error who "第 ~a 行：不是数字 ~s" line s))
  (exact->inexact n))

;; 解析 f 的一个角：1/2/3、1//3、1/2、1
(define (parse-corner tok)
  (define parts (string-split tok "/" #:trim? #f))
  (define (num s) (and (not (string=? s "")) (string->number s)))
  (values (num (list-ref parts 0))
          (and (>= (length parts) 2) (num (list-ref parts 1)))
          (and (>= (length parts) 3) (num (list-ref parts 2)))))

(define (read-obj path)
  (define vlist '())   ; 反序累积
  (define vtlist '())
  (define vnlist '())
  (define faces '())   ; (list (list corner ...) ...)
  (for ([entry (in-list (obj-lines path))])
    (define line-no (car entry))
    (define toks (cadr entry))
    (define tag (car toks))
    (define args (cdr toks))
    (case tag
      [("v")
       (unless (= 3 (length args)) (error 'read-obj "第 ~a 行：v 需要 3 个数" line-no))
       (set! vlist (cons (apply vec-3 (map (lambda (s) (parse-number 'read-obj s line-no)) args)) vlist))]
      [("vt")
       (unless (>= (length args) 2) (error 'read-obj "第 ~a 行：vt 至少需要 2 个数" line-no))
       (set! vtlist (cons (vec-2 (parse-number 'read-obj (list-ref args 0) line-no)
                                 (parse-number 'read-obj (list-ref args 1) line-no))
                          vtlist))]
      [("vn")
       (unless (= 3 (length args)) (error 'read-obj "第 ~a 行：vn 需要 3 个数" line-no))
       (set! vnlist (cons (apply vec-3 (map (lambda (s) (parse-number 'read-obj s line-no)) args)) vnlist))]
      [("f")
       (unless (>= (length args) 3) (error 'read-obj "第 ~a 行：f 至少需要 3 个角" line-no))
       (set! faces (cons (for/list ([tok (in-list args)])
                           (let-values ([(vi ti ni) (parse-corner tok)])
                             (list vi ti ni)))
                         faces))]
      [else (void)])) ; 忽略 o/g/s/usemtl/mtllib/...

  (define vs (list->vector (reverse vlist)))
  (define vts (list->vector (reverse vtlist)))
  (define vns (list->vector (reverse vnlist)))

  (define verts '())          ; 反序
  (define vert-index (make-hash))
  (define indices '())        ; 反序
  (define (vertex-ref who vi ti ni)
    (define key (list vi ti ni))
    (or (hash-ref vert-index key #f)
        (let* ([p (vector-ref vs vi)]
               [uv (if ti (vector-ref vts ti) (vec-2 0.0 0.0))]
               [n (if ni (vector-ref vns ni) (vec-3 0.0 0.0 0.0))]
               [idx (length verts)])
          (set! verts (cons (obj-vertex p n uv) verts))
          (hash-set! vert-index key idx)
          idx)))

  ;; 每个 face：先解析三个索引，再多边形扇形三角化
  (for ([face (in-list (reverse faces))])
    (define corners
      (for/list ([c (in-list face)])
        (define vi (resolve-checked 'read-obj (list-ref c 0) (vector-length vs)))
        (define ti (and (list-ref c 1)
                        (resolve-checked 'read-obj (list-ref c 1) (vector-length vts))))
        (define ni (and (list-ref c 2)
                        (resolve-checked 'read-obj (list-ref c 2) (vector-length vns))))
        (vertex-ref 'read-obj vi ti ni)))
    (for ([i (in-range 1 (- (length corners) 1))])
      (set! indices (cons (list-ref corners (+ i 1)) (cons (list-ref corners i) (cons (list-ref corners 0) indices))))))

  (obj-mesh (list->vector (reverse verts))
            (list->vector (reverse indices))))
