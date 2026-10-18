#lang racket/base
;; ============================================================
;; 对照文件：不用 racket-shader，只用 RacketGL（opengl 包）
;; 运行：racket compare/raw-triangle.rkt
;; 对应课程里的 02-hello-triangle/triangle.rkt（用 racket-shader）
;; ============================================================
;; 目的：看看少了 racket-shader 之后，同样一个三角形要写多少东西。
;; 注意：本文件故意保留原始 GLSL 拼写（gl_Position / FragColor / vec3），
;;       因为它演示的是"不用 racket-shader、直接写裸 GLSL 字符串"的对照版本。

(require racket/class
         racket/gui
         racket/draw
         opengl          ; glBindBuffer / glBufferData / GL_ARRAY_BUFFER ...
         ffi/vector)     ; s32vector

;; ---- 着色器：只能写成裸 GLSL 字符串 -------------------------
(define vert-src
  "#version 330 core
layout(location = 0) in vec3 aPos;
void main() {
  gl_Position = vec4(aPos, 1.0);
}
")

(define frag-src
  "#version 330 core
out vec4 FragColor;
void main() {
  FragColor = vec4(1.0, 0.5, 0.2, 1.0);
}
")

;; ---- 顶点数据：只能手写 f32vector ---------------------------
(define vertices (f32vector -0.5 -0.5 0.0
                             0.5 -0.5 0.0
                             0.0  0.5 0.0))

(define program #f)
(define vao #f)

;; ---- 手写 compile-shader（racket-shader 用一行 shader-compile 代替）----
(define (shader-info-log shader)
  (define len (glGetShaderiv shader GL_INFO_LOG_LENGTH))
  (define-values (actual log) (glGetShaderInfoLog shader len))
  (bytes->string/utf-8 log #\? 0 actual))

(define (compile-shader type src)
  (define shader (glCreateShader type))
  (glShaderSource shader 1 (vector src) (s32vector (string-length src)))
  (glCompileShader shader)
  (when (zero? (glGetShaderiv shader GL_COMPILE_STATUS))
    (error 'compile-shader "着色器编译失败：\n~a" (shader-info-log shader)))
  shader)

;; ---- 手写 link-program（racket-shader 用一行 build-program 代替）----
(define (program-info-log prog)
  (define len (glGetProgramiv prog GL_INFO_LOG_LENGTH))
  (define-values (actual log) (glGetProgramInfoLog prog len))
  (bytes->string/utf-8 log #\? 0 actual))

(define (link-program vs fs)
  (define prog (glCreateProgram))
  (glAttachShader prog vs)
  (glAttachShader prog fs)
  (glLinkProgram prog)
  (when (zero? (glGetProgramiv prog GL_LINK_STATUS))
    (error 'link-program "程序链接失败：\n~a" (program-info-log prog)))
  prog)

(define (init)
  ;; 编译 + 链接（这里还漏了失败时的清理，racket-shader 会做）
  (define vs (compile-shader GL_VERTEX_SHADER vert-src))
  (define fs (compile-shader GL_FRAGMENT_SHADER frag-src))
  (set! program (link-program vs fs))
  (glDeleteShader vs)
  (glDeleteShader fs)

  ;; 上传顶点数据
  (define vbo (u32vector-ref (glGenBuffers 1) 0))
  (glBindBuffer GL_ARRAY_BUFFER vbo)
  (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof vertices) vertices GL_STATIC_DRAW)

  ;; VAO + 属性
  (set! vao (u32vector-ref (glGenVertexArrays 1) 0))
  (glBindVertexArray vao)
  (glVertexAttribPointer 0 3 GL_FLOAT #f 0 0)
  (glEnableVertexAttribArray 0)
  (glBindVertexArray 0))

(define (draw)
  (glClearColor 0.10 0.12 0.20 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glUseProgram program)
  (glBindVertexArray vao)
  (glDrawArrays GL_TRIANGLES 0 3)
  (glBindVertexArray 0))

;; ---- 窗口骨架：教程的 lib/window.rkt 用不了，只能自己抄 ----
(define cfg (new gl-config%))
(send cfg set-legacy? #f)

(define gl-frame%
  (class frame%
    (define/augment (on-close)
      (inner (void) on-close)
      (exit 0))
    (super-new)))

(define gl-canvas%
  (class canvas%
    (inherit with-gl-context swap-gl-buffers get-gl-client-size)
    (define inited? #f)
    (define/override (on-size _w _h)
      (with-gl-context
       (lambda ()
         (define-values (cw ch) (get-gl-client-size))
         (glViewport 0 0 cw ch))))
    (define/override (on-paint)
      (with-gl-context
       (lambda ()
         (unless inited? (set! inited? #t) (init))
         (draw)
         (swap-gl-buffers))))
    (super-new)))

(define frame (new gl-frame% [label "raw opengl"] [width 800] [height 600]))
(define canvas (new gl-canvas%
                    [style '(gl no-autoclear)]
                    [gl-config cfg]
                    [parent frame]))
(send frame show #t)
