#lang glsl
;; ============================================================
;; lib/matrix.rkt —— 变换 / 投影 / 视图矩阵（05–07 三节共用）
;; ============================================================
;; 为什么有这个文件：racket-shader 只提供 GLSL 与矩阵运算本身
;; （mat4-mul / mat4-mul-vec / vdot / vcross / vnormalize / …），不含
;; "平移、旋转、缩放、透视、look-at"这些公式。05 变换、06 坐标系、07 相机
;; 三节全都要用，与其每课各抄一遍、改一处就漂移，不如共用这一份。
;;
;; 约定：角度一律弧度；向量参数一律 vec-3；返回 mat-4。
;; mat-4 是列主序 f32vector（mat-4 收 4 个列向量），与 OpenGL uniform 上传
;; 布局一致（uniform-set! 上传矩阵时 transpose 固定 #f），不用再转置。

(provide m-translate m-rotate-x m-rotate-y m-rotate-z m-scale m-perspective m-look-at
         m-normal-matrix)

;; 平移：把点 (x,y,z) 变成 (x+tx, y+ty, z+tz)，w 保持 1。
;; 位移只体现在第 4 列（col3），前 3 列是单位阵——列主序下位移就长在最后一列。
(define (m-translate v)
  (mat-4 (vec-4 1.0 0.0 0.0 0.0)
        (vec-4 0.0 1.0 0.0 0.0)
        (vec-4 0.0 0.0 1.0 0.0)
        (vec-4 (vx v) (vy v) (vz v) 1.0)))

;; 绕 X 轴右手旋转 a 弧度：y、z 两个分量在 (y,z) 平面里逆时针转，x 不动。
;; 于是 +y 转向 +z（右手法则：拇指指 +x，四指从 +y 卷向 +z）。
(define (m-rotate-x a)
  (define c (cos a))
  (define s (sin a))
  (mat-4 (vec-4 1.0 0.0 0.0 0.0)
        (vec-4 0.0 c s 0.0)
        (vec-4 0.0 (- s) c 0.0)
        (vec-4 0.0 0.0 0.0 1.0)))

;; 绕 Y 轴右手旋转 a 弧度：z、x 两个分量在 (z,x) 平面里转，y 不动。
;; +z 转向 +x——从 +y 往下看是逆时针。
(define (m-rotate-y a)
  (define c (cos a))
  (define s (sin a))
  (mat-4 (vec-4 c 0.0 (- s) 0.0)
        (vec-4 0.0 1.0 0.0 0.0)
        (vec-4 s 0.0 c 0.0)
        (vec-4 0.0 0.0 0.0 1.0)))

;; 绕 Z 轴右手旋转 a 弧度：x、y 两个分量在 (x,y) 平面里转，z 不动。
;; +x 转向 +y——就是我们纸面上看的逆时针。
(define (m-rotate-z a)
  (define c (cos a))
  (define s (sin a))
  (mat-4 (vec-4 c s 0.0 0.0)
        (vec-4 (- s) c 0.0 0.0)
        (vec-4 0.0 0.0 1.0 0.0)
        (vec-4 0.0 0.0 0.0 1.0)))

;; 缩放：x、y、z 各分量分别乘 sx、sy、sz，w 不变。
;; 对角线上放三个缩放因子，其余为 0。
(define (m-scale v)
  (mat-4 (vec-4 (vx v) 0.0 0.0 0.0)
        (vec-4 0.0 (vy v) 0.0 0.0)
        (vec-4 0.0 0.0 (vz v) 0.0)
        (vec-4 0.0 0.0 0.0 1.0)))

;; 透视投影：把眼前的一个视锥压成标准立方体（NDC，x/y/z 都在 -1..1）。
;; f 由竖直视场角 fovy 定；near 平面映射到 z=-1，far 平面映射到 z=+1。
;; col2 的 -1 是"透视除法"的机关：它把观察空间的 -z 抄进 w，之后 GPU 除以 w，
;; 远的物体自然变小；同时 z 被压成非线性（近处精度高、远处精度低）。
(define (m-perspective fovy aspect near far)
  (define f (/ 1.0 (tan (/ fovy 2.0))))
  (define nf (- near far))
  (mat-4 (vec-4 (/ f aspect) 0.0 0.0 0.0)
        (vec-4 0.0 f 0.0 0.0)
        (vec-4 0.0 0.0 (/ (+ far near) nf) -1.0)
        (vec-4 0.0 0.0 (/ (* 2.0 far near) nf) 0.0)))

;; 视图矩阵：把"世界坐标"换成"相机坐标"，等价于把整个世界搬到相机眼前——
;; 相机在 eye 看向 center、以 up 定头顶方向，原点就落到相机上、-z 指向 center。
;; f 是相机前向、s 是右向、u 是头顶方向（三者正交）；位移那列把 eye 抹掉。
(define (m-look-at eye center up)
  (define f (vnormalize (vsub center eye)))
  (define s (vnormalize (vcross f up)))
  (define u (vcross s f))
  (mat-4 (vec-4 (vx s) (vx u) (- (vx f)) 0.0)
        (vec-4 (vy s) (vy u) (- (vy f)) 0.0)
        (vec-4 (vz s) (vz u) (- (vz f)) 0.0)
        (vec-4 (- (vdot s eye)) (- (vdot u eye)) (vdot f eye) 1.0)))

;; 法线矩阵：法线要跟着模型转，但不能直接拿 mat-3(model) 去转——模型含非等比缩放时，
;; 顶点被压扁、法线却还指着原方向，光照就错（球会被照成椭球的样子）。
;; 正确的变换是「左上 3x3 的逆转置」：逆抵消缩放的歪曲，再转置回法线该在的方向；
;; 纯旋转 + 等比缩放时逆与转置互相抵消，退化成 mat-3(model)。法线是方向（w=0），平移影响不到它。
(define (m-normal-matrix m)
  (define (e col row) (mat4-ref m col row))
  (mat3-transpose
   (mat3-inverse
    (mat-3 (e 0 0) (e 0 1) (e 0 2)
          (e 1 0) (e 1 1) (e 1 2)
          (e 2 0) (e 2 1) (e 2 2)))))