# study-opengl · 设计文档

## 核心命题

这个教程真正想讲清楚的，不是“OpenGL 有多少个函数”，而是：

> **图形程序 = 公式 + 计算位置 + 数据流。**  
> OpenGL API 只是把公式放到 GPU 上、并把数据送过去的入口。

同一套矩阵乘法、同一套 Phong 光照，既可以在 CPU 上算，也可以在 GPU 上算。  
API 的数量有限；真正变化的是：

1. **公式放在哪算**：CPU / 顶点着色器 / 片元着色器 / 离线预计算 / GPU compute。
2. **数据怎么流**：CPU → GPU、GPU 内部、GPU → CPU，以及每帧传一次还是只传一次。

教程的章节、代码、复习页都围绕这两件事组织。

---

## 1. 三个问题

每写一段图形代码，先问三个问题。

### 1.1 算什么公式？

例如：

- 顶点变换：`clip = proj · view · model · vec4(pos, 1)`
- 透视除法：`ndc = clip.xyz / clip.w`
- 光照：`ambient + diffuse + specular`
- 纹理采样：`texture(map, uv)`
- 文字：把字形位图贴到 quad 上

公式本身与 CPU/GPU 无关。GPU 的硬件单元、内存层次和 OpenGL 数据模型的详细版见 `00-intro/index.html` 的 0.8 节。

### 1.2 在哪算？

| 位置 | 适合 |
|---|---|
| CPU（Racket） | 文件解析、场景图、矩阵构造、少量/串行计算、需要复杂控制流 |
| 顶点着色器 | 每顶点一次的计算：MVP、法线变换、传 varying |
| 片元着色器 | 每像素一次的计算：光照、纹理采样、透明/裁剪 |
| 离线预计算 | lightmap、顶点色、烘焙阴影、SDF 字体 |
| GPU compute | 通用并行计算（本教程暂未展开） |

GPU 不是唯一选择。选择依据是：

- 数据量/并行度：顶点/像素越多，GPU 越有优势。
- 动态性：每帧都变的相机/灯光/骨骼，通常留在 GPU。
- 回读成本：GPU → CPU 通常很贵，能不回读就不回读。
- 传输成本：CPU 算完再上传，可能比“传公式参数 + GPU 算”更贵。
- 实现复杂度：一次性、小规模、需要调试的计算，CPU 更简单。

### 1.3 数据怎么流？

```text
文件/程序数据
  │
  ├─ CPU 解析、构造（OBJ、矩阵、文字位图）
  │
  ├─ 上传：VBO / EBO / texture / uniform
  │        CPU ───────────────▶ GPU
  │
  ├─ GPU 内部：vertex shader → varying → fragment shader
  │
  └─ 输出：framebuffer → 屏幕
           （GPU → CPU 回读通常很贵，尽量避免）
```

同一个公式，换一个计算位置，数据流就完全变了。

---

## 2. API 的角色

OpenGL API 自己不做矩阵乘法、不算光照。它主要做三类事：

| 类别 | 典型 API | 作用 |
|---|---|---|
| 传数据 | `gl-buffer-data`、`gl-tex-image-2d`、`gl-uniform-*` | 把数据从 CPU 送到 GPU |
| 设状态 | `gl-use-program`、`gl-bind-vertex-array`、`gl-enable`、`gl-blend-func` | 告诉 GPU 下一步用什么 |
| 启动计算 | `gl-draw-arrays`、`gl-draw-elements` | 让 GPU 跑顶点/片元 shader |

所以：

```text
API = 数据搬运 + 状态设置 + 计算启动
公式 = shader 或 CPU 代码
```

教程里的复习 R1 就是把这张表重新过一遍。

---

## 3. 课程结构

```text
正式章节：用例 + API + 计算位置提示
   ↓
复习章节：API 记忆 / 数据流 / CPU vs GPU 同一公式
   ↓
后续章节：算法与计算本质
```

### 3.1 正式章节

正式章节随着用例讲 API，但每章都要能回答：

- 这一章的公式是什么？
- 公式在 CPU 还是 GPU 上算？
- 数据从哪来、到哪去？
- 如果换到另一边算，代价是什么？

### 3.2 复习章节

| 复习 | 位置 | 主题 |
|---|---|---|
| R1 | `review/01-api-flow/` | API 与数据流：传数据 / 设状态 / 启动计算 |
| R2 | `review/02-matrices/` | 矩阵：CPU 算 vs GPU 顶点着色器算 |
| R3 | `review/03-lighting/` | 光照：逐顶点 / 逐片元 / 离线烘焙 |
| R4 | `review/04-cpu-renderer/` | 整条管线：CPU 光栅化 + 深度测试 + 片元着色，画出同一只茶壶 |

复习章节都带可运行代码，不是纯文字。R4 是**参考实现**：它把 GPU 固定管线里的光栅化、深度测试、透视校正显式写出来，并给出各阶段执行次数和耗时。后面每章的 CPU 对照都可以指回它的对应函数。

写作时区分两件事：

- **公式层**（矩阵、Phong、纹理采样、法线变换）：适合 CPU 先算，因为 CPU 循环和 shader 循环一一对应。
- **管线层**（VBO/VAO、光栅化、深度测试、混合、纹理上传）：不适合从 CPU 讲起。CPU 版等于重写一遍 OpenGL，比直接用 GL 更难。

所以「CPU 先算」从 05 开始按公式使用，不要求 01–04 也先写一遍软件管线。

### 3.3 课程与计算位置对照

| 章节 | 主要公式 | CPU 侧 | GPU 侧 | 数据流 |
|---|---|---|---|---|
| 02 | 顶点位置/颜色 | 准备顶点 | 顶点/片元 shader | VBO/VAO → draw |
| 03 | varying / uniform | 准备 uniform | 插值、使用 uniform | uniform → shader |
| 04 | 纹理采样 | 生成/加载像素 | texture 采样 | texture → shader |
| 05 | 矩阵乘法 | 构造矩阵、`order-check.rkt` 验证顺序 | 顶点乘矩阵 | uniform → vertex |
| 06 | MVP / NDC | 构造 model/view/proj | MVP、透视除法 | uniform → vertex → fixed |
| 07 | look-at / perspective | 相机矩阵 | view/proj 乘法 | uniform → vertex |
| 08 | 颜色乘法 | 可选预计算 | 片元乘法 | uniform → fragment |
| 09 | Phong | `phong-cpu.rkt` 三个采样点 | 逐片元 Phong | varying/uniform → fragment |
| 10 | 材质贴图 | 生成/加载贴图 | 采样 diffuse/spec | texture → fragment |
| 11 | 光源类型/衰减 | 光参数 | 片元光照 | uniform → fragment |
| 12 | 多光源累加 | 灯数组 | 片元循环累加 | uniform array → fragment |
| 13 | 文字 | 字形位图 | 纹理 quad | bitmap → texture → quad |
| 14 | 模型 | OBJ 解析、CPU 参考渲染（R4） | 顶点变换/光照 | file → VBO/EBO → shader |
| 15 | 精灵图 / HD 场景 | 图集加载、帧 UV、billboard、深度/混合顺序 | 图集采样、加法混合、MSAA | PNG → atlas → 片元 UV → 屏幕 |
| R4 | 整条管线 | 光栅化、深度测试、透视校正、Phong | 同一套公式的并行版 | CPU 帧缓冲 → 图 |

---

### 3.4 GPU 资源的所有权

`buffer-new` / `vertex-array-new` / `texture-new` / `build-program` 返回的是 **GPU 侧句柄**：内存在驱动里，Racket 堆里只有那个整数，GC 看不见也不能回收。`racket-shader-opengl` 因此不挂终结器（删除必须在拥有上下文的线程上执行，见其 DESIGN.md §3.1）。

约定：

- **每课都手动删、不用封装**：句柄提到顶层（`(define vbo 0)`），关窗前在 `shutdown` 里逐个 `program-delete` / `vertex-array-delete` / `buffer-delete` / `texture-delete`，由 `make-window` 的 `#:on-close` 在上下文还活着时触发。
- 教程不提供 `own!` 之类的登记表：新手要能看见「谁创建、谁删除」的对应关系。等对象多到几十上百个，再考虑登记表（`racket-shader-opengl` DESIGN.md §3.1 的入队思路），那是优化而不是入门必需。
- **Racket 值不用管**：`vec-array` / `vec-3` / shader-struct 都是普通值，上传是复制（`glBufferData` 语义），复制完就是 GC 的事。
- 初始值用 `0`：GL 规定 delete 忽略名字 0，所以 `init` 半途失败时关窗也安全。
- 手写 raw 的对照见 `compare/raw-triangle.rkt`：用显式 `glDeleteProgram` / `glDeleteVertexArrays` / `glDeleteBuffers`，与各课的 `shutdown` 是同一件事。

判据一句话：**返回值是指向 GPU 侧分配的整数句柄 → 要删；是 Racket 值 → 不用管。**

---


## 4. 写作规则

以后新增或修改章节，按这套规则检查。

1. **先讲公式，再讲 API**  
   不要一上来列函数；先说明这一步要算什么。

2. **标出计算位置**  
   每章开头有一句“计算位置”：CPU / 顶点 / 片元 / 离线。

3. **给出数据流**  
   至少说清：谁产生数据、经过哪个对象、最后到哪里。

4. **给出 CPU 替代方案**  
   至少一句话：如果不用 GPU，CPU 会怎么算，代价是什么。

5. **API 只讲当前用例需要的**  
   不追求 API 大全；复习章节再集中整理。

6. **复习章节必须能运行**  
   复习代码要能直接 `racket` 跑，输出能验证公式。能出图就出图：同一场景的 CPU 图和 GPU 图比一串打印数字更有说服力。

7. **CPU 对照要标清边界**  
   只对「每个顶点 / 每个片元算一次」的公式做 CPU 先行；光栅化、深度测试、缓冲上传这些管线机制保持 GPU-first，不要为了对照重写一个软件渲染器（R4 是唯一例外，它是整条管线的参考实现）。

8. **避免“GPU 一定更快”**  
   小数据、串行、需要回读的场景，CPU 可能更合适。

9. **避免“逐片元一定更好”**  
   逐片元质量更好，但成本随像素数增长；静态场景用烘焙可能更划算。

---

## 5. 常见误区

| 误区 | 纠正 |
|---|---|
| “OpenGL 在算图形” | OpenGL 是通道；算的是 shader 或 CPU 代码 |
| “矩阵乘法只能在 GPU 上做” | CPU 也能做；GPU 只是并行执行 |
| “用了 OpenGL 就必须所有计算都上 GPU” | 文件解析、场景图、一次性计算通常留在 CPU |
| “GPU 一定比 CPU 快” | 数据量小、传输/启动开销大时，CPU 可能更快 |
| “逐片元光照一定比逐顶点好” | 质量更好，但成本更高；静态光照可烘焙 |
| “上传越少越好” | 要看数据是否每帧变化；有时传公式参数比传结果更省 |
| “CPU 对照就是打印几个数” | 同一场景的 CPU 图 + GPU 图、执行次数和耗时，才是可验证的对照（见 R4） |
| “API 背下来就会图形” | API 只是入口；核心是公式、计算位置、数据流 |

---

## 6. 术语速查

| 术语 | 含义 |
|---|---|
| attribute | 每个顶点一份的输入：位置、法线、UV |
| uniform | 一次 draw 内共享的输入：矩阵、光参数、材质 |
| varying | 顶点着色器 → 片元着色器的插值数据 |
| VBO | 顶点缓冲对象，存顶点字节 |
| EBO/IBO | 索引缓冲对象，存三角形索引 |
| VAO | 顶点数组对象，记录属性格式和 buffer 绑定 |
| program | 链接后的 shader 程序 |
| draw call | `gl-draw-*`，启动一次 GPU 计算 |
| readback | GPU → CPU 取回数据，通常很贵 |
| `vec-3` / `vec3` | Racket 侧名字由 racket-shader 提供（不是 `racket/math`）；生成 GLSL 时写成 `vec3` |
| Gouraud | 逐顶点光照，颜色插值 |
| Phong | 逐片元光照，法线/位置插值后逐像素算 |
| lightmap | 离线烘焙的光照贴图 |
| sprite sheet / atlas | 把多帧/多张小图排进同一张纹理；用 UV 格子取其中一块 |
| billboard | 永远面向相机的四边形，常用于精灵、文字、粒子 |

---

## 7. 非目标

- 不追求覆盖 OpenGL 全部 API。
- 不把教程写成 GPU 体系结构教材。
- 不展开 compute shader、CUDA、Vulkan、光线追踪。
- 不假设 GPU 永远优于 CPU。
- 不用“背 API”代替“理解公式和数据流”。

---

## 8. 一句话总结

> **先问“公式在哪算”，再问“数据怎么流”，最后才问“用哪个 API”。**
