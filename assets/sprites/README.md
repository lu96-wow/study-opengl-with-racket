# sprites

| 文件 | 来源 | 说明 |
|---|---|---|
| `racket-sigil.png` | `assets/sprites/make-sprites.rkt` 生成 | 1024×1024，4 列 × 4 行，每帧 256×256，共 16 帧；帧 0 在左上角 |

图形是一个透明的发光 Racket λ 符印：射线旋转、核心呼吸、外圈脉冲。15 课用 additive blending 把它叠到 3D 场景里。

重新生成：

```sh
racket assets/sprites/make-sprites.rkt
```

改颜色、帧数或网格时，记得同步 `15-sprite-atlas/sprite-atlas.rkt` 里的 `sprite-cols` / `sprite-rows` 和片元里的图集 UV 公式。
