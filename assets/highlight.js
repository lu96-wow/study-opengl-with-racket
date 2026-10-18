/* study-opengl · Racket 语法高亮
   代码块里放的是纯文本（HTML 实体已转义过的），所以这里读 textContent，
   自己转义后重建 innerHTML。TOKEN 的候选顺序就是优先级：注释 → 字符串 →
   # 字面量 → 数字 → 其它标识符；前面几类一旦吃掉一段，后面不会再切进去。 */

(function () {
  'use strict';

  var KEYWORDS = [
    /* Racket 核心 */
    'define', 'define-values', 'define/override', 'define/augment', 'lambda',
    'let', 'let*', 'set!', 'if', 'when', 'unless', 'cond', 'for', 'for*',
    'require', 'provide', 'provide/contract', 'class', 'class*', 'new', 'send',
    'super-new', 'inherit', 'this', 'inner', 'void', 'error', 'exit', 'printf',
    'displayln', 'list', 'list*', 'cons', 'car', 'cdr', 'cadr', 'values',
    'begin', 'struct', 'quote', 'quasiquote', 'unquote',
    /* racket-shader 与本项目高频的 GL 名字 */
    'glsl', 'gl-vertex-shader', 'gl-fragment-shader', 'build-program',
    'program-use', 'uniform-location', 'vec-array', 'vec-array-data',
    'concat-vecs', 'vec2', 'vec3', 'vec4', 'mat4', 'glsl-size',
    'glsl-byte-size', 'glsl-stride-bytes', 'glsl-struct', 'gl-vector-sizeof',
    'make-window', 'gl-viewport', 'gl-clear', 'gl-clear-color', 'gl-gen-buffers',
    'gl-bind-buffer', 'gl-buffer-data', 'gl-gen-vertex-arrays',
    'gl-bind-vertex-array', 'gl-vertex-attrib-pointer',
    'gl-enable-vertex-attrib-array', 'gl-draw-arrays', 'gl-draw-elements',
    'gl-gen-textures', 'gl-bind-texture', 'gl-tex-parameter-i', 'gl-tex-image-2d',
    'gl-generate-mipmap', 'gl-active-texture', 'gl-uniform-1f', 'gl-uniform-1i'
  ];

  var KW = Object.create(null);
  for (var i = 0; i < KEYWORDS.length; i++) {
    KW[KEYWORDS[i]] = true;
  }

  var TOKEN = /(;[^\n]*)|("(?:\\.|[^"\\\n])*")|(#[tf]\b|#\\(?:[^\s()\[\]{}";]+|[^\s])|#'|#`|#,|#lang\b)|(-?\d+(?:\.\d+)?)|([^\s()\[\]{}'`,;"]+)/g;

  function esc(s) {
    return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  }

  function wrap(cls, s) {
    return '<span class="' + cls + '">' + esc(s) + '</span>';
  }

  function isKeyword(w) {
    return w.indexOf('gl-') === 0 || KW[w] === true;
  }

  function highlight(src) {
    var out = '';
    var last = 0;
    var m;
    TOKEN.lastIndex = 0;
    while ((m = TOKEN.exec(src)) !== null) {
      if (m.index > last) {
        out += esc(src.slice(last, m.index));
      }
      if (m[1]) {
        out += wrap('tok-c', m[1]);
      } else if (m[2]) {
        out += wrap('tok-s', m[2]);
      } else if (m[3]) {
        out += wrap('tok-p', m[3]);
      } else if (m[4]) {
        out += wrap('tok-n', m[4]);
      } else if (isKeyword(m[5])) {
        out += wrap('tok-k', m[5]);
      } else {
        out += esc(m[5]);
      }
      last = m.index + m[0].length;
    }
    return out + esc(src.slice(last));
  }

  function run() {
    var blocks = document.querySelectorAll('pre code');
    for (var i = 0; i < blocks.length; i++) {
      var code = blocks[i];
      if (code.getAttribute('data-hl')) {
        continue;
      }
      var src = code.textContent;
      if (/\S/.test(src)) {
        code.innerHTML = highlight(src);
      }
      code.setAttribute('data-hl', '1');
    }
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', run);
  } else {
    run();
  }

  /* 源码查看页（code.html）在 fetch 拿到内容后要手动高亮一次，
     所以把 highlight 暴露出去；传入的 src 是未转义的纯文本。 */
  window.highlightRacket = highlight;
})();