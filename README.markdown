# Glisph

This is glyph rendering engine using OpenGL shaders.  
It adopt vector based rendering with quadratic bezier curve, so it can draw only TTF format.  

## Usage

First, initialize this library as below.

```lisp
(glisph:init)
```

Load TTF font file and make glyph table.

```lisp
(defvar *font* (glisph:open-font-loader "NotoSans.ttf")
(defvar *glyph-table* (glisph:make-glyph-table *font*))
```

Then you can regist and draw glyphs!

```lisp
;; in the case of CL-GLUT
;; before display-window section
;; :stencil and :multisample are required
(glut:set-display-mode :stencil :multisample)

;; in draw section
(gl:translate -1.0 0.0 0.0)
(glisph:regist-glyph *glyph-table* #\A)
(glisph:render-glyph (gethash #\A *glyph-table*))
```

## Installation

## Author

* Tamamu

## Copyright

Copyright (c) 2016 Tamamu

## License

Licensed under the MIT License.
