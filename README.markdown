# GLisph

![Screen Shot](screenshot.gif)

This is glyph rendering engine using OpenGL shaders.  
It adopt vector based rendering with quadratic bezier curve, so it can draw only TTF format.  

## Usage

First, initialize this library as below.

```lisp
(gli:init)
```

Load TTF font file and make glyph table.

```lisp
(defvar *font* (gli:open-font-loader "/path/to/display-font.ttf")
(defvar *glyph-table* (gli:make-glyph-table *font*))
```

Then you can regist and draw glyphs!

```lisp
;;;; in the case of CL-GLUT
;;;; before display-window section
;;;; :stencil and :multisample are required
(glut:set-display-mode :stencil :multisample)

;;;; before draw section
(gli:regist-glyph *glyph-table* #\G)
(gli:regist-glyph *glyph-table* #\l)

;;;; in draw section
(gli:gcolor 1.0 0.0 0.0 0.0)
(gli:gsize 2.0)
(gli:gtrans -400.0 300.0 0.0)
(gli:gscale 800.0 600.0 1.0)
(gli:grotate 0.0 0.0 0.5)
(gli:render-glyph (gethash #\G *glyph-table*))

;; or

(gli:draw-string *glyph-table* "Gl"
  150.0 -300.0 0.0
	:color '(1 0.5 0.25)
	:spacing 0.5)
```

## Dependencies

* cl-annot
* cl-opengl
* cl-glu
* zpb-ttf
* cl-glut (optional - only required when you test GLisph)

## Installation

```
$ git clone https://github.com/tamamu/glisph.git
```

```lisp
(require :glisph)
(asdf:test-system :glisph) ;; test
```

## Author

* Tamamu

## Copyright

Copyright (c) 2016 Tamamu

## License

Licensed under the MIT License.
