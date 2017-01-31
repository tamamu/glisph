# GLisph

[![Quicklisp](http://quickdocs.org/badge/glisph.svg)](http://quickdocs.org/glisph/)

![Screen Shot](screenshot.gif)

GLisph is a glyph rendering engine using OpenGL shader language. The engine draws string clearly by vector based font rendering. Currently, TrueType (TTF) is the only format that can draw.

## Usage

First, initialize the engine as below.

```lisp
(gli:init)
```

Load TrueType font file and make glyph table.  
Glyph table manages the contour points of the glyphs.

```lisp
(defvar *font* (gli:open-font-loader "/path/to/display-font.ttf")
(defvar *glyph-table* (gli:make-glyph-table *font*))
```

Then you can regist and draw glyphs!

```lisp
;; For CL-GLUT
;; Before display-window section
;; These display mode keywords are required
(glut:set-display-mode :stencil :multisample)

;; Before draw section
(defvar *text-object* (gli:regist-glyphs *glyph-table* "Hello World!" 0.0))

;; In draw section
;; Viewport settings for GLisph context
(gli:gscale (/ width 2) (/ height 2) 1.0)

;; Rotate
(gli:grotate 0.0 0.0 0.5)

;; Draw
(gli:draw-string *text-object*
  -400.0 300.0 0.0    ; x y z
  32.0                ; size
  :color '(1 1 1 1))  ; '(r g b a)

```

## Dependencies

* cl-annot
* cl-opengl
* cl-glu
* zpb-ttf
* cl-glut (optional - only required when you test GLisph)

## Installation

* Quicklisp

```lisp
(ql:quickload :glisph)
```

* Roswell

```bash
$ ros install glisph
```

* Test

```lisp
(asdf:test-system :glisph)
```

## Author

* Tamamu

## Copyright

Copyright (c) 2017 Tamamu

## License

Licensed under the MIT License.
