# GLisph

[![Quicklisp](http://quickdocs.org/badge/glisph.svg)](http://quickdocs.org/glisph/)

![Screen Shot](screenshot.gif)

GLisph is a glyph rendering engine using OpenGL shader language. The engine draws string clearly by vector based font rendering on an OpenGL context. TrueType (TTF) is the only drawable format currently.

## Usage

Initialize the engine as below first.

```lisp
(gli:init 800 600)
```

Load TrueType font file and make glyph table. Glyph table manages the contour points of the glyphs.

```lisp
(defvar *font* (gli:open-font-loader "/path/to/display-font.ttf")
(defvar *glyph-table* (gli:make-glyph-table *font*))
```

Then you can regist and draw glyphs!

```lisp
;;; For CL-GLUT, you should require these display mode keywords before display-window section.
(glut:set-display-mode :stencil :multisample)

;; Before draw section
(defvar *text-buffer*
  (gli:draw *glyph-table*
	  '(:x 120 :y 40 :size 20
	    :text "Hello World!"
			:y 64
			:text "Common Lisp")))

;;; In draw section, you can set some parameters to draw for GLisph context.
(gli:gcolor 0.8 0.2 0.5)

;;; This is rotation example.
(gli:grotate 0.0 0.0 0.5)

;; Render
(gli:render *text-buffer*)

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
