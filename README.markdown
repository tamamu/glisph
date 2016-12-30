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
;; In the case of CL-GLUT
;; Before display-window section
;; :stencil and :multisample are required
(glut:set-display-mode :stencil :multisample)

;; Before draw section
;; (gli:regist-glyphs glyph-table text spacing)
(defvar *text-object* (gli:regist-glyphs *glyph-table* "Hello World!" 0.0))


;; In draw section
;; Viewport settings for GLisph
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
