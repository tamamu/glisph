(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :glisph)
    (defpackage glisph
      (:use :cl)
      (:import-from :glisph.shader
                    :+glyph-vs+
                    :+glyph-fs+
                    :+bounding-box-vs+
                    :+bounding-box-fs+)
      (:import-from :zpb-ttf
                    :open-font-loader)
      (:export :init
               :finalize
               :vglyph
               :make-glyph-table
               :regist-glyph
               :set-glyph-color
               :render-glyph
               :render-string
               :draw-string
               :open-font-loader
               :delete-glyph-table))))
(in-package :glisph)

(annot:enable-annot-syntax)

(defvar *glyph-program* nil)
(defvar *bounding-box-program* nil)
(defvar *glyph-color* nil)

; Glyph-table
; :font := zpb-ttf:font
; :em   := zpb-ttf:units/em font
; #\a   := vglyph

(defstruct vglyph
  (source nil :type zpb-ttf::glyph)
  (xmin 0.0 :type single-float)
  (ymin 0.0 :type single-float)
  (xmax 1.0 :type single-float)
  (ymax 1.0 :type single-float)
  (buffer nil)
  (box-buffer nil)
  (count 0 :type fixnum))

(defun create-program (vsource fsource)
  "Create GLSL program."
  (let ((program (gl:create-program))
        (vs (gl:create-shader :vertex-shader))
        (fs (gl:create-shader :fragment-shader)))
    (gl:shader-source vs vsource)
    (gl:compile-shader vs)
    (gl:shader-source fs fsource)
    (gl:compile-shader fs)
    (let ((vs-log (gl:get-shader-info-log vs))
          (fs-log (gl:get-shader-info-log fs)))
      (when (> (length vs-log) 0)
        (format t "Vertex shader: ~A~%" vs-log))
      (when (> (length fs-log) 0)
        (format t "Fragment shader: ~A~%" fs-log)))
    (gl:attach-shader program vs)
    (gl:attach-shader program fs)
    (gl:link-program program)
    (let ((program-log (gl:get-program-info-log program)))
      (when (> (length program-log) 0)
        (format t "Program log: ~A~%" program-log)))
    program))

(defun init ()
  "Initialize GLisph.
   Compile GLSL shaders to program."
  (setf *glyph-program* (create-program +glyph-vs+ +glyph-fs+))
  (gl:use-program *glyph-program*)
  (gl:bind-attrib-location *glyph-program* 0 "vertex")
  (gl:bind-attrib-location *glyph-program* 1 "attrib")
  (gl:use-program 0)

  (setf *bounding-box-program* (create-program +bounding-box-vs+ +bounding-box-fs+))
  (gl:use-program *bounding-box-program*)
  (gl:bind-attrib-location *bounding-box-program* 0 "vertex")
  (setf *glyph-color* (gl:get-uniform-location *bounding-box-program* "color"))
  (gl:uniformf *glyph-color* 0.0 0.0 0.0 1.0)
  (gl:use-program 0))

(defun finalize ()
  "Delete GLSL programs."
  (gl:delete-program *glyph-program*)
  (gl:delete-program *bounding-box-program*))

(defmacro make-gl-array (data)
  `(let* ((len (length ,data))
         (glarr (gl:alloc-gl-array :float len)))
    (dotimes (i len)
      (setf (gl:glaref glarr i) (aref ,data i)))
    glarr))

(defmacro vector-push-extend-to (vec &rest rest)
  `(loop for e in (list ,@rest)
         do (vector-push-extend e ,vec)))

(defun vertex-fill (glyph scale)
  "Make vertex of filled region of the glyph."
  (let ((polygon (make-array 0 :element-type 'single-float :fill-pointer 0 :adjustable t))
        (curve (make-array 0 :element-type 'single-float :fill-pointer 0 :adjustable t)))
    (zpb-ttf:do-contours (contour glyph)
      (let ((contour (zpb-ttf:explicit-contour-points contour)))
        (loop for i from 1 below (- (length contour) 1)
              for cp = (aref contour i)
              when (not (zpb-ttf:on-curve-p cp))
              do (let ((bp (aref contour (1- i)))
                       (np (aref contour (1+ i))))
                   (vector-push-extend-to curve
                     (float (/ (zpb-ttf:x bp) scale)) (float (/ (zpb-ttf:y bp) scale)) 0.0 0.0
                     (float (/ (zpb-ttf:x cp) scale)) (float (/ (zpb-ttf:y cp) scale)) 0.5 0.0
                     (float (/ (zpb-ttf:x np) scale)) (float (/ (zpb-ttf:y np) scale)) 1.0 1.0)))
        (let ((pv (make-array 0 :element-type 'single-float :fill-pointer 0 :adjustable t)))
          (loop for p across contour
                when (zpb-ttf:on-curve-p p)
                do (vector-push-extend-to pv
                     (float (/ (zpb-ttf:x p) scale)) (float (/ (zpb-ttf:y p) scale))))
          (let ((ox (aref pv 0))
                (oy (aref pv 1)))
            (loop for i from 2 below (- (length pv) 2) by 2
                  do (vector-push-extend-to polygon
                       ox oy 0.5 0.5
                       (aref pv i) (aref pv (+ i 1)) 0.5 0.5
                       (aref pv (+ i 2)) (aref pv (+ i 3)) 0.5 0.5))))))
      (concatenate 'vector polygon curve)))

(defmacro make-glyph-table (font)
  `(let ((tbl (make-hash-table)))
    (setf (gethash :font tbl) ,font
          (gethash :em tbl) (zpb-ttf:units/em ,font))
    tbl))

(defmacro regist-glyph-helper (table ch)
  `(let* ((glyph (zpb-ttf:find-glyph ,ch (gethash :font ,table)))
          (bbox (zpb-ttf:bounding-box glyph))
          (em (gethash :em ,table))
          (vertex (vertex-fill glyph em))
          (buffer (gl:gen-buffer))
          (box-buffer (gl:gen-buffer))
          (xmin (float (/ (zpb-ttf:xmin bbox) em)))
          (ymin (float (/ (zpb-ttf:ymin bbox) em)))
          (xmax (float (/ (zpb-ttf:xmax bbox) em)))
          (ymax (float (/ (zpb-ttf:ymax bbox) em))))
    (gl:bind-buffer :array-buffer buffer)
    (gl:buffer-data :array-buffer :static-draw (make-gl-array vertex))
    (gl:bind-buffer :array-buffer 0)
    (gl:bind-buffer :array-buffer box-buffer)
    (gl:buffer-data :array-buffer :static-draw
      (make-gl-array (vector xmin ymax xmin ymin xmax ymin xmax ymax)))
    (gl:bind-buffer :array-buffer 0)
    (setf (gethash ,ch ,table) (make-vglyph :source glyph
                                            :xmin xmin
                                            :ymin ymin
                                            :xmax xmax
                                            :ymax ymax
                                            :buffer buffer
                                            :box-buffer box-buffer
                                            :count (/ (length vertex) 4)))))

(defmacro regist-glyph (table ch)
  `(when (null (gethash ,ch ,table))
    (regist-glyph-helper ,table ,ch)))

(defun delete-glyph-table (table)
  "Delete font data and glyphs vertex."
  (zpb-ttf:close-font-loader (gethash :font table))
  #|
  (loop for key being each hash-key of table
        using (hash-value vg)
        when (typep key 'character)
        do (gl:delete-buffer (vglyph-buffer vg))
           (gl:delete-buffer (vglyph-box-buffer vg)))
  |#)

(defun set-glyph-color (r g b a)
  "Set render color of glyph."
  (gl:use-program *bounding-box-program*)
  (gl:uniformf *glyph-color* r g b a)
  (gl:use-program 0))

(defun render-glyph (vg)
  "Render the glyph."
  @type vglyph vg
  @optimize (speed 3)
  @optimize (safety 0)
  (gl:enable :stencil-test)
  (gl:enable :sample-alpha-to-coverage)
  (gl:stencil-func :always 0 1)
  (gl:stencil-op :keep :invert :invert)
  (gl:color-mask nil nil nil nil)
  (gl:use-program *glyph-program*)
  (gl:enable-vertex-attrib-array 0)
  (gl:enable-vertex-attrib-array 1)
  (gl:bind-buffer :array-buffer (vglyph-buffer vg))
  (gl:vertex-attrib-pointer 0 2 :float nil #.(* 4 4) 0)
  (gl:vertex-attrib-pointer 1 2 :float nil #.(* 4 4) #.(* 4 2))
  (gl:draw-arrays :triangles 0 (vglyph-count vg))
  (gl:disable-vertex-attrib-array 0)
  (gl:disable-vertex-attrib-array 1)
  (gl:use-program 0)
  (gl:disable :sample-alpha-to-coverage)
  (gl:stencil-func :notequal 0 1)
  (gl:stencil-op :keep :keep :keep)
  (gl:color-mask t t t t)
  (gl:use-program *bounding-box-program*)
  (gl:enable-vertex-attrib-array 0)
  (gl:bind-buffer :array-buffer (vglyph-box-buffer vg))
  (gl:vertex-attrib-pointer 0 2 :float nil 0 0)
  (gl:draw-arrays :triangle-fan 0 4)
  (gl:disable-vertex-attrib-array 0)
  (gl:use-program 0)
  (gl:disable :stencil-test))

(defun render-string (table str spacing)
  "Syntax sugar for rendering string in a single line."
  @type string str
  (gl:with-pushed-matrix
   (let ((em (gethash :em table)))
      (loop for i from 0 below (length str)
            for ch = (char str i)
            for cvg = (gethash ch table)
            for pvg = nil then cvg
            do (when (null cvg) (format t "~A is nil~%" ch))
            do (when pvg
                 (gl:translate (float (/ (- (zpb-ttf:kerning-offset
                                              (vglyph-source pvg)
                                              (vglyph-source cvg)
                                              (gethash :font table))) em))
                               0 0))
              (render-glyph cvg)
               (gl:translate
                 (+ spacing (float (/ (zpb-ttf:advance-width
                                        (vglyph-source cvg)) em)))
                 0.0 0.0)))))

(defun draw-string (table str &key (size nil sized-p)
                                   (color nil colored-p)
                                   (spacing 0.0))
  "Toy function to render string with set size, color, and spacing."
  (when colored-p
    (set-glyph-color (elt color 0) (elt color 1)
                     (elt color 2) (elt color 3)))
  (gl:with-pushed-matrix
    (when sized-p
      (gl:scale size size 1.0))
    (render-string table str spacing)))

