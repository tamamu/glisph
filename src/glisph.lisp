(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :glisph)
    (defpackage glisph (:nicknames :gli)
      (:documentation "Glyph rendering engine using OpenGL shading language")
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
               :gcolor
               :gtrans
               :gscale
               :grotate
               :render-glyph
               :render-string
               :draw-string
               :open-font-loader
               :delete-glyph-table))))
(in-package :glisph)

(annot:enable-annot-syntax)

(defvar *glyph-program* nil)
(defvar *glyph-size* nil)
(defvar *glyph-translation* nil)
(defvar *glyph-scale* nil)
(defvar *glyph-rotate* nil)
(defvar *bounding-box-program* nil)
(defvar *bounding-box-size* nil)
(defvar *bounding-box-color* nil)
(defvar *bounding-box-translation* nil)
(defvar *bounding-box-scale* nil)
(defvar *bounding-box-rotate* nil)

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

(defun matrix4f (a b c d e f g h i j k l m n o p)
  (make-array 16 :element-type 'single-float
                 :initial-contents `(,a ,b ,c ,d
                                     ,e ,f ,g ,h
                                     ,i ,j ,k ,l
                                     ,m ,n ,o ,p)))

(defun init ()
  "Initialize GLisph engine.
   Please call this function before draw glyphs."
  (let ((imat (matrix4f 1.0 0.0 0.0 0.0
                        0.0 1.0 0.0 0.0
                        0.0 0.0 1.0 0.0
                        0.0 0.0 0.0 1.0)))
    (setf *glyph-program* (create-program +glyph-vs+ +glyph-fs+))
    (gl:use-program *glyph-program*)
    (gl:bind-attrib-location *glyph-program* 0 "vertex")
    (gl:bind-attrib-location *glyph-program* 1 "attrib")
    (setf *glyph-size* (gl:get-uniform-location *glyph-program* "sizeMatrix"))
    (gl:uniform-matrix-4fv *glyph-size* imat)
    (setf *glyph-translation* (gl:get-uniform-location *glyph-program* "translationMatrix"))
    (gl:uniform-matrix-4fv *glyph-translation* imat)
    (setf *glyph-scale* (gl:get-uniform-location *glyph-program* "scaleMatrix"))
    (gl:uniform-matrix-4fv *glyph-scale* imat)
    (setf *glyph-rotate* (gl:get-uniform-location *glyph-program* "rotateMatrix"))
    (gl:uniform-matrix-4fv *glyph-rotate* imat)
    (gl:use-program 0)

    (setf *bounding-box-program* (create-program +bounding-box-vs+ +bounding-box-fs+))
    (gl:use-program *bounding-box-program*)
    (gl:bind-attrib-location *bounding-box-program* 0 "vertex")
    (setf *bounding-box-color* (gl:get-uniform-location *bounding-box-program* "color"))
    (gl:uniformf *bounding-box-color* 0.0 0.0 0.0 1.0)
    (setf *bounding-box-size* (gl:get-uniform-location *bounding-box-program* "sizeMatrix"))
    (gl:uniform-matrix-4fv *bounding-box-size* imat)
    (setf *bounding-box-translation* (gl:get-uniform-location *bounding-box-program* "translationMatrix"))
    (gl:uniform-matrix-4fv *bounding-box-translation* imat)
    (setf *bounding-box-scale* (gl:get-uniform-location *bounding-box-program* "scaleMatrix"))
    (gl:uniform-matrix-4fv *bounding-box-scale* imat)
    (setf *bounding-box-rotate* (gl:get-uniform-location *bounding-box-program* "rotateMatrix"))
    (gl:uniform-matrix-4fv *bounding-box-rotate* imat)

    (gl:use-program 0)
    t))

(defun finalize ()
  "Delete GLisph shader programs.
   Please call this function before exit program."
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
  "Make glyphs cache table."
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
  "Regist a glyph of the character to glyph table."
  `(when (null (gethash ,ch ,table))
    (regist-glyph-helper ,table ,ch)))

(defun delete-glyph-table (table)
  "Delete font data from the table."
  (zpb-ttf:close-font-loader (gethash :font table))
  #|
  (loop for key being each hash-key of table
        using (hash-value vg)
        when (typep key 'character)
        do (gl:delete-buffer (vglyph-buffer vg))
           (gl:delete-buffer (vglyph-box-buffer vg))
  |#)

(defun gcolor (r g b a)
  "Set render color of glyphs."
  (gl:use-program *bounding-box-program*)
  (gl:uniformf *bounding-box-color* r g b a)
  (gl:use-program 0))

(defun gsize (size)
  "Set the size matrix of glyphs."
  (let ((mat (matrix4f size  0.0  0.0 0.0
                        0.0 size  0.0 0.0
                        0.0  0.0  1.0 0.0
                        0.0  0.0  0.0 1.0)))
    (gl:use-program *glyph-program*)
    (gl:uniform-matrix-4fv *glyph-size* mat)
    (gl:use-program 0)
    (gl:use-program *bounding-box-program*)
    (gl:uniform-matrix-4fv *bounding-box-size* mat)
    (gl:use-program 0)))

(defun gtrans (x y z)
  "Set the translation matrix of glyphs."
  (let ((mat (matrix4f 1.0 0.0 0.0 x
                       0.0 1.0 0.0 y
                       0.0 0.0 1.0 z
                       0.0 0.0 0.0 1.0)))
    (gl:use-program *glyph-program*)
    (gl:uniform-matrix-4fv *glyph-translation* mat)
    (gl:use-program 0)
    (gl:use-program *bounding-box-program*)
    (gl:uniform-matrix-4fv *bounding-box-translation* mat)
    (gl:use-program 0)))

(defun gscale (x y z)
  "Set the scale matrix of glyphs."
  (let ((mat (matrix4f  x  0.0 0.0 0.0
                       0.0  y  0.0 0.0
                       0.0 0.0  z  0.0
                       0.0 0.0 0.0 1.0)))
    (gl:use-program *glyph-program*)
    (gl:uniform-matrix-4fv *glyph-scale* mat)
    (gl:use-program 0)
    (gl:use-program *bounding-box-program*)
    (gl:uniform-matrix-4fv *bounding-box-scale* mat)
    (gl:use-program 0)))

(defun grotate (x y z)
  "Set the rotate matrix of glyphs."
  (let ((mat (matrix4f (* (cos y) (cos z)) (- (sin z)) (sin y) 0.0
                       (sin z) (* (cos x) (cos z)) (- (sin x)) 0.0
                       (- (sin y)) (sin x) (* (cos x) (cos y)) 1.0
                       0.0 0.0 0.0 1.0)))
    (gl:use-program *glyph-program*)
    (gl:uniform-matrix-4fv *glyph-rotate* mat)
    (gl:use-program 0)
    (gl:use-program *bounding-box-program*)
    (gl:uniform-matrix-4fv *bounding-box-rotate* mat)
    (gl:use-program 0)))

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

(defun render-string (table str spacing x y z size)
  "Syntax sugar for rendering string in a single line."
  @type string str
  @type single-float x
  @type single-float y
  @type single-float z
  @type single-float size
  (let ((em (gethash :em table)))
    (loop for i from 0 below (length str)
          for ch = (char str i)
          for cvg = (gethash ch table)
          for pvg = nil then cvg
          do (when pvg
               (incf x (float (* size (/ (- (zpb-ttf:kerning-offset
                                              (vglyph-source pvg)
                                              (vglyph-source cvg)
                                              (gethash :font table)))
                                              em)))))
             (gtrans x y z)

          if (null cvg)
          do (format t "~A is not registed~%" ch)
          else
          do (render-glyph cvg)
             (incf x (* size
                        (+ spacing (float (/ (zpb-ttf:advance-width
                                               (vglyph-source cvg)) em))))))))

(defun draw-string (table str x y z size &key (color nil colored-p)
                                              (spacing 0.0))
  "Toy function to render string with set size, color, and spacing."
  (when colored-p
    (gcolor (elt color 0) (elt color 1)
            (elt color 2) (elt color 3)))
  (gsize size)
  (render-string table str spacing x y z size))

