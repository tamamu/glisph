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
      (:export :init
               :finalize
               :set-render-size
               :make-glyph-table
               :regist-glyphs
               :gcolor
               :gtrans
               :gscale
               :grotate
               :draw
               :render
               :delete-glyph-table))))
(in-package :glisph)

(cl-reexport:reexport-from :zpb-ttf
                           :include '(:open-font-loader))
(annot:enable-annot-syntax)

(defvar *render-width* 640.0)
(defvar *render-height* 480.0)

(defvar *glyph-program* nil)
(defvar *glyph-translation* nil)
(defvar *glyph-scale* nil)
(defvar *glyph-rotate* nil)
(defvar +glyph-vertex-loc+ nil)
(defvar +glyph-attrib-loc+ nil)
(defvar *bounding-box-program* nil)
(defvar *bounding-box-color* nil)
(defvar *bounding-box-translation* nil)
(defvar *bounding-box-scale* nil)
(defvar *bounding-box-rotate* nil)
(defvar +bounding-box-vertex-loc+ nil)

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
  (vertex nil :type array)
  (count 0 :type fixnum))

(defstruct context
  (glyph-table nil :type hash-table)
  (vertex (make-array 0 :element-type 'single-float :fill-pointer 0 :adjustable t) :type array)
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (size 10 :type fixnum)
  (letter-spacing 0 :type fixnum)
  (count 0 :type fixnum))

(defstruct text-buffer
  (width 0.0 :type float)
  (height 0.0 :type float)
  (polygon-buffer nil)
  (fill-buffer nil)
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

(defun init (&optional (width 640) (height 480))
  "Initialize GLisph engine.
   Please call this function before draw glyphs."
  (let ((imat (matrix4f 1.0 0.0 0.0 0.0
                        0.0 1.0 0.0 0.0
                        0.0 0.0 1.0 0.0
                        0.0 0.0 0.0 1.0)))
    (setf *glyph-program* (create-program +glyph-vs+ +glyph-fs+))
    (gl:use-program *glyph-program*)
    (setf +glyph-vertex-loc+ (gl:get-attrib-location *glyph-program* "vertex")
          +glyph-attrib-loc+ (gl:get-attrib-location *glyph-program* "attrib"))
    (setf *glyph-translation* (gl:get-uniform-location *glyph-program* "translationMatrix"))
    (gl:uniform-matrix-4fv *glyph-translation* imat)
    (setf *glyph-scale* (gl:get-uniform-location *glyph-program* "scaleMatrix"))
    (gl:uniform-matrix-4fv *glyph-scale* imat)
    (setf *glyph-rotate* (gl:get-uniform-location *glyph-program* "rotateMatrix"))
    (gl:uniform-matrix-4fv *glyph-rotate* imat)
    (gl:enable-vertex-attrib-array +glyph-vertex-loc+)
    (gl:enable-vertex-attrib-array +glyph-attrib-loc+)
    (gl:use-program 0)

    (setf *bounding-box-program* (create-program +bounding-box-vs+ +bounding-box-fs+))
    (gl:use-program *bounding-box-program*)
    (setf +bounding-box-vertex-loc+ (gl:get-attrib-location *bounding-box-program* "vertex"))
    (setf *bounding-box-color* (gl:get-uniform-location *bounding-box-program* "color"))
    (gl:uniformf *bounding-box-color* 0.0 0.0 0.0 1.0)
    (setf *bounding-box-translation* (gl:get-uniform-location *bounding-box-program* "translationMatrix"))
    (gl:uniform-matrix-4fv *bounding-box-translation* imat)
    (setf *bounding-box-scale* (gl:get-uniform-location *bounding-box-program* "scaleMatrix"))
    (gl:uniform-matrix-4fv *bounding-box-scale* imat)
    (setf *bounding-box-rotate* (gl:get-uniform-location *bounding-box-program* "rotateMatrix"))
    (gl:uniform-matrix-4fv *bounding-box-rotate* imat)
    (gl:enable-vertex-attrib-array +bounding-box-vertex-loc+)

    (gl:use-program 0)
    (set-render-size width height)
    t))

(defun finalize ()
  "Delete GLisph shader programs.
   Please call this function before exit program."
  (gl:delete-program *glyph-program*)
  (gl:delete-program *bounding-box-program*))

(defun set-render-size (width height)
  (setf *render-width* (float width)
        *render-height* (float height))
  (gscale 1 1 1)
  (gtrans 0.0 0.0 0.0))

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

@export
(defmacro %set-glyph-table (context table)
  `(setf (context-source ,context) ,table))

(defun %calc-kerning (context vglyph-1 vglyph-2)
  "Calc offsets of kerning and advance width between two glyphs."
  (let* ((tbl (context-glyph-table context))
        (font (gethash :font tbl))
        (em (gethash :em tbl)))
    (float (/ (- (zpb-ttf:kerning-offset (vglyph-source vglyph-1)
                                         (vglyph-source vglyph-2)
                                         font))
              em))))

(defun %calc-advance-width (context glyph)
  (float (/ (zpb-ttf:advance-width (vglyph-source glyph))
               (gethash :em (context-glyph-table context)))))

(defun %add-glyph (context vglyph x y)
  (let* ((cv   (context-vertex context))
         (size (context-size context))
         (gv   (vglyph-vertex vglyph))
         (gcnt (vglyph-count vglyph)))
    (loop for i from 0 below gcnt by 4
          do (vector-push-extend-to cv
             (+ (* size (aref gv i)) x)
             (+ (* size (- 1.0 (aref gv (+ i 1)))) y)
             (aref gv (+ i 2))
             (aref gv (+ i 3))))
    (incf (context-count context) gcnt)))

@export
(defun %draw-string (context str)
  (let ((tbl (context-glyph-table context))
        (x (float (context-x context)))
        (y (float (context-y context)))
        (size (float (context-size context)))
        (ls (float (context-letter-spacing context))))
    (loop for ch across str
          for vg = (gethash ch tbl)
          for pvg = nil then vg
          with aw = 0
          when pvg do (incf aw (* size (%calc-kerning context vg pvg)))
          do (%add-glyph context vg (+ x aw) y)
             (incf aw (+ ls (* size (%calc-advance-width context vg)))))))

@export
(defmacro %set-x (context x)
  `(setf (context-x ,context) ,x))

@export
(defmacro %set-y (context y)
  `(setf (context-y ,context) ,y))

@export
(defmacro %set-size (context size)
  `(setf (context-size ,context) ,size))

@export
(defmacro %set-letter-spacing (context width)
  `(setf (context-letter-spacing ,context) ,width))

@export
(defmacro draw (glyph-table proc-list)
  (let ((proc (list)))
    (loop for e in (eval proc-list)
          with cmd = nil
          if (and (keywordp e)
                  (not (null cmd)))
          do (push (reverse cmd) proc)
             (setf cmd nil)
          if (keywordp e)
          do (push (case e
                     (:x '%set-x)
                     (:y '%set-y)
                     (:size '%set-size)
                     (:spacing '%set-letter-spacing)
                     (:glyph-table '%set-glyph-table)
                     (:text '%draw-string))
                   cmd)
             (push '%context cmd)
          else
          do (push e cmd)
             (push (macroexpand (reverse cmd)) proc)
             (setf cmd nil)
          finally (unless (null cmd) (push (reverse cmd) proc)))
    `(let ((%context (make-context :glyph-table ,glyph-table)))
      ,@(reverse proc)
      (let ((xmax *render-width*)
            (ymax *render-height*)
            (polygon-buffer (gl:gen-buffer))
            (fill-buffer (gl:gen-buffer)))
        (gl:bind-buffer :array-buffer polygon-buffer)
        (gl:buffer-data :array-buffer :static-draw
                        (make-gl-array (context-vertex %context)))
        (gl:bind-buffer :array-buffer 0)
        (gl:bind-buffer :array-buffer fill-buffer)
        (gl:buffer-data :array-buffer :static-draw
                        (make-gl-array (vector 0.0 ymax 0.0 0.0 xmax ymax xmax 0.0)))
        (gl:bind-buffer :array-buffer 0)
        (make-text-buffer :polygon-buffer polygon-buffer
                          :fill-buffer fill-buffer
                          :width *render-width*
                          :height *render-height*
                          :count (/ (context-count %context) 4))))))

(defmacro make-glyph-table (font)
  "Make glyphs cache table."
  `(let ((tbl (make-hash-table :test 'eq)))
    (setf (gethash :font tbl) ,font
          (gethash :em tbl) (zpb-ttf:units/em ,font))
    tbl))

(defmacro regist-glyph-helper (table ch)
  `(let* ((glyph (zpb-ttf:find-glyph ,ch (gethash :font ,table)))
          (bbox (zpb-ttf:bounding-box glyph))
          (em (gethash :em ,table))
          (vertex (vertex-fill glyph em))
          (xmin (float (/ (zpb-ttf:xmin bbox) em)))
          (ymin (float (/ (zpb-ttf:ymin bbox) em)))
          (xmax (float (/ (zpb-ttf:xmax bbox) em)))
          (ymax (float (/ (zpb-ttf:ymax bbox) em))))
     (setf (gethash ,ch ,table) (make-vglyph :source glyph
                                             :vertex vertex
                                             :xmin xmin
                                             :ymin ymin
                                             :xmax xmax
                                             :ymax ymax
                                             :count (length vertex)))))

(defmacro regist-glyphs (table str)
  "Regist glyphs of the string to the glyph table."
  `(loop for ch across ,str
         when (null (gethash ch ,table))
         do (regist-glyph-helper ,table ch)))

(defun delete-glyph-table (table)
  "Delete font data from the glyph table."
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


(defvar *glyph-trans-mat*
  (matrix4f 1.0 0.0 0.0 0.0
            0.0 1.0 0.0 0.0
            0.0 0.0 1.0 0.0
            0.0 0.0 0.0 1.0))
(defun gtrans (x y z)
  "Set the translation matrix of glyphs."
  (setf (aref *glyph-trans-mat* 3) (+ (float (- (/ *render-width* 2))) x)
        (aref *glyph-trans-mat* 7) (- (float (/ *render-height* 2)) y)
        (aref *glyph-trans-mat* 11) z)
  (gl:use-program *glyph-program*)
  (gl:uniform-matrix-4fv *glyph-translation* *glyph-trans-mat*)
  (gl:use-program 0)
  (gl:use-program *bounding-box-program*)
  (gl:uniform-matrix-4fv *bounding-box-translation* *glyph-trans-mat*)
  (gl:use-program 0))

(defvar *glyph-scale-mat*
  (matrix4f 1.0 0.0 0.0 0.0
            0.0 1.0 0.0 0.0
            0.0 0.0 1.0 0.0
            0.0 0.0 0.0 1.0))
(defun gscale (x y z)
  "Set the scale matrix of glyphs."
  (setf (aref *glyph-scale-mat* 0) (float (/ 1 (* x (/ *render-width* 2))))
        (aref *glyph-scale-mat* 5) (float (/ 1 (* y (/ *render-height* 2))))
        (aref *glyph-scale-mat* 10)(float (/ 1 z)))
    (gl:use-program *glyph-program*)
    (gl:uniform-matrix-4fv *glyph-scale* *glyph-scale-mat*)
    (gl:use-program 0)
    (gl:use-program *bounding-box-program*)
    (gl:uniform-matrix-4fv *bounding-box-scale* *glyph-scale-mat*)
    (gl:use-program 0))

(defvar *glyph-rotate-mat*
  (matrix4f 1.0 0.0 0.0 0.0
            0.0 1.0 0.0 0.0
            0.0 0.0 1.0 0.0
            0.0 0.0 0.0 1.0))
(defun grotate (x y z)
  "Set the rotate matrix of glyphs."
  (setf (aref *glyph-rotate-mat* 0) (* (cos y) (cos z))
        (aref *glyph-rotate-mat* 1) (- (sin z))
        (aref *glyph-rotate-mat* 2) (sin y)
        (aref *glyph-rotate-mat* 4) (sin z)
        (aref *glyph-rotate-mat* 5) (* (cos x) (cos z))
        (aref *glyph-rotate-mat* 6) (- (sin x))
        (aref *glyph-rotate-mat* 8) (- (sin y))
        (aref *glyph-rotate-mat* 9) (sin x)
        (aref *glyph-rotate-mat* 10) (* (cos x) (cos y)))
    (gl:use-program *glyph-program*)
    (gl:uniform-matrix-4fv *glyph-rotate* *glyph-rotate-mat*)
    (gl:use-program 0)
    (gl:use-program *bounding-box-program*)
    (gl:uniform-matrix-4fv *bounding-box-rotate* *glyph-rotate-mat*)
    (gl:use-program 0))

(defun render (buffer)
  "Render the text buffer."
  @type text-buffer buffer
  @optimize (speed 3)
  @optimize (safety 0)
  @optimize (debug 0)
  (gl:enable :stencil-test)
  (gl:enable :sample-alpha-to-coverage)
  (gl:stencil-func :always 0 1)
  (gl:stencil-op :keep :invert :invert)
  (gl:color-mask nil nil nil nil)
  (gl:use-program *glyph-program*)
;  (gl:enable-vertex-attrib-array 0)
;  (gl:enable-vertex-attrib-array 1)
  (gl:bind-buffer :array-buffer (text-buffer-polygon-buffer buffer))
  (gl:vertex-attrib-pointer +glyph-vertex-loc+ 2 :float nil 16 0)
  (gl:vertex-attrib-pointer +glyph-attrib-loc+ 2 :float nil 16 8)
  (gl:draw-arrays :triangles 0 (text-buffer-count buffer))
;  (gl:disable-vertex-attrib-array 0)
;  (gl:disable-vertex-attrib-array 1)
  ;(gl:use-program 0)
  (gl:disable :sample-alpha-to-coverage)
  (gl:stencil-func :notequal 0 1)
  (gl:stencil-op :keep :keep :keep)
  (gl:color-mask t t t t)
  (gl:use-program *bounding-box-program*)
;  (gl:enable-vertex-attrib-array 0)
  (gl:bind-buffer :array-buffer (text-buffer-fill-buffer buffer))
  (gl:vertex-attrib-pointer +bounding-box-vertex-loc+ 2 :float nil 0 0)
  (gl:draw-arrays :triangle-strip 0 4)
;  (gl:disable-vertex-attrib-array 0)
  (gl:use-program 0)
  (gl:disable :stencil-test))

