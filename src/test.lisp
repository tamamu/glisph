
(require 'asdf)
(require 'cffi)
(require 'cl-opengl)
(require 'cl-glu)
(require 'cl-glut)
(require 'zpb-ttf)
(require 'cl-annot)

(annot:enable-annot-syntax)

;; Glyph vertex
(defvar +vs-source+ "#version 130
precision mediump float;
attribute vec2 vertex;
attribute vec2 attrib;
uniform mat4 mvpMatrix;
varying vec2 p;
void main(void) {
  gl_Position = gl_ModelViewMatrix * vec4(vertex, 0.0, 1.0);
  p = attrib;
}
")

(defvar +fs-source+ "#version 130

precision mediump float;
varying vec2 p;
void main(void) {
    vec2 px = dFdx(p);
    vec2 py = dFdy(p);
    float fx = (2.0*p.x)*px.x - px.y;
    float fy = (2.0*p.x)*py.x - py.y;
    float sd = (p.x*p.x - p.y)/sqrt(fx*fx + fy*fy);
    float alpha = 0.5 - sd;
    if (alpha > 1.0) {
        // inside
        gl_FragColor = vec4(1.0);
    } else if (alpha < 0.0) {
        // outside
        discard;
    } else {
        // near boundary
        gl_FragColor = vec4(alpha);
    }
}")

;; Bounding box
(defvar +vs-source2+ "#version 130
precision mediump float;
attribute vec2 vertex;
uniform mat4 mvpMatrix;
void main(void) {
    gl_Position = gl_ModelViewMatrix * vec4(vertex, 0.0, 1.0);
}
")

(defvar +fs-source2+ "#version 130
uniform vec4 color;
void main(void) {
    gl_FragColor = color;
}
")

(defvar *window-width* 640)
(defvar *window-height* 480)
(defvar *font*)
(defvar *mouse-x* 0)
(defvar *mouse-y* 0)
(defvar *zoom* 1.0)
(defvar *program* nil)
(defvar *program2* nil)

(defvar *uniform-color* nil)
(defvar *uniform-mvp1* nil)
(defvar *uniform-mvp2* nil)

(defun create-program (vsource fsource)
  (let ((program (gl:create-program))
        (vs (gl:create-shader :vertex-shader))
        (fs (gl:create-shader :fragment-shader)))
    (gl:shader-source vs vsource)
    (gl:compile-shader vs)
    (gl:shader-source fs fsource)
    (gl:compile-shader fs)
    (eval-when (:execute)
      (print (gl:get-shader-info-log vs))
      (print (gl:get-shader-info-log fs)))
    (gl:attach-shader program vs)
    (gl:attach-shader program fs)
    (gl:link-program program)
    (print (gl:get-program-info-log program))
    program))


(defclass main-window (glut:window)
  ()
  (:default-initargs :width *window-width*
                     :height *window-height*
                     :title "bezier.lisp"
                     :mode '(:single :rgb :stencil :multisample)
                     :tick-interval #.(round 1000 60)))

(defmethod glut:reshape ((w main-window) width height)
  (setf *window-width* width
        *window-height* height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  ;(glu:ortho-2d 0 width 0 height)
  (glu:perspective *zoom* (float (/ *window-width* *window-height*)) 0.01 10)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (glu:look-at 0 0 1
               0 0 0
               0 1 0)
  )

(defun make-gl-array (data)
  (let* ((len (length data))
         (glarr (gl:alloc-gl-array :float len)))
    (dotimes (i len)
      (setf (gl:glaref glarr i) (aref data i)))
    glarr))

(defmacro vector-push-extend-to (vec &rest rest)
  `(loop for e in (list ,@rest)
         do (vector-push-extend e ,vec)))

(defun mvp-matrix (x y z mx my mz)
  (make-array 16 :element-type 'single-float
                 :initial-contents `(,mx 0.0 0.0  ,x
                                     0.0 ,my 0.0  ,y
                                     0.0 0.0 ,mz  ,z
                                     0.0 0.0 0.0 1.0)))

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


(defun vertex-fill (glyph scale)
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

(defun set-glyph-color (r g b a)
  (gl:use-program *program2*)
  (gl:uniformf *uniform-color* r g b a)
  (gl:use-program 0))

(defun render-glyph (vg)
  @type vglyph vg
  @optimize (speed 3)
  @optimize (safety 0)
  (gl:enable :stencil-test)
  (gl:enable :sample-alpha-to-coverage)
  (gl:stencil-func :always 0 1)
  (gl:stencil-op :keep :invert :invert)
  (gl:color-mask nil nil nil nil)
  (gl:use-program *program*)
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
  (gl:use-program *program2*)
  (gl:enable-vertex-attrib-array 0)
  (gl:bind-buffer :array-buffer (vglyph-box-buffer vg))
  (gl:vertex-attrib-pointer 0 2 :float nil 0 0)
  (gl:draw-arrays :triangle-fan 0 4)
  (gl:disable-vertex-attrib-array 0)
  (gl:use-program 0)
  (gl:disable :stencil-test))

(defun render-string (table str spacing)
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

(defun draw-string (table str &key (size 1.0)
                                   (color '(1.0 1.0 1.0 1.0))
                                   (spacing 0.0))
  (set-glyph-color (elt color 0) (elt color 1)
                   (elt color 2) (elt color 3))
  (gl:with-pushed-matrix
    (gl:scale size size 1.0)
    (render-string table str spacing)))

(defvar takao-gothic nil)

(defmethod glut:display-window :before ((w main-window))
  (setf *font*
    (zpb-ttf:open-font-loader "/usr/share/fonts/OTF/TakaoGothic.ttf"))
  (setf takao-gothic (make-glyph-table *font*))
  (loop for ch across "新しい朝が来た　希望の朝だ喜びに胸を開け大空あおげラジオの声に健やかな胸をこの香る風に開けよそれ一ニ三"
        do (regist-glyph takao-gothic ch))
  (gl:shade-model :flat)
  (setf *program* (create-program +vs-source+ +fs-source+))
  (gl:use-program *program*)
  (gl:bind-attrib-location *program* 0 "vertex")
  (gl:bind-attrib-location *program* 1 "attrib")
  (gl:use-program 0)

  (setf *program2* (create-program +vs-source2+ +fs-source2+))
  (gl:use-program *program2*)
  (gl:bind-attrib-location *program2* 0 "vertex")
  (setf *uniform-color* (gl:get-uniform-location *program2* "color"))
  (gl:uniformf *uniform-color* 1.0 0.0 0.0 1.0)
  (gl:use-program 0))



(defmethod glut:tick ((w main-window))
  (glut:post-redisplay))

(defmethod glut:idle ((w main-window))
  (sleep (/ 1.0 60.0))
  (glut:post-redisplay))

(defmethod glut:passive-motion ((w main-window) x y)
  (declare (ignore w))
  (setf *mouse-x* x *mouse-y* y))

(defvar *click* nil)
(defvar *zoom-delta* *zoom*)
(defmethod glut:mouse ((w main-window) button state x y)
  (declare (ignore w x y))
  (case button
    (:left-button
      (if (eq state :down)
        (setf *click* t)
        (setf *click* nil)))
    (:wheel-down
      (setf *zoom* (/ *zoom* 1.2))
      (print *zoom*)
      (glut:post-redisplay))
    (:wheel-up
      (setf *zoom* (* *zoom* 1.2))
      (print *zoom*)
      (glut:post-redisplay))))


(defmethod glut:close ((w main-window))
  (zpb-ttf:close-font-loader *font*)
  (gl:delete-program *program*)
  (gl:delete-program *program2*)
)

(defmacro signeq (a b)
  `(or (and (plusp ,a) (plusp ,b))
       (and (minusp ,a) (minusp ,b))))

(defmethod glut:display ((w main-window))
  (gl:clear-color 0.2 0.7 0.5 1.0)
  (gl:clear-stencil 0)
  (gl:clear :color-buffer-bit :stencil-buffer-bit)
  (gl:enable :normalize)

  (if (/= *zoom* *zoom-delta*)
    (let* ((d (- *zoom* *zoom-delta*))
           (ad (abs d)))
      (when (> ad 2.0)
        (setf d (* (/ d ad) 2.0)))
      (setf *zoom-delta* (* *zoom-delta* (expt 1.2 d)))
      (let ((nd (- *zoom* *zoom-delta*)))
        (unless (signeq d nd)
          (setf *zoom-delta* *zoom*)))))

  (gl:with-pushed-matrix
    (gl:translate -1.0 0.8 0.0)
    (gl:scale *zoom-delta* *zoom-delta* 0.0)
    (let ((size 0.15))
      (draw-string takao-gothic "新" :size size)
      (gl:translate 0.0 -0.2 0.0)
      (draw-string takao-gothic "新しい朝が来た　希望の朝だ" :size size)
      (gl:translate 0.0 -0.2 0.0)
      (draw-string takao-gothic "喜びに胸を開け　大空あおげ" :size size)
      (gl:translate 0.0 -0.2 0.0)
      (draw-string takao-gothic "ラジオの声に　健やかな胸を" :size size)
      (gl:translate 0.0 -0.2 0.0)
      (draw-string takao-gothic "この香る風に　開けよ" :size size)
      (gl:translate 0.0 -0.2 0.0)
      (draw-string takao-gothic "それ　一　ニ　三" :size size)))






  (gl:flush))

(sb-profile:profile "COMMON-LISP-USER")
(glut:display-window (make-instance 'main-window))
