(in-package :cl-user)
(defpackage glisph-test
  (:use :cl
        :prove))
(in-package :glisph-test)

;; NOTE: To run this test file, execute `(asdf:test-system :glisph)' in your Lisp.

(defvar *width* 800)
(defvar *height* 600)
(defvar *font*)
(defvar *glyph-table*)
(defvar *origin-x* 0.0)
(defvar *origin-y* 0.0)
(defvar *display-x* 0.0)
(defvar *display-y* 0.0)
(defvar *zoom* 1.0)
(defvar *frame-count* 0)

(defclass test-window (glut:window)
  ()
  (:default-initargs :title "GLisphTest"
                     :width *width* :height *height*
                     :mode '(:stencil :multisample)
                     :tick-interval (round (/ 1000 60))))

(defmethod glut:mouse ((w test-window) button state x y)
  (declare (ignore w state))
  (case button
    (:left-button
      (setf *origin-x* (- x *display-x*)
            *origin-y* (- y *display-y*)))
    (:wheel-down
      (setf *zoom* (/ *zoom* 1.2))
      (glut:post-redisplay))
    (:wheel-up
      (setf *zoom* (* *zoom* 1.2))
      (glut:post-redisplay))))

(defmethod glut:motion ((w test-window) x y)
  (setf *display-x* (- x *origin-x*)
        *display-y* (- y *origin-y*))
  (glut:post-redisplay))

(defmethod glut:reshape ((w test-window) width height)
  (gl:viewport *display-x* (- *display-y*) width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 width height 0 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (setf *width* width
        *height* height))

(defmethod glut:display-window :before ((w test-window))
  (setf *font*
    (gli:open-font-loader "/usr/share/fonts/OTF/TakaoGothic.ttf"))
    ;(gli:open-font-loader "/usr/share/fonts/TTF/RictyDiscord-Regular.ttf"))
  (setf *glyph-table* (gli:make-glyph-table *font*))
  (loop for ch across "Hello World!
  The quick brown fox jumps over the lazy dog.色は匂へと　散りぬるを"
        do (gli:regist-glyph *glyph-table* ch))
  (gli:init))

(defmethod glut:tick ((w test-window))
  (incf *frame-count*)
  (when (>= *frame-count* 360)
    (setf *frame-count* 0))
  (glut:post-redisplay))

(defmethod glut:display ((w test-window))
  (gl:viewport *display-x* (- *display-y*) *width* *height*)
  (gl:clear-color 0 0 0 1)
  (gl:clear-stencil 0)
  (gl:clear :color-buffer-bit :stencil-buffer-bit)
  (gl:color 0.5 0.0 0.0 1.0)
  (gl:with-primitive :quads
    (gl:vertex 0 0)
    (gl:vertex 300 0)
    (gl:vertex 300 300)
    (gl:vertex 0 300))
  (gli:gscale (float (* *zoom* (/ 2 *width*)))
              (float (* *zoom* (/ -2 *height*)))
              1.0)
  (let* ((rad (* (coerce pi 'single-float) (/ *frame-count* 180)))
        (sinr (sin rad))
        (cosr (cos rad)))
    (gli:grotate 0.0 0.0 0.0)
    (gli:draw-string *glyph-table* "The quick brown fox jumps over the lazy dog."
      -350.0 0.0 0.0 30.0
      :color '(1 1 1 1))
    (gli:grotate 0.0 0.0 rad)
    (gli:draw-string *glyph-table* "Hello World!"
      -300.0 -150.0 0.0 (+ 45.0 (* 30.0 cosr))
      :color '(1 1 0 1))
    (gli:draw-string *glyph-table* "色は匂へと　散りぬるを"
      -300.0 200.0 0.0 40.0
      :color `(0 1 1 1)
      :spacing sinr))
  (gl:flush))

(defmethod glut:close ((w test-window))
  (gli:delete-glyph-table *glyph-table*)
  (gli:finalize)
  (format t "close~%"))

(plan 1)

(pass (glut:display-window (make-instance 'test-window)))

(finalize)
