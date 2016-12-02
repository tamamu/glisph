(in-package :cl-user)
(defpackage glisph-test
  (:use :cl
        :prove))
(in-package :glisph-test)

;; NOTE: To run this test file, execute `(asdf:test-system :glisph)' in your Lisp.

(defparameter +width+ 800)
(defparameter +height+ 600)
(defvar *font*)
(defvar *glyph-table*)
(defvar *origin-x* 0)
(defvar *origin-y* 0)
(defvar *display-x* 0)
(defvar *display-y* 0)
(defvar *zoom* 1)

(defclass test-window (glut:window)
  ()
  (:default-initargs :title "GLisphTest"
                     :width +width+ :height +height+
                     :mode '(:stencil :multisample)))

(defmethod glut:mouse ((w test-window) button state x y)
  (declare (ignore w state))
  (case button
    (:left-button
      (setf *origin-x* (- (/ x *zoom*) *display-x*)
            *origin-y* (- (/ y *zoom*) *display-y*)))
    (:wheel-down
      (setf *zoom* (/ *zoom* 1.2))
      (glut:post-redisplay))
    (:wheel-up
      (setf *zoom* (* *zoom* 1.2))
      (glut:post-redisplay))))

(defmethod glut:motion ((w test-window) x y)
  (setf *display-x* (- (/ x *zoom*) *origin-x*)
        *display-y* (- (/ y *zoom*) *origin-y* ))
  (glut:post-redisplay))

(defmethod glut:reshape ((w test-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 width height 0 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod glut:display-window :before ((w test-window))
  (setf *font*
    (glisph:open-font-loader "/usr/share/fonts/OTF/TakaoGothic.ttf"))
  (setf *glyph-table* (glisph:make-glyph-table *font*))
  (loop for ch across "Hello World!色は匂へと　散りぬるを"
        do (glisph:regist-glyph *glyph-table* ch))
  (glisph:init))

(defmethod glut:display ((w test-window))
  (gl:clear-color 0 0 0 1)
  (gl:clear-stencil 0)
  (gl:clear :color-buffer-bit :stencil-buffer-bit)
  (gl:color 0.5 0.0 0.0 1.0)
  (gl:with-primitive :quads
    (gl:vertex 0 0)
    (gl:vertex 300 0)
    (gl:vertex 300 300)
    (gl:vertex 0 300))
  (gl:with-pushed-matrix
    (gl:translate (- *display-x*) (- *display-y*) 0)
    (gl:scale *zoom* *zoom* 0)
    (gl:translate (* *display-x* 2) (* *display-y* 2) 0)
    (glisph:draw-string *glyph-table* "Hello World!" :size 60
                                                     :color '(1 1 1 1))
    (gl:translate 0 60 0)
    (glisph:draw-string *glyph-table* "色は匂へと　散りぬるを" :size 40
                                                               :color '(1 0 1 1)
                                                               :spacing 0.5))
  (gl:flush))

(defmethod glut:close ((w test-window))
  (glisph:delete-glyph-table *glyph-table*)
  (glisph:finalize)
  (format t "close~%"))

(plan 1)

(pass (glut:display-window (make-instance 'test-window)))

(finalize)
