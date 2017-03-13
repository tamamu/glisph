(in-package :cl-user)
(defpackage glisph-test
  (:use :cl
        :prove))
(in-package :glisph-test)

;; NOTE: To run this test file, execute `(asdf:test-system :glisph)' in your Lisp.

(defvar *width* 800)
(defvar *height* 600)
(defvar *font-en*)
(defvar *font-ja*)
(defvar *glyph-table-en*)
(defvar *glyph-table-ja*)
(defvar *origin-x* 0.0)
(defvar *origin-y* 0.0)
(defvar *display-x* 0.0)
(defvar *display-y* 0.0)
(defvar *zoom* 1.0)
(defvar *frame-count* 0)
(defvar *text-en* #("Hello World!"
                    "The Quick Brown Fox Jumps Over The Lazy Dog."
                    "0123456789"))
(defvar *text-ja* #("色はにほへど　散りぬるを"
                    "我が世たれぞ　常ならむ"
                    "有為の奥山　　今日越えて"
                    "浅き夢見じ　　酔ひもせず"))

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
      (setf *zoom* (* *zoom* 1.2))
      (glut:post-redisplay))
    (:wheel-up
      (setf *zoom* (/ *zoom* 1.2))
      (glut:post-redisplay))))

(defmethod glut:motion ((w test-window) x y)
  (setf *display-x* (- x *origin-x*)
        *display-y* (- y *origin-y*))
  (glut:post-redisplay))

(defmethod glut:reshape ((w test-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 width height 0 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (setf *width* width
        *height* height))

(defmethod glut:display-window :before ((w test-window))
  (gli:init *width* *height*)
  (pass "Success: init")
  (setf *font-en* (gli:open-font-loader
                   (merge-pathnames "Ubuntu-R.ttf" *load-truename*))
        *font-ja* (gli:open-font-loader
                   (merge-pathnames "mplus-1m-regular.ttf" *load-truename*)))
  (setf *glyph-table-en* (gli:make-glyph-table *font-en*)
        *glyph-table-ja* (gli:make-glyph-table *font-ja*))
  (pass "Success: make-glyph-table")
  (loop for text across *text-en*
        do (gli:regist-glyphs *glyph-table-en* text))
  (loop for text across *text-ja*
        do (gli:regist-glyphs *glyph-table-ja* text))
  (pass "Success: regist-glyphs")
  (setf *text-en*
        (gli:draw *glyph-table-en*
          '(:size 20 :x 0 :y 0
           :text (aref *text-en* 0)
           :size 30 :x 0 :y 24
           :text (aref *text-en* 1)
           :size 40 :x 0 :y 60
           :text (aref *text-en* 2))))
  (setf *text-ja*
        (gli:draw *glyph-table-ja*
          `(:size 24 :x 200
            ,@(loop for idx from 0 below (length *text-ja*)
                  append (list :spacing (* idx 4)
                               :y (+ 200 (* idx 60))
                               :text (aref *text-ja* idx))))))
  (pass "Success: draw")
  (ok t))

(defmethod glut:tick ((w test-window))
  (incf *frame-count*)
  (when (>= *frame-count* 360)
    (setf *frame-count* 0))
  (glut:post-redisplay))

(defmethod glut:display ((w test-window))
  (gl:viewport 0 0 *width* *height*)
  (gl:clear-color 0 0 0 1)
  (gl:clear-stencil 0)
  (gl:clear :color-buffer-bit :stencil-buffer-bit)
  (gl:color 0.5 0.0 0.0 1.0)
  (gl:with-primitive :quads
    (gl:vertex 0 0)
    (gl:vertex 300 0)
    (gl:vertex 300 300)
    (gl:vertex 0 300))
  (let* ((rad (* (coerce pi 'single-float) (/ *frame-count* 180)))
         (cosr (cos rad)))

    ;; Currently, because of #2, it has no effect in this context.
    (gli:gcolor 1.0 1.0 1.0 1.0)

    (gli:render *text-en*)

    ;; #2 It overwrites the color drawn before.
    (gli:gcolor cosr 1.0 1.0 1.0)

    (gli:render *text-ja*)
    (gl:flush)))

(defmethod glut:close ((w test-window))
  (gli:delete-glyph-table *glyph-table-en*)
  (gli:delete-glyph-table *glyph-table-ja*)
  (gli:finalize)
  (format t "close~%"))

(plan 5)

(glut:display-window (make-instance 'test-window))

(finalize)
