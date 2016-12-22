(in-package :cl-user)
(defpackage glisph-benchmark
  (:use :cl)
  (:export :window-main))
(in-package :glisph-benchmark)

(defparameter +text+ "A Quick Brown Fox Jumps Over The Lazy Dog 0123456789 A Quick Brown Fox Jumps Over The Lazy Dog 0123456789 ")
(defvar *line-count* 42)
(defvar *font* nil)
(defvar *glyph-table* nil)
(defvar *frame* 0)
(defvar *count* 0)
(defvar *last* 0)
(defvar *now* 0)
(defvar *delta* 0)

(defclass benchmark-window (glut:window)
  ()
  (:default-initargs :title "GLisph Benchmark"
                     :width 800 :height 600
                     :mode '(:stencil :multisample)
                     :tick-interval 1))

(defmethod glut:display-window :before ((w benchmark-window))
  (setf *font* (gli:open-font-loader "/usr/share/fonts/TTF/DroidSans.ttf"))
;  (setf *font* (gli:open-font-loader "/path/to/freetype-gl/fonts/VeraMono.ttf"))
  (setf *glyph-table* (gli:make-glyph-table *font*))
  (loop for ch across +text+
        do (gli:regist-glyph *glyph-table* ch))
  (gli:init)
  (gli:gscale (float (/ 2 800)) (float (/ -2 600)) 1.0)
  (gl:clear-color 1.0 1.0 1.0 1.0)
  (setf *last* (glut:get :elapsed-time)))


(defmethod glut:display ((w benchmark-window))
  (gl:clear :color-buffer-bit :stencil-buffer-bit)
  (when (= 0 *count* *frame*)
    (format t "Computing FPS with text rendering at each frame...~%")
    (format t "Number of glyphs: ~D~%" (* (length +text+) *line-count*)))

  (incf *frame*)
  (setf *now* (glut:get :elapsed-time))
  (setf *delta* (float (/ (- *now* *last*) 1000)))
  (when (> *delta* 2.5)
    (format t "FPS : ~,2F (~D frames in ~,2F second, ~,1F glyph/second)~%"
            (/ *frame* *delta*) *frame* *delta* 
            (* (/ *frame* *delta*) (length +text+) *line-count*))
    (setf *last* (glut:get :elapsed-time))
    (setf *frame* 0)
    (incf *count*)
    (when (> *count* 5)
      (glut:leave-main-loop)))
  (let ((x -390.0)
        (y -300.0))
    (gli:gsize 12.0)
    (gli:gcolor 0.0 0.0 0.0 1.0)
    (dotimes (i *line-count*)
      (gli:render-string *glyph-table* +text+ 0.01 x y 0.0 12.0)
      (incf y 14))
    (gl:flush)))

(defmethod glut:reshape ((w benchmark-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 width height 0 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod glut:tick ((w benchmark-window))
  (glut:post-redisplay))

(defun window-main ()
  (glut:display-window (make-instance 'benchmark-window)))