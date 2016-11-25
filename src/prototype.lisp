(require :asdf)
(require :zpb-ttf)
(require :cl-glut)
(require :cl-glu)
(require :cl-opengl)

(in-package :cl-user)
(defpackage proto
  (:use cl))
(in-package :proto)

(defvar *font*)

(defun separate-contour (contour)
  (let ((curve (make-array 6 :element-type 'fixnum :fill-pointer 0 :adjustable t))
        (control (make-array 0 :element-type 'fixnum :fill-pointer 0 :adjustable t)))
    (loop for i from 0 below (1- (length contour))
          for p = (aref contour i)
          for x = (zpb-ttf:x p)
          for y = (zpb-ttf:y p)
          if (zpb-ttf:on-curve-p p)
            do (vector-push-extend x curve)
               (vector-push-extend y curve)
          else
            do (let ((prev (aref contour (1- i)))
                     (next (aref contour (1+ i))))
                 (vector-push-extend-to control
                   (zpb-ttf:x prev) (zpb-ttf:y prev)
                   x y
                   (zpb-ttf:x next) (zpb-ttf:y next))))
    (values curve control)))

(defun fill-inner (glyph)
  (let ((bbox (zpb-ttf:bounding-box glyph)))
    (zpb-ttf:do-contours (contour glyph)
      (multiple-value-bind (curve control) (separate-contour contour)
        (let ((ix (aref curve 0))
              (iy (aref curve 1)))
          (loop for i from 2 below (- (length curve) 2) by 2
                do (gl:with-primitives :triangles
                     (gl:vertex ix iy)
                     (gl:vertex (aref curve    i   ) (aref curve (+ i 1)))
                     (gl:vertex (aref curve (+ i 2)) (aref curve (+ i 3))))))))))

(defun render-string (str font spacing)
  (gl:with-pushed-matrix
    (loop for i from 0 below (length str)
          for cur = (zpb-ttf:find-glyph (char str i) font)
          for prev = nil then cur
          do (when prev
               (gl:translate (- (zpb-ttf:kerning-offset prev cur font)
                                (zpb-ttf:left-side-bearing cur))
                             0 0))
               (render-glyph cur)
               (gl:translate (+ spacing (zpb-ttf:advance-width cur)) 0 0))))

;; !! :stencil is required !!
(defclass main-window (glut:window)
  ()
  (:default-initargs :width 1024 :height 120 :title "bezier.lisp"
                     :mode '(:single :rgb :stencil)))

(defmethod glut:reshape ((w main-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d 0 width 0 height))

(defmethod glut:display :before ((w main-window))
  (setf *font*
    (zpb-ttf:open-font-loader "/usr/share/fonts/OTF/TakaoPGothic.ttf"))
  (gl:clear-color 0 0 0 0)
  (gl:shade-model :flat))

(defun render-glyph (glyph)
  (gl:enable :stencil-test)
  (gl:stencil-func :always 0 1)
  (gl:stencil-op :keep :invert :invert)
  (gl:color-mask nil nil nil nil)
  (fill-inner glyph)
  (gl:stencil-func :notequal 0 1)
  (gl:stencil-op :keep :keep :keep)
  (gl:color-mask t t t t)
  (let* ((bbox (zpb-ttf:bounding-box glyph))
         (bx1 (aref bbox 0))
         (by1 (aref bbox 1))
         (bx2 (aref bbox 2))
         (by2 (aref bbox 3)))
    (gl:with-primitives :quads
      (gl:vertex bx1 by1)
      (gl:vertex bx2 by1)
      (gl:vertex bx2 by2)
      (gl:vertex bx1 by2))))

(defun draw-string (str font size spacing)
  (gl:with-pushed-matrix
    (let* ((bbox (zpb-ttf:bounding-box font))
           ;(bx1 (aref bbox 0))
           ;(by1 (aref bbox 1))
           ;(bx2 (aref bbox 2))
           ;(by2 (aref bbox 3))
           (scaling (/ size (zpb-ttf:units/em font))))
      (gl:scale scaling scaling 1)
      (render-string str font (* spacing scaling)))))

(defmethod glut:display ((w main-window))
  ;(gl:enable :stencil-test)
  (gl:clear-color 0.5 0.5 0.5 1.0)
  (gl:with-pushed-matrix
    (gl:translate 32 20 0)
    (draw-string "新しい朝が来た　希望の朝だ" *font* 32 0))
  (gl:with-pushed-matrix
    (gl:translate 32 64 0)
    (draw-string "A Quick Brown Fox Jumps Over The Lazy Dog." *font* 32 0))

  (gl:flush))

(defmethod glut:close ((w main-window))
  (zpb-ttf:close-font-loader *font*))

(glut:display-window (make-instance 'main-window))
