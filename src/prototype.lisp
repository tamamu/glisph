(require :asdf)
(require :zpb-ttf)
(require :cl-glut)
(require :cl-glu)
(require :cl-opengl)
(require :xsubseq)

(in-package :cl-user)
(defpackage proto
  (:use cl xsubseq))
(in-package :proto)

(defvar *font*)

(defun draw-one-line (x1 y1 x2 y2)
  (gl:with-primitives :lines
    (gl:vertex x1 y1)
    (gl:vertex x2 y2)))

(defun normalize (bounding-box)
  (let* ((xbase (- (zpb-ttf:xmin bounding-box)))
         (ybase (- (zpb-ttf:ymin bounding-box)))
         (w (+ xbase (zpb-ttf:xmax bounding-box))))
    (values
      (lambda (x)
        (coerce (/ (+ x xbase) w) 'single-float))
      (lambda (y)
        (coerce (/ (+ y ybase) w) 'single-float)))))

(defun convex-hull (points)
  (setf points
    (sort points
      (lambda (a b)
        (if (/= 0 (- (zpb-ttf:x a) (zpb-ttf:x b)))
          t
          (/= 0 (- (zpb-ttf:y a) (zpb-ttf:y b)))))))
  (labels ((half-convex (sign)
             (let ((tmp (xsubseq points 0 2)))
               (loop for i from 2 below (length points)
                 do (let ((p2 (aref points i)))
                      (loop while (>= (xlength tmp) 2)
                            for p0 = (aref points (- (xlength tmp) 2))
                            for p1 = (aref points (- (xlength tmp) 1))
                            for s  = (- (* (- (zpb-ttf:x p0)
                                              (zpb-ttf:x p1))
                                           (- (zpb-ttf:y p2)
                                              (zpb-ttf:y p1)))
                                        (* (- (zpb-ttf:y p0)
                                              (zpb-ttf:y p1))
                                           (- (zpb-ttf:x p2)
                                              (zpb-ttf:x p1))))
                        when (> (* s sign) 0) return 0
                        do (setf tmp (xsubseq tmp 0 (1- (xlength tmp)))))
                      (xnconc tmp p2)))
               (coerce-to-sequence tmp))))
    (concatenate 'vector (half-convex 1) (reverse (half-convex -1)))))

(defstruct point2d
  (x 0.0 :type single-float)
  (y 0.0 :type single-float))

(defmacro vector-push-extend-to (vec &rest rest)
  `(loop for e in (list ,@rest)
         do (vector-push-extend e ,vec)))

(defmacro center-x (a b)
  `(/ (+ (point2d-x ,a) (point2d-y ,b)) 2))

(defmacro center-y (a b)
  `(/ (+ (point2d-y ,a) (point2d-y ,b)) 2))

(defun add-path (data1 tmp1 data2 &optional (cubic 0))
  (case (length tmp1)
    ((0 1) nil)
    (2 (setf tmp1 (subseq tmp1 1)))
    (3 (let ((t0 (elt tmp1 0))
             (t1 (elt tmp1 1))
             (t2 (elt tmp1 2)))
         (vector-push-extend-to data1
           (point2d-x t0) (point2d-y t0) 0 0
           (point2d-x t1) (point2d-y t1) 0.5 0
           (point2d-x t2) (point2d-y t2) 1 1)
         (setf tmp1 (subseq tmp1 2))))
    (4 (if (zerop cubic)
         (let ((t0 (elt tmp1 0))
               (t1 (elt tmp1 1))
               (t2 (elt tmp1 2))
               (t3 (elt tmp1 3)))
           (vector-push-extend-to data1
             (point2d-x t0) (point2d-y t0) 0 0
             (point2d-x t1) (point2d-y t1) 0.5 0
             (center-x t1 t2) (center-y t1 t2) 1 1
             
             (center-x t1 t2) (center-y t1 t2) 0 0
             (point2d-x t2) (point2d-y t2) 0.5 0
             (point2d-x t3) (point2d-y t3) 1 1)
           (vector-push-extend (make-point2d :x (center-x t1 t2)
                                             :y (center-y t1 t2))
                               data2)
           (setf tmp1 (subseq tmp1 3)))
         (print "Error")))
    (6 (let ((t0 (elt tmp1 0))
             (t1 (elt tmp1 1))
             (t2 (elt tmp1 2))
             (t3 (elt tmp1 3))
             (t4 (elt tmp1 4))
             (t5 (elt tmp1 5)))
         (vector-push-extend-to data1
           (point2d-x t0) (point2d-y t0) 0 0
           (point2d-x t1) (point2d-y t1) 0.5 0
           (center-x t1 t2) (center-y t1 t2) 1 1
           
           (center-x t1 t2) (center-y t1 t2) 0 0
           (point2d-x t2) (point2d-y t2) 0.5 0
           (center-x t2 t3) (center-y t2 t3) 1 1
           
           (center-x t2 t3) (center-y t2 t3) 0 0
           (point2d-x t3) (point2d-y t3) 0.5 0
           (center-x t3 t4) (center-y t3 t4) 1 1

           (center-x t3 t4) (center-y t3 t4) 0 0
           (point2d-x t4) (point2d-y t4) 0.5 0
           (point2d-x t5) (point2d-y t5) 1 1)
         (vector-push-extend-to data2
           (make-point2d :x (center-x t1 t2)
                         :y (center-y t1 t2))
           (make-point2d :x (center-x t2 t3)
                         :y (center-y t2 t3))
           (make-point2d :x (center-x t3 t4)
                         :y (center-y t3 t4)))
         (subseq tmp1 5))))
  (values data1 tmp1 data2))

(defun make-data (ch font)
  (let ((glyph (zpb-ttf:find-glyph ch font)))
    (zpb-ttf:do-contours (contour glyph)
      
    )
  )
)

(defun draw-character (ch font)
  (let* ((bbox (zpb-ttf:bounding-box font))
         (glyph (zpb-ttf:find-glyph ch font))
         (gbox (zpb-ttf:bounding-box glyph))
         (lsb (zpb-ttf:left-side-bearing glyph))
         (dx (zpb-ttf:left-side-bearing glyph)))
    (multiple-value-bind (nmx nmy)
      (normalize bbox)
      (zpb-ttf:do-contours (contour glyph)
        (zpb-ttf:do-contour-segments (p0 p1 p2) contour
          (let ((x1 (funcall nmx (zpb-ttf:x p0)))
                (y1 (funcall nmy (zpb-ttf:y p0)))
                (x2 (funcall nmx (zpb-ttf:x p2)))
                (y2 (funcall nmy (zpb-ttf:y p2))))
            (draw-one-line x1 y1 x2 y2))))
        (- (funcall nmx (zpb-ttf:xmax gbox))
           (funcall nmx (zpb-ttf:xmin gbox))))))

(defun draw-text (x y text font size spacing)
  (gl:translate 0 0 0)
  (gl:scale 1 1 1)
  (gl:translate x y 0)
  (gl:scale size size 1)
  (dotimes (i (length text))
    (let ((w (draw-character (char text i) font)))
      (gl:translate (+ w spacing) 0 0))))

(defclass main-window (glut:window)
  ()
  (:default-initargs :width 1024 :height 120 :title "bezier.lisp"
                     :mode '(:single :rgb)))

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

(defmethod glut:display ((w main-window))
  (gl:clear-color 0.5 0.5 0.5 1.0)
  (gl:clear :color-buffer-bit)
  (draw-text 0 0 "新しい朝が来た　希望の朝だ" *font* 64 0.15)
  (draw-text 0 120 "A Quick Brown Fox Jumps Over The Lazy Dog" *font* 16 0.15)
  (gl:flush))

(defmethod glut:close ((w main-window))
  (zpb-ttf:close-font-loader *font*))

;(sb-profile:profile "BEZIER")

;(glut:display-window (make-instance 'main-window))
