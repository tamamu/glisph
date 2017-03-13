#|
  This file is a part of glisph project.
  Copyright (c) 2016 Tamamu
|#

#|
  Author: Tamamu
|#

(in-package :cl-user)
(defpackage glisph-asd
  (:use :cl :asdf))
(in-package :glisph-asd)

(defsystem glisph
  :version "0.1"
  :author "Tamamu"
  :license "MIT"
  :depends-on (:cl-annot
               :cl-opengl
               :cl-glu
               :cl-reexport
               :zpb-ttf)
  :components ((:module "src"
                :components
                ((:file "glisph" :depends-on ("shader"))
                 (:file "shader"))))
  :description "Glyph rendering engine using OpenGL shading language"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op glisph-test))))
