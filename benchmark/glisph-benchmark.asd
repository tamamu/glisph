#|
  This file is a part of glisph-benchmark project.
  Copyright (c) 2016 tamamu
|#

#|
  Author: tamamu
|#

(in-package :cl-user)
(defpackage glisph-benchmark-asd
  (:use :cl :asdf))
(in-package :glisph-benchmark-asd)

(defsystem glisph-benchmark
  :version "0.1"
  :author "tamamu"
  :license "MIT"
  :depends-on (:glisph
               :cl-glut)
  :components ((:module "src"
                :components
                ((:file "glisph-benchmark"))))
  :description ""
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
  :in-order-to ((test-op (test-op glisph-benchmark-test))))
