#|
  This file is a part of glisph project.
  Copyright (c) 2016 Tamamu
|#

(in-package :cl-user)
(defpackage glisph-test-asd
  (:use :cl :asdf))
(in-package :glisph-test-asd)

(defsystem glisph-test
  :author "Tamamu"
  :license "MIT"
  :depends-on (:glisph
               :prove
               :cl-glut)
  :components ((:module "t"
                :components
                ((:test-file "glisph"))))
  :description "Test system for glisph"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
