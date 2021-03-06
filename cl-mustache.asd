
(in-package :cl-user)

(asdf:defsystem #:cl-mustache
  :serial t
  :depends-on (#:cl-who
               #:yason
               #:cl-ppcre
               #:fiveam)
  :components ((:file "package")
               (:file "cl-mustache")))

(asdf:defsystem #:cl-mustache-test
  :components ((:file "tests"))
  :depends-on (:cl-mustache :fiveam :yason))

