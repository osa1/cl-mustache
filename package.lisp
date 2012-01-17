
(in-package :cl-user)

(defpackage #:cl-mustache
  (:use :cl)
  (:export #:mustache-render))

(defpackage #:cl-mustache-test
  (:use :cl :cl-mustache)
  (:export #:run-tests #:*results* #:explain))


