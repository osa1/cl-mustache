(in-package :cl-mustache-test)

(defun walk-directory (directory pattern)
  (directory (merge-pathnames pattern directory)))

(defun utf8-json-decode (pathname)
  (with-open-file (stream pathname
                          :direction :input
                          :external-format :utf-8)
    (json:parse stream :object-as :alist
                       :json-arrays-as-vectors t)))

(defvar *spec-directory* #P"~/cl/cl-mustache/mustache.spec/")

(defvar *all-specs* 
  (mapcar #'utf8-json-decode (walk-directory *spec-directory* "specs/*.json")))

(fiveam:def-suite :mustache-specs)
(fiveam:in-suite :mustache-specs)

(defmacro mustache-spawn-test-suite (specs)
  `(progn
     ,@(loop 
         for spec in (eval specs)
         append (loop
                  for test across (cdr (assoc "tests" spec :test #'equalp))
                  for name = (cdr (assoc "name" test :test #'equalp))
                  for template = (cdr (assoc "template" test :test #'equalp))
                  for data = (cdr (assoc "data" test :test #'equalp))
                  for expected = (cdr (assoc "expected" test :test #'equalp))
                  for desc = (cdr (assoc "desc" test :test #'equalp))
                  for partial = (cdr (assoc "partials" test :test #'equalp))
                  collect `(fiveam:test ,(intern name) ,desc
                             (fiveam:is (string= ,expected (mustache-render ,template ',data ',partial)))))))) 

(defun pretty-result (test-result)
  (flet ((result-type (result) (format nil "~(~A~)" (symbol-name (type-of result)))))
    (let ((test-case (fiveam::test-case test-result)))
      (list (symbol-name (fiveam::name test-case))
            (fiveam::description test-case)
            (result-type test-result)))))

(defvar *results*)
(defun run-tests ()
  (mustache-spawn-test-suite *all-specs*)
  (setf *results* (fiveam:run :mustache-specs))

  (with-open-file (stream #P"~/public_html/results.html"
                          :direction :output :if-exists :supersede)
    (cl-who:with-html-output (stream)
      (:style :type "text/css" 
              ".test-passed { background-color: #0f0; }"
              ".test-failure { background-color: #f00; }"
              ".unexpected-test-failure { background-color: #ff0; }")
      (:table
       (loop
         for (name description result) in (mapcar #'pretty-result *results*)
         do (cl-who:htm 
             (:tr 
             (:td (cl-who:fmt name))
              (:td (cl-who:fmt description))
              (:td :class result (cl-who:fmt result))))))))
  'ook)

(defun explain ()
  (fiveam:explain! *results*))
