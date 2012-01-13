(ql:quickload '(fiveam cl-json cl-who))

(defun walk-directory (directory pattern)
  (directory (merge-pathnames pattern directory)))

(setq *spec-directory* #P"~/cl-mustache/mustache.spec/")

(defun utf8-json-decode (pathname)
  (with-open-file (stream pathname
                          :direction :input
                          :external-format :utf-8)
    (json:decode-json-from-source stream)))

(setq *all-specs* 
      (mapcar #'utf8-json-decode (walk-directory *spec-directory* "specs/*.json")))


