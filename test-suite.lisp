(ql:quickload '(fiveam cl-json cl-who cl-ppcre))
;; cl-interpol

;; Utils ---------------------------------------------------------------

(defun walk-directory (directory pattern)
  (directory (merge-pathnames pattern directory)))

(defun utf8-json-decode (pathname)
  (with-open-file (stream pathname
                          :direction :input
                          :external-format :utf-8)
    (json:decode-json-from-source stream)))

;; from common-lisp cookbook
(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 

(defun search-all (sequence1 sequence2 &key (test #'eql) (start2 0))
  "Return indexes of all matches(cl:search returns index of first match)."
  (let ((idx (search sequence1 sequence2 :test test :start2 start2)))
    (when idx
      (cons idx
            (search-all sequence1 sequence2 :test test
                                            :start2 (1+ (min idx (1- (length sequence2)))))))))

(defun remove-whitespace (str)
  "Remove trailing and following whitespace."
  (string-trim '(#\ ) str))

(defun string->keyword (str)
  (intern (string-upcase (remove-whitespace str)) "KEYWORD"))

(defun ensure-string (obj)
  (if (stringp obj)
      obj
      (write-to-string obj)))

(defmacro concat (&rest strs)
  `(concatenate 'string ,@strs))

;; ---------------------------------------------------------------------

(defvar *delimiters* '("{{" . "}}"))

(defun remove-comments (template)
  (cl-ppcre:regex-replace-all "\\s*?\\{\\{\\![^\\}]*\\}\\}" template "")
  ;(cl-ppcre:regex-replace-all #?"\n?\s*\{\{\![^\}]*\}\}\s*?\n?" template "")
  )

(defun find-tags (start end template)
  (let ((ranges (merge 'list
                       (search-all start template)
                       (search-all end template)
                       #'<)))
    (when ranges
      (loop for (start-index end-index) on ranges by #'cddr
            when end-index
            collect (subseq template start-index (+ (length end) end-index))))))

(defvar implicit-iter-pattern "{{.}}")

(defun name->data (name alist)
  (let ((pos (position #\. name)))
    (if pos
        (name->data (subseq name (1+ pos))
                    (cdr (assoc (string->keyword (subseq name 0 pos)) alist)))
        (cdr (assoc (string->keyword name) alist)))))

(defun triple->double (template)
  "Convert triple mustaches to double mustaches."
  (destructuring-bind (open-delimiter . close-delimiter) *delimiters*
    (setf template
          (replace-all template (concat open-delimiter "{") (concat open-delimiter "&")))
    (setf template
          (replace-all template (concat "}" close-delimiter) close-delimiter)))
  template)

(defun remove-double-mustaches (template data)
  (destructuring-bind (open-delimiter . close-delimiter) *delimiters*
    (let* ((tags (find-tags open-delimiter close-delimiter template))
           (replacements
             (loop for tag in tags
                   collect (name->data (subseq tag (if (char= #\& (char tag 2))
                                                       3
                                                       2) (- (length tag) 2))
                                       data))))
      (loop for s in replacements
            for tag in tags do
              (setf template (replace-all template tag
                                          (if s
                                              (if (char= #\& (char tag 2))
                                                  (ensure-string s)
                                                  (cl-who:escape-string-all
                                                   (ensure-string s)))
                                              ""))))))
  template)

(defvar section-pattern
  "(\\{\\{#\\s*?(\\S*?)\\s*?\\}\\}([\\s\\S]*?)\\{\\{/\\s*?(\\S*?)\\s*\\}\\})")
(defun remove-sections (template data)
  (cl-ppcre:do-register-groups (section section-name section-content section-end)
      (section-pattern template)
    (let ((section-data (cdr (assoc (string->keyword section-name) data)))
          (implicit-iter-pos (search implicit-iter-pattern section-content)))
      (cond (implicit-iter-pos
             (setf template
                   (replace-all template section-content
                                (apply #'concatenate 'string
                                       (loop for s in section-data
                                             collect (replace-all
                                                      section-content
                                                      implicit-iter-pattern
                                                      (ensure-string s)))))))
            (t
             (setf template
                   (replace-all template
                                section
                                (if section-data
                                    (remove-double-mustaches
                                     section-content
                                     section-data)
                                    "")))))))
  template)

(defun mustache-render (template data &optional partial)
  (declare (ignore partial))
  (remove-double-mustaches
   (remove-sections
    (triple->double
     (remove-comments template))
    data)
   data))

;; test suite ---------------------------------------------------

(defvar *spec-directory* #P"~/cl-mustache/mustache.spec/")

(defvar *all-specs* 
  (mapcar #'utf8-json-decode (walk-directory *spec-directory* "specs/*.json")))

(fiveam:def-suite :mustache-specs)
(fiveam:in-suite :mustache-specs)

(defmacro mustache-spawn-test-suite (specs)
  `(progn
     ,@(loop 
         for spec in (eval specs)
         append (loop
                  for test in (cdr (assoc :tests spec))
                  for name = (cdr (assoc :name test))
                  for template = (cdr (assoc :template test))
                  for data = (cdr (assoc :data test))
                  for expected = (cdr (assoc :expected test))
                  for desc = (cdr (assoc :desc test))
                  for partial = (cdr (assoc :partials test))
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
  'ok)

