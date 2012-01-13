(ql:quickload '(fiveam cl-json cl-who cl-ppcre cl-interpol))
(cl-interpol:enable-interpol-syntax)

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

;; ---------------------------------------------------------------------

(defvar *spec-directory* #P"~/cl-mustache/mustache.spec/")

(defvar *all-specs* 
  (mapcar #'utf8-json-decode (walk-directory *spec-directory* "specs/*.json")))

(defun mustache-render (template data)
  (remove-double-mustaches
   (remove-triple-mustaches
    (remove-comments template)
    data)
   data))

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

(defun remove-sections (template data)
  (let* ((section-starts (find-tags "{{#" "}}"))
         (section-ends (find-tags "{{/" "}}")))))

(defun remove-triple-mustaches (template data)
  ;; TODO: dotted names
  (let* ((tags (find-tags "{{{" "}}}" template))
         (replacements
           (loop for tag in tags
                 collect (cdr (assoc
                               (string->keyword
                                (subseq tag 3 (- (length tag) 3)))
                               data)))))
    ;; TODO: a DOTO macro could be useful
    (loop for s in replacements
          for tag in tags do
            (if s
                (setf template (replace-all template tag (ensure-string s)))
                (setf template (replace-all template tag "")))))
  template)

(defun remove-double-mustaches (template data)
  ;; TODO: dotted names
  (let* ((tags (find-tags "{{" "}}" template))
         (replacements
           (loop for tag in tags
                 collect (cdr (assoc
                               (string->keyword
                                (subseq tag (if (char= #\& (char tag 2))
                                                3
                                                2) (- (length tag) 2)))
                               data)))))
    ;; TODO: a DOTO macro could be useful
    (loop for s in replacements
          for tag in tags do
            (if s
                (setf template (replace-all template tag
                                            (if (char= #\& (char tag 2))
                                                (ensure-string s)
                                                (cl-who:escape-string-all
                                                 (ensure-string s)))))
                (setf template (replace-all template tag "")))))
  template)

;; test suite ---------------------------------------------------

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
                  collect `(fiveam:test ,(intern name) ,desc
                             (fiveam:is (string= ,expected (mustache-render ,template ',data)))))))) 

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

