(in-package #:cl-mustache)

;; Utils ---------------------------------------------------------------

(defun ensure-string (obj)
  (if obj
      (if (stringp obj)
          obj
          (write-to-string obj))
      ""))

(defun search-alist (name alist)
  (let* ((dot-pos (search "." name))
         (key (if dot-pos
                  (subseq name 0 dot-pos)
                  name))
         (result (cdr (assoc key alist :test #'equalp))))
    (if dot-pos
        (search-alist (subseq name (1+ dot-pos)) result)
        result)))

(defun search-stack (name stack)
  ;; stack is a list of alists
  (let* ((dot-pos (search "." name))
         (key (if dot-pos
                  (subseq name 0 dot-pos)
                  name))
         (result
           (find key stack :test (lambda (key alist)
                                   (assoc key alist :test #'equalp)))))
    (when result
      (if dot-pos
          (search-alist name result)
          (cdr (assoc key result :test #'equalp))))))

(defun top-stack (stack)
  (first stack))

(defun utf8-json-decode (pathname)
  "Read JSON data from pathname in a format mustache-render needs."
  (with-open-file (stream pathname
                          :direction :input
                          :external-format :utf-8)
    (json:parse stream :object-as :alist
                       :json-arrays-as-vectors t)))

;; ---------------------------------------------------------------------

(defstruct delimiter
  (start "{{")
  (end "}}"))

(defvar *delimiter* (make-delimiter :start "{{" :end "}}"))

;; basically I'm putting this indicator when I don't want to render
;; contents of a section by `render` function. This should be a list
;; which contains a list to not break `search-stack`
(defvar *falsey-indicator* '(nil))

(defvar pt-whitespace '(:non-greedy-repetition 0 nil :whitespace-char-class))
(defvar pt-greedy-whitespace '(:greedy-repetition 0 nil :whitespace-char-class))
(defvar pt-everything '(:non-greedy-repetition 0 nil (:alternation :everything #\Newline)))

;; TODO: parser functions would be incredibly simpler if only I could
;; use fixed-length look-behind(or look-behind to a named group) in
;; cl-ppcre parse trees.
(defun make-tag-parser (&key (delimiter *delimiter*)
                          (func-char '(:alternation "=" ">" "{" "^" "!" "&" "#" "/" :void))
                          (content pt-everything)
                          (end-char '(:alternation "=" "}" :void)))
  `(:sequence
    (:register
     (:sequence ,pt-greedy-whitespace))
    (:sequence
     ,(delimiter-start delimiter)
     ,pt-greedy-whitespace
     (:register ,func-char)
     ,pt-greedy-whitespace
     (:register ,content)
     ,pt-whitespace
     ,end-char
     ,(delimiter-end delimiter)
     (:register
      (:sequence ,pt-greedy-whitespace)))))

(defstruct tag func-char content)

(defun tag-equal-p (tag1 tag2)
  (and (equal (tag-content tag1) (tag-content tag2))
       (eql (tag-func-char tag1) (tag-func-char tag2))))

(defun parse-delimiter (delimiter-tag-content)
  "Split delimiters in delimiter change tag and return new delimiter struct."
  (destructuring-bind (open-delimiter close-delimiter)
      (cl-ppcre:split '(:greedy-repetition 1 nil :whitespace-char-class)
                      delimiter-tag-content)
    (make-delimiter :start open-delimiter :end close-delimiter)))

(defun parse-line (line delimiter-stack &optional result)
  "Parse a line to tokens, return values of results(in reversed order) and updated delimiter stack.
line should be a string."
  ;; I decided to parse each line separately because it's easier to
  ;; handle standalone tags with this
  (let ((parser (make-tag-parser :delimiter (top-stack delimiter-stack))))
    (multiple-value-bind (start-idx end-idx parts-start parts-end)
        (cl-ppcre:scan parser line)
      (if (null start-idx)
          (values (remove-if (lambda (x) (and (stringp x) (zerop (length x))))
                             (cons line result))
                  delimiter-stack
                  nil)
          ;; TODO: match before-tag and rest in parse tree and use scan-to-strings
          (let ((before-tag (subseq line 0 start-idx))
                (before-tag-space (subseq line (elt parts-start 0) (elt parts-end 0)))
                (after-tag-space (subseq line (elt parts-start 3) (elt parts-end 3)))
                (func-char (if (= (elt parts-start 1) (elt parts-end 1))
                               nil
                               (char line (elt parts-start 1))))
                (content (subseq line (elt parts-start 2) (elt parts-end 2)))
                (rest (subseq line end-idx)))
            (case func-char
              ((#\# #\^)
               (push (top-stack delimiter-stack) delimiter-stack))
              ((#\=)
               (pop delimiter-stack)
               (push (parse-delimiter content) delimiter-stack))
              ((#\/)
               (pop delimiter-stack)))
            (let ((tag (make-tag :func-char func-char
                                 :content content)))
              (if (and (null result)
                       (zerop start-idx)
                       (= (length line) end-idx)
                       (not (member func-char '(#\{ #\& nil))))
                  ;; tag is standalone
                  (values (cons tag result) delimiter-stack t)
                  (parse-line
                   rest
                   delimiter-stack
                   (append (list after-tag-space tag before-tag-space before-tag) result)))))))))

(defvar *newline* (string #\Newline))

;; TODO: consing and then reversing is ugly, find a better idiom to
;; append a list/array/vector/any-kind-of-ordered-collection
(defun parse-template (stream &optional (result '()) (delimiter-stack (list *delimiter*)))
  "Parse template to tokens. A token is a string or tag struct.
This function also removes delimiter change tags from template, so there's no need to
handle delimiter change tags in render function."
  (multiple-value-bind (line end-of-stream-p)
      (read-line stream nil)
    (multiple-value-bind (line-tokens new-delimiter-stack standalone-p)
        (parse-line (or line "") delimiter-stack)
      (let ((r (append (cons (if (or standalone-p end-of-stream-p) "" *newline*)
                             line-tokens)
                       result)))
        (if end-of-stream-p
            (reverse r)
            (parse-template stream r new-delimiter-stack))))))

(defun collect-tokens (tokens target)
  "Collect tokens in token list up to target tag."
  (loop for token in tokens
        while (or (not (tag-p token))
                  (not (tag-equal-p token target)))
        collect token))

(defun render-implicit-iterator (tokens list out)
  "Renders tokens for each element in list and princs to `out`. Returns `out`."
  (loop for data across list do
    (loop for token in tokens do
      (typecase token
        (string
         (princ token out))
        (tag
         (princ
          (cl-who:escape-string-all
           (ensure-string data))
          out)))))
  out)

(defun render (stream-or-tokens stack partials &optional (out (make-string-output-stream)))
  "Renders a template or list of tokens and princs it to `out` output stream. Returns out.
A token is a string or mustache tag(struct tag). Stack is a list of alists."
  ;; Parse template to tokens when needed
  (let ((tokens (if (streamp stream-or-tokens)
                    (parse-template stream-or-tokens)
                    stream-or-tokens)))
    (loop for token-list on tokens do
      (let ((token (first token-list)))
        (cond ((eql (top-stack stack) *falsey-indicator*)
               (when (tag-p token)
                 (cond ((eql (tag-func-char token) #\/) (pop stack))
                       ((member (tag-func-char token) '(#\^ #\#))
                        ;; nested falsey sections
                        (push *falsey-indicator* stack)))))
              ((stringp token)
               (princ token out))
              (t ;; token is a tag
               (let* ((func-char (tag-func-char token))
                      (tag-content (tag-content token))
                      (tag-data (search-stack tag-content stack)))
                 (case func-char
                   
                   ((#\# #\^)
                    (cond ((or (and (null tag-data) (eql func-char #\#))
                               (and (not (null tag-data)) (eql func-char #\^)))
                           (push *falsey-indicator* stack))
                          ((vectorp tag-data)
                           (unless (zerop (length tag-data))
                             (let ((section-tokens
                                     (collect-tokens (cdr token-list)
                                                     (make-tag :func-char #\/
                                                               :content tag-content))))
                               (if (listp (elt tag-data 0))
                                   (loop for datum across tag-data do
                                     (render section-tokens (list datum) partials out))
                                   (render-implicit-iterator section-tokens tag-data out))))
                           (push *falsey-indicator* stack))
                          ((eq t tag-data)
                           (push (top-stack stack) stack))
                          ((string= tag-content "lambda")
                           ;; TODO: lambda support
                           )
                          (t (push tag-data stack))))

                   ((#\/)
                    (pop stack))
                   
                   ((#\{ #\&)
                    (princ (ensure-string tag-data) out))

                   ((#\>)
                    (render
                     (make-string-input-stream
                      (or (cdr (assoc tag-content partials :test #'equalp))
                          "")) stack partials out))

                   ((#\!))
                   
                   (t
                    (princ (cl-who:escape-string-all (ensure-string tag-data)) out)))))))))
  out)

(defun mustache-render (template data-or-json-path &optional partials)
  (get-output-stream-string
   (render (if (streamp template)
               template
               (make-string-input-stream template))
           (list (if (pathnamep data-or-json-path)
                     (utf8-json-decode data-or-json-path)
                     data-or-json-path))
           partials)))
