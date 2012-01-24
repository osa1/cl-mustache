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

;; (defun make-tag-parser (&key (delimiter *delimiter*)
;;                           (func-char '(:alternation "=" ">" "{" "^" "!" "&" "#" "/" :void))
;;                           (content pt-everything)
;;                           (end-char '(:alternation "=" "}" :void)))
;;   (let ((tag-body
;;           `(,(delimiter-start delimiter)
;;              ,pt-greedy-whitespace
;;              (:register ,func-char)
;;              ,pt-greedy-whitespace
;;              (:register ,content)
;;              ,pt-whitespace
;;              ,end-char
;;              ,(delimiter-end delimiter))))
;;     `(:alternation
;;       (:sequence ;; standalone tag
;;        (:positive-lookbehind #\Newline)
;;        (:greedy-repetition 0 nil #\ )
;;        ;:start-anchor (:greedy-repetition 0 nil #\ )
;;        ,@tag-body
;;        (:greedy-repetition 0 nil #\ ) #\Newline)
;;       (:sequence
;;        ,@tag-body))))


(defun make-tag-parser (&key (delimiter *delimiter*)
                          (func-char '(:alternation "=" ">" "{" "^" "!" "&" "#" "/" :void))
                          (content pt-everything)
                          (end-char '(:alternation "=" "}" :void)))
  `(:sequence
    (:register
     (:alternation
      (:sequence #\Newline ,pt-greedy-whitespace) ""))
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
      (:alternation
       (:sequence ,pt-greedy-whitespace #\Newline) "")))))

(defun handle-standalone-tag (tag-parts)
  (let ((before-tag (elt tag-parts 0)) ;; destructuring on arrays?
        (func-char (if (zerop (length (elt tag-parts 1)))
                       nil
                       (char (elt tag-parts 1) 0)))
        (tag-content (elt tag-parts 2))
        (after-tag (elt tag-parts 3)))
    (let ((tag (make-tag :func-char func-char
                         :content tag-content)))
      (if (and (position #\Newline before-tag)
               (position #\Newline after-tag)
               )
          ;; tag is standalone, we need to add a newline since we
          ;; matched 2 newlines
          ;(list (string #\Newline) tag)
          (list tag (string #\Newline))
          ;; tag is not standalone, we need to add spaces to rendered string
          ;(list before-tag tag after-tag)
          (list after-tag tag before-tag)))))

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

(defun parse-template (template &optional (result '()) (delimiter-stack (list *delimiter*)))
  "Parse template to tokens. A token is a string or tag struct.
This function also removes delimiter change tags from template, so there's no need to
handle delimiter change tags in render function."
  (let ((parser (make-tag-parser :delimiter (top-stack delimiter-stack))))
    (multiple-value-bind (start-idx end-idx parts-start parts-end)
        (cl-ppcre:scan parser template)
      (cond ((null start-idx) ;; rest of the template doesn't contain any tags
             (reverse (cons template result)))
            ((zerop start-idx) ;; next tag is at the beginning of template
             (let ((func-char (if (= (elt parts-start 1) (elt parts-end 1))
                                  nil
                                  (char template (elt parts-start 1))))
                   (content (subseq template (elt parts-start 2) (elt parts-end 2))))
               (case func-char
                 ((#\# #\^)
                  (push (top-stack delimiter-stack) delimiter-stack))
                 ((#\=)
                  (pop delimiter-stack)
                  (push (parse-delimiter content) delimiter-stack))
                 ((#\/)
                  (pop delimiter-stack)))
               ;; TODO: why this call is not tail-call optimized?
               (parse-template (subseq template end-idx)
                               (append
                                (handle-standalone-tag
                                 (nth-value 1 (cl-ppcre:scan-to-strings parser template)))
                                result)
                               delimiter-stack)))
            (t ;; have a string before tag, should render it first
             (parse-template (subseq template start-idx)
                             (cons (subseq template 0 start-idx) result)
                             delimiter-stack))))))


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

(defun render (template-or-tokens stack partials &optional (out (make-string-output-stream)))
  "Renders a template or list of tokens and princs it to `out` output stream. Returns out.
A token is a string or mustache tag(struct tag). Stack is a list of alists."
  ;; Parse template to tokens when needed
  (let ((tokens (if (stringp template-or-tokens)
                    (parse-template template-or-tokens)
                    template-or-tokens)))
    (loop for token-list on tokens do
      (let ((token (first token-list)))
        (cond ((eql (top-stack stack) *falsey-indicator*)
               (when (and (tag-p token)
                          (eql (tag-func-char token) #\/))
                 (pop stack)))
              ((stringp token)
               (princ token out))
              (t ;; token is a tag
               (let* ((func-char (tag-func-char token))
                      (tag-content (tag-content token))
                      (tag-data (search-stack tag-content stack)))
                 (case func-char
                   
                   ((#\#)
                    (cond ((null tag-data)
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

                   ((#\^))
                   
                   ((#\/)
                    (pop stack))
                   
                   ((#\{ #\&)
                    (princ (ensure-string tag-data) out))

                   ((#\>)
                    (render
                     (cdr (assoc tag-content partials :test #'equalp)) stack partials out))

                   ((#\!))
                   
                   (t
                    (princ (cl-who:escape-string-all (ensure-string tag-data)) out)))))))))
  out)

(defun mustache-render (template data &optional partials)
  (get-output-stream-string
   (render template (list data) partials)))
