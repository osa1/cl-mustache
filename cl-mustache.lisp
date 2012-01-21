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
  ;; stack is a list of plists
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

(defvar *falsey-indicator* 'nilnilnil)

(defvar pt-whitespace '(:non-greedy-repetition 0 nil :whitespace-char-class))
(defvar pt-greedy-whitespace '(:greedy-repetition 0 nil :whitespace-char-class))
(defvar pt-non-whitespace '(:non-greedy-repetition 0 nil :non-whitespace-char-class)) 
(defvar pt-everything '(:non-greedy-repetition 0 nil :everything))

(defun make-tag-parser (&key (delimiter *delimiter*)
                          (func-char '(:alternation "=" ">" "{" "^" "!" "&" "#" "/" :void))
                          (content '(:non-greedy-repetition 0 nil :everything))
                          (end-char '(:alternation "=" "}" :void)))
  `(:sequence
    (:alternation
     (:sequence #\Newline ,pt-greedy-whitespace) "")
    (:register
     (:sequence
      ,(delimiter-start delimiter)
      ,pt-greedy-whitespace
      (:register ,func-char)
      ,pt-greedy-whitespace
      (:register ,content)
      ,pt-whitespace
      ,end-char
      ,(delimiter-end delimiter)))))

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
  (let ((parser (make-tag-parser :delimiter (top-stack delimiter-stack))))
    (multiple-value-bind (start-idx end-idx parts-start parts-end)
        (cl-ppcre:scan parser template)
      (if (null start-idx)
          (reverse (cons template result)) ;; template doesn't contain tags
          (if (zerop start-idx)
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
                (parse-template (subseq template end-idx)
                                (cons (make-tag :func-char func-char
                                                :content content)
                                      result)
                                delimiter-stack))
              (parse-template (subseq template start-idx)
                              (cons (subseq template 0 start-idx) result)
                              delimiter-stack))))))

(defun collect-tokens (tokens target)
  (loop for token in tokens
        while (or (not (tag-p token))
                  (not (tag-equal-p token target)))
        collect token))

(defun render-implicit-iterator (tokens list out)
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
  (let ((tokens (if (stringp template-or-tokens)
                    (parse-template template-or-tokens)
                    template-or-tokens)))
    (loop for token-list on tokens do
      (let ((token (first token-list)))
        (typecase token
          (string
           (unless (eql (top-stack stack) *falsey-indicator*)
             ;(format t "printing: ~S~%" token)
             (princ token out)))
          (tag
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
                      (t (push tag-data stack))))

               ((#\^))
               
               ((#\/)
                (pop stack))
               
               ((#\{ #\&)
                (unless (eql (top-stack stack) *falsey-indicator*)
                  (princ (ensure-string tag-data) out)))

               ((#\>)
                (unless (eql (top-stack stack) *falsey-indicator*)
                  (render (cdr (assoc tag-content partials :test #'equalp)) stack partials out)))

               ((#\!))
               
               (t
                (unless (eql (top-stack stack) *falsey-indicator*)
                  (princ (cl-who:escape-string-all (ensure-string tag-data)) out))))))))))
  out)

(defun mustache-render (template data &optional partials)
  (get-output-stream-string
   (render template (list data) partials)))

