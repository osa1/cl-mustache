(in-package #:cl-mustache)

;; Utils ---------------------------------------------------------------

(defun remove-whitespace (str)
  "Remove trailing and following whitespace."
  (string-trim '(#\ ) str))

(defun string->keyword (str)
  (intern (string-upcase (remove-whitespace str)) "KEYWORD"))

(defun ensure-string (obj)
  (if obj
      (if (stringp obj)
          obj
          (write-to-string obj))
      ""))

(defun search-alist (name alist)
  (let* ((dot-pos (search "." name))
         (key (if dot-pos
                  (string->keyword (subseq name 0 dot-pos))
                  (string->keyword name)))
         (result (cdr (assoc key alist))))
    (if dot-pos
        (search-alist (subseq name (1+ dot-pos)) result)
        result)))

(defun search-stack (name stack)
  ;; stack is a list of plists
  (let* ((dot-pos (search "." name))
         (key (if dot-pos
                  (string->keyword (subseq name 0 dot-pos))
                  (string->keyword name)))
         (result
           (find key stack :test (lambda (key alist)
                                   (assoc key alist)))))
    (when result
      (if dot-pos
          (search-alist name result)
          (cdr (assoc key result))))))

;; ---------------------------------------------------------------------

(defstruct delimiter
  (start "{{")
  (end "}}"))

(defvar *delimiter* (make-delimiter :start "{{" :end "}}"))

(defvar *falsey-indicator* '(nil))

(defun add-to-stack (alist &optional stack)
  (if stack
      (cons alist stack)
      (list alist `((:delimiter . ,*delimiter*)))))

(defun pop-stack (stack)
  (values (first stack)
          (rest stack)))

(defun top-stack (stack)
  (car stack))

(defvar pt-whitespace '(:non-greedy-repetition 0 nil :whitespace-char-class))
(defvar pt-greedy-whitespace '(:greedy-repetition 0 nil :whitespace-char-class))
(defvar pt-non-whitespace '(:non-greedy-repetition 0 nil :non-whitespace-char-class)) 
(defvar pt-everything '(:non-greedy-repetition 0 nil :everything))

(defun make-tag-parser (&key (delimiter *delimiter*)
                          (func-char '(:alternation "=" "{" "^" "!" "&" "#" "/" :void))
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

(defun parse-delimiter (delimiter-tag-content)
  "Split delimiters in delimiter change tag and return new delimiter struct."
  (destructuring-bind (open-delimiter close-delimiter)
      (cl-ppcre:split '(:greedy-repetition 1 nil :whitespace-char-class)
                      delimiter-tag-content)
    (make-delimiter :start open-delimiter :end close-delimiter)))

(defun make-template-parser (template)
  (lambda (delimiter)
    (let ((parser (make-tag-parser :delimiter delimiter)))
      (multiple-value-bind (start-idx end-idx parts-start parts-end)
          (cl-ppcre:scan parser template)
        (if (null start-idx)
            (let ((token template))
              (setf template nil)
              token)
            (if (zerop start-idx)
                (let ((func-char (if (= (elt parts-start 1) (elt parts-end 1))
                                     nil
                                     (char template (elt parts-start 1))))
                      (content (subseq template (elt parts-start 2) (elt parts-end 2))))
                  (setf template (subseq template end-idx))
                  (make-tag :func-char func-char
                            :content content))
                (let ((token (subseq template 0 start-idx)))
                  (setf template (subseq template start-idx))
                  token)))))))

(defun get-next-token (parser delimiter)
  (funcall parser delimiter))

(defun render-lambda (lambda &optional param)
  (apply
   (eval
    (read-from-string lambda))
   param))

(defun alistp (list)
  ;; cl-json converts JSON dictionaries to plists and JSON lists to
  ;; lists, only way came to my mind to distinguish them is this
  (and (consp list)
       (consp (car list))
       (consp (caar list))
       (keywordp (caaar list))))

(defun render-tag (tag stack out)
  ;; TODO: handle wrong nested sections
  (if (not (equal (top-stack stack) *falsey-indicator*))
      (case (tag-func-char tag)
        ((#\=)
         (let ((new-delimiter (parse-delimiter (tag-content tag))))
           (multiple-value-bind (top rest)
               (pop-stack stack)
             (add-to-stack (cons `(:delimiter . ,new-delimiter) top)
                           rest))))
        ((#\#)
         (let ((content (search-stack (tag-content tag) stack)))
           ;(format t "content: ~S~%" content)
           (if (alistp content)
               'alist
               (if (eq content t)
                   (add-to-stack (top-stack stack) stack)
                   ;; basically I'm putting *falsey-indicator* to the
                   ;; stack when I don't want to render content of a section
                   (if content
                       (add-to-stack content stack)
                       (add-to-stack *falsey-indicator* stack))))))
        ((#\/)
         (nth-value 1 (pop-stack stack)))
        ((#\{ #\&)
         (princ (ensure-string (search-stack (tag-content tag) stack)) out)
         stack)
        ((#\^)
         (let ((content (search-stack (tag-content tag) stack)))
           (if (null content)
               (add-to-stack (top-stack stack) stack)
               (add-to-stack *falsey-indicator* stack))))
        (t
         (let ((data (search-stack (tag-content tag) stack)))
           ;; (if (string= (tag-content tag) "lambda")
           ;;     (render-tag (render-lambda (search-alist "lisp" data) nil) stack out)
           ;;     (princ (cl-who:escape-string-all
           ;;             (ensure-string data)) out)))
           (princ (cl-who:escape-string-all
                   (ensure-string data)) out)
         stack)))
      (if (eql #\/ (tag-func-char tag))
          (nth-value 1 (pop-stack stack))
          stack)))

(defun render (template stack
               &optional (out (make-string-output-stream)))
  (let ((parser (make-template-parser template)))
    (loop for s = (get-next-token parser (search-stack "delimiter" stack))
          while s do
            ;(format t "s: ~S~%" s)
            (typecase s
              (string
               (unless (equal (top-stack stack) *falsey-indicator*)
                 ;(format t "top-stack: ~S~%" (top-stack stack))
                 ;(format t "s: ~S~%" s)
                 (princ s out)))
              (tag
               (setf stack (render-tag s stack out))
               ;(format t "new stack: ~S~%" stack)
               ))))
  (get-output-stream-string out))

(defun mustache-render (template data &optional partial)
  (declare (ignore partial))
  (render template (add-to-stack data)))

