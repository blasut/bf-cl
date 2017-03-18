;;
;; Brainfuck:
;;

(in-package :cl-user)
(defpackage bf
  (:use :cl)
  (:export :parse
           :compile-brainfuck))
(in-package :bf)

;; Very nice, an inspiration
;; http://beautifulracket.com/bf/the-parser.html

(defun parse-command (char)
  (case char
    (#\+ 'inc)
    (#\- 'dec)
    (#\> 'inc-p)
    (#\< 'dec-p)
    (#\. 'output-cb)
    (#\, 'input-cb)
    (#\[ 'jump-forward)
    (#\] 'jump-backward)))

;; not really a general purpose tokenizer because we assume every token will match to a single char
(defun tokenize (string)
  (remove-if #'null (loop :for char :across string
                       collect (parse-command char))))

;; tokenize svara med en platt lista av tokens, det är upp till parse att bestämma hur den listan ska hanteras. 
;; parse : str -> bf-program
;; parse "+++[<-]" -> (bf-program (bf-op "+") (bf-op "+") (bf-op "+") (bf-loop :decp dec)))
(defun parse (str)
  (parse-tokens (tokenize str)))

;; parse-tokens tar en platt lista med tokens,
;; vi vill mappa varje token mot grammatiken och returna en lista
;; inte riktigt exakt som grammatiken, vi skippar '[' och ']' i resultatet
;; ecase används för att vi har redan parsat allt till tokens
(defun parse-tokens (tokens)
  (let ((string-of-tokens (loop :for token :in tokens
                             :collecting (ecase token
                                           (inc "(bf-op \"+\")")
                                           (dec "(bf-op \"-\")")
                                           (inc-p "(bf-op \">\")")
                                           (dec-p "(bf-op \"<\")")
                                           (output-cb "(bf-op \".\")")
                                           (input-cb "(bf-op \",\")")
                                           (jump-forward "(bf-loop ")
                                           (jump-backward ")")) :into fragments
                             :finally (return
                                        (format nil "(bf-program ~a)" (apply #'concatenate 'string fragments))))))
    (read-from-string string-of-tokens)))

(defmacro bf-op (op)
  (cond ((string= op "+") `(incf (aref cells ptr)))
        ((string= op "-") `(decf (aref cells ptr)))
        ((string= op ">") `(incf ptr))
        ((string= op "<") `(decf ptr))
        ((string= op ".") `(write-char (code-char (aref cells ptr))))
        ((string= op ",") `(setf (aref cells ptr) (char-code (read-char))))))

(defmacro bf-loop (&body body)
  `(do ((val (aref cells ptr) (aref cells ptr)))
       ((= val 0))
     ,@body))

(defmacro bf-program (&body body)
  `(let ((ptr 0)
         (cells (make-array 30000 :initial-element 0 :element-type '(unsigned-byte 8))))
     ,@body))

(defun optimize-brainfuck (forms)
  forms)

(defmacro compile-brainfuck (src)
  (declare (optimize (debug 3)))
  (parse src))

(compile-brainfuck "+++[-]")
(compile-brainfuck "+++[-]")
(compile-brainfuck "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
(compile-brainfuck "+[-].")

(parse "+[-].+++[-].")
(compile-brainfuck "+[-].+++[-].")

(compile-brainfuck "[]")

(compile-brainfuck "[]++++++++++[>>+>+>++++++[<<+<+++>>>-]<<<<-]")

(compile-brainfuck "[]++++++++++[>>+>+>++++++[<<+<+++>>>-]<<<<-] \"A*$\";?@![#>>+<<]>[>>]<<<<[>++<[-]]>.>. ")

(compile-brainfuck "[<]")

(defmacro defbf (name src)
  `(defun ,name ()
     (compile-brainfuck ,src)))
