;;
;; Brainfuck:
;;

(in-package :cl-user)
(defpackage bf
  (:use :cl))
        ;:prove))
(in-package :bf)

;;; Commands

(defun parse-commands (string)
  (remove-if #'null (loop :for char :across string
                       collect (parse-command char))))

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


