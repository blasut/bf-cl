;;
;; Brainfuck:
;;
;; Start:
;; [0, 0, 0, 0, 0, 0, 0, 0]
;;  ^
;;
;; Command:
;; +
;;
;; After:
;; [1, 0, 0, 0, 0, 0, 0, 0]
;;  ^
;;
;; Command:
;; +
;;
;; After:
;; [2, 0, 0, 0, 0, 0, 0, 0]
;;  ^
;;
;; Command:
;; >
;;
;; After:
;; [2, 0, 0, 0, 0, 0, 0, 0]
;;     ^

(defun make-cells ()
  (make-array 8 :initial-element 0))  

(defun parse-commands (string)
  (loop :for char :across string
     collect (parse-command char)))

(defun parse-command (char)
  (cond ((char= char #\+) #'1+)
        (t (error "Unknown brainfuck command"))))

(defun bf (bf-string)
  (let ((cells (make-cells))
        (commands (parse-commands bf-string)))
    (list commands cells)))

(pprint (bf "++"))

(assert (bf "++") (make-array 8 :initial-contents '(2 0 0 0 0 0 0 0)))

