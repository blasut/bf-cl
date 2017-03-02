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

;;;;; LOOP
;;
;; START:
;; [1, 1, 0, 0, 0, 0, 0, 0]
;;     ^
;; Command:
;; [-]
;;
;; After:
;; [1, 0, 0, 0, 0, 0, 0, 0]
;;     ^
;;

;; [0,0,0,0,0,0]
;;  ptr
;; instrc: [ == läs tills matchande ]
;;         ]
;; Vad är matchande ]? Det är beroende på hur många [ innan.
;; Definera matchande:
;; En matchande bracket är ... 

(in-package :cl-user)
(defpackage bf
  (:use :cl
        :prove))
(in-package :bf)

;; Model

(defun make-cells ()
  (make-array 1 :element-type 'fixnum))  

;;; Commands

(defun parse-commands (string)
  (loop :for char :across string
     collect (parse-command char)))

(defun parse-command (char)
  (cond ((char= char #\+) 'inc)
        ((char= char #\-) 'dec)
        ((char= char #\>) 'inc-p)
        ((char= char #\<) 'dec-p)
        ((char= char #\.) 'output-cb)
        ((char= char #\,) 'input-cb)
        ((char= char #\[) 'jump-forward)
        ((char= char #\]) 'jump-backward)))

;; First: write interpreter: which is something that reads every line and executes it one by one
(defun default-input ()
  (progn (print "please insert new char: ") (char-int (read-char))))

(defun bf (str)
  (let ((cells (make-array 8 :element-type 'fixnum))
        (data-ptr 0)
        (command-pointer 0)
        (commands (parse-commands str)))
    (loop :while (< command-pointer (length commands))
       :do 
       (case (elt commands command-pointer)
         (inc (progn (setf (aref cells data-ptr) (1+ (aref cells data-ptr)))
                     (incf command-pointer)))
         (dec (progn (setf (aref cells data-ptr) (1- (aref cells data-ptr)))
                     (incf command-pointer)))
         (inc-p (progn (incf data-ptr)
                       (incf command-pointer)))
         (dec-p (progn (decf data-ptr)
                       (incf command-pointer)))
         (output-cb (progn (print (aref cells data-ptr))
                           (incf command-pointer)))
         (input-cb (progn (setf (aref cells data-ptr) (funcall #'default-input))
                          (incf command-pointer)))
         (jump-forward (progn (if (zerop (aref cells data-ptr)) 
                                  (setf command-pointer (match-forward commands command-pointer))
                                  (incf command-pointer))))
         (jump-backward (progn (if (zerop (aref cells data-ptr))
                                   (incf command-pointer)
                                   (setf command-pointer (match-backward commands command-pointer)))))))

    (list :cells cells :data-ptr data-ptr :command-pointer command-pointer :commands commands :clength (length commands))))
    
  

(defun match-forward (commands cp) 
  (let ((level 0)
        (match 0)
        (cmds (subseq commands (1+ cp))))
    (loop
       :while (and (\= match 0))
       :for c :in cmds
       :for i :from cp :upto (length commands)
       :do (case c
             ((jump-forward)  (incf level))
             ((jump-backward) (progn (if (= level 0)
                                         (setf match i)
                                         (decf level)))))
       :finally (return (1+ match)))))


(defun match-backward (commands cp) 
  (let ((level 0)
        (match 0)
        (cmds (reverse (subseq commands 0 (1- cp)))))
    (loop
       :while (and (\= match 0))
       :for c :in cmds
       :for i :from cp :downto 0
       :do (case c
             ((jump-forward) (progn (if (= level 0)
                                        (setf match i)
                                        (decf level))))
             ((jump-backward) (incf level)))
       :finally (return (1- match)))))

(subtest "Testing matching forward"
  (is 10 (match-forward (parse-commands "+[-[[[]]]-]---------------------[]") 1))
  (is 3  (match-forward (parse-commands "+[-]") 1)))

(subtest "Testing matching backward"
  (is 2 (match-backward (parse-commands "+[-]") 4))
  (is 1 (match-backward (parse-commands "[-]")  3)))

(pprint (bf "+++[-]"))
(pprint (bf "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"))

