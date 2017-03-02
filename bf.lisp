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
  (remove-if #'null (loop :for char :across string
                       collect (parse-command char))))

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

(defun bf (str &optional debug)
  (let ((cells (make-array 30000 :initial-element 0 :element-type '(unsigned-byte 8)))
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
         (output-cb (progn (print (code-char (aref cells data-ptr)))
                           (incf command-pointer)))
         (input-cb (progn (setf (aref cells data-ptr) (funcall #'default-input))
                          (incf command-pointer)))
         (jump-forward (progn (if (zerop (aref cells data-ptr)) 
                                  (setf command-pointer (match-forward commands command-pointer))
                                  (incf command-pointer))))
         (jump-backward (progn (if (zerop (aref cells data-ptr))
                                   (incf command-pointer)
                                   (setf command-pointer (match-backward commands command-pointer)))))))

    (when debug (list :data-ptr data-ptr :command-pointer command-pointer :commands commands :clength (length commands)))))
    
  

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


(bf " ")


;;;;;;;;;; TESTING



(bf "++       Cell c0 = 2
> +++++  Cell c1 = 5

[        Start your loops with your cell pointer on the loop counter (c1 in our case)
< +      Add 1 to c0
> -      Subtract 1 from c1
]        End your loops with the cell pointer on the loop counter

At this point our program has added 5 to 2 leaving 7 in c0 and 0 in c1
BUT we cannot output this value to the terminal since it's not ASCII encoded!

To display the ASCII character '7' we must add 48 to the value 7!
48 = 6 * 8 so let's use another loop to help us!

++++ ++++  c1 = 8 and this will be our loop counter again
[
< +++ +++  Add 6 to c0
> -        Subtract 1 from c1
]
< .        Print out c0 which has the value 55 which translates to '7'!
")



(bf "
[ This program prints 'Hello World!' and a newline to the screen, its
  length is 106 active command characters. [It is not the shortest.]

  This loop is an 'initial comment loop', a simple way of adding a comment
  to a BF program such that you don't have to worry about any command
  characters. Any '.', ',', '+', '-', '<' and '>' characters are simply
  ignored, the '[' and ']' characters just have to be balanced. This
  loop and the commands it contains are ignored because the current cell
  defaults to a value of 0; the 0 value causes this loop to be skipped.
]
++++++++               Set Cell #0 to 8
[
    >++++               Add 4 to Cell #1; this will always set Cell #1 to 4
    [                   as the cell will be cleared by the loop
        >++             Add 2 to Cell #2
        >+++            Add 3 to Cell #3
        >+++            Add 3 to Cell #4
        >+              Add 1 to Cell #5
        <<<<-           Decrement the loop counter in Cell #1
    ]                   Loop till Cell #1 is zero; number of iterations is 4
    >+                  Add 1 to Cell #2
    >+                  Add 1 to Cell #3
    >-                  Subtract 1 from Cell #4
    >>+                 Add 1 to Cell #6
    [<]                 Move back to the first zero cell you find; this will
                        be Cell #1 which was cleared by the previous loop
    <-                  Decrement the loop Counter in Cell #0
]                       Loop till Cell #0 is zero; number of iterations is 8

The result of this is:
Cell No :   0   1   2   3   4   5   6
Contents:   0   0  72 104  88  32   8
Pointer :   ^

>>.                     Cell #2 has value 72 which is 'H'
>---.                   Subtract 3 from Cell #3 to get 101 which is 'e'
+++++++..+++.           Likewise for 'llo' from Cell #3
>>.                     Cell #5 is 32 for the space
<-.                     Subtract 1 from Cell #4 for 87 to give a 'W'
<.                      Cell #3 was set to 'o' from the end of 'Hello'
+++.------.--------.    Cell #3 for 'rl' and 'd'
>>+.                    Add 1 to Cell #5 gives us an exclamation point
>++.                    And finally a newline from Cell #6
")
