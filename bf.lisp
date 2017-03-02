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
;; When we see a '[', it means that the pointer should not change, and that all commands till the ']' should run with the pointer starting at the cell before '[', until the cell value of the cell before the '[' is zero 
;;
;; A loop is saying: run these commands between '[' and ']' starting the pointer before the '[' 
;; Could we say that looping is just running commands withouth changing the pointer?
;; No, because other commands do change the pointer, maybe a loop takes the model and the commands
;; So a loop:
;; - If the value of the cell the pointer is looking at is 0, terminate
;;   - terminate means continue with the code reading AFTER the loop
;;   - the CELL pointer should be saved to the beginning value
;; - If the value of the cell the pointer is looking at is NOT 0:
;;   - Save the pointer
;;   - Take the commands between '[' & ']' and run the as usual. 

;; Model

(defun make-cells ()
  (make-array 8 :initial-element 0))  

(defclass model ()
  ((cells
    :initarg :cells
    :initform (make-cells)
    :accessor cells)
   (pointer
    :initarg :pointer
    :initform 0
    :accessor pointer)
   (output-cb
    :initarg :output
    :initform nil
    :accessor output-cb)
   (input-cb
    :initarg :input
    :initform nil
    :accessor input-cb)
   (commands
    :initarg :commands
    :initform '()
    :accessor commands)))

(defmethod initialize-instance :after ((m model) &rest args)
  (if (null (output-cb m))
      (setf (output-cb m) (lambda (x) (print x))))
  (if (null (input-cb m))
      (setf (input-cb m) (lambda () (progn (print "please insert new char: ") (char-int (read-char)))))))

(defmethod print-object ((m model) stream)
  (print-unreadable-object (m stream :type t)
    (with-slots (cells pointer commands) m
      (format stream ":pointer ~d  :cells ~a :commands ~a" pointer cells  commands))))

(defun get-current-cell-value (model)
  (aref (cells model) (pointer model)))

(defun cell-value (model pointer)
  (aref (cells model) pointer))

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
        ((char= char #\[) 'start-loop)
        ((char= char #\]) 'end-loop)
        (t (error "Unknown brainfuck command: ~@c" char))))

(defun get-func (command)
  (cond ((equal command 'inc)   #'command-increase)
        ((equal command 'dec)   #'command-decrease)
        ((equal command 'inc-p) #'command-increase-pointer)
        ((equal command 'dec-p) #'command-decrease-pointer)
        ((equal command 'output-cb)  #'command-run-output-cb)
        ((equal command 'input-cb)  #'command-run-input-cb)
        ((equal command 'start-loop)  #'command-start-loop)
        ((equal command 'end-loop)  #'command-noop)
        (t (error "Unknown brainfuck command: ~@a" command))))

(defun command-increase (model)
  (setf (aref (cells model) (pointer model)) ;; position
        (1+ (aref (cells model) (pointer model))))) ;; new value

(defun command-decrease (model)
  (setf (aref (cells model) (pointer model)) ;; position
        (1- (aref (cells model) (pointer model))))) ;; new value

(defun command-increase-pointer (model)
  (setf (pointer model)
        (1+ (pointer model))))

(defun command-decrease-pointer (model)
  (setf (pointer model)
        (1- (pointer model))))

(defun command-run-output-cb (model)
  (funcall (output-cb model) (get-current-cell-value model)))

(defun command-run-input-cb (model)
  (let ((new-val (funcall (input-cb model))))
    (setf (aref (cells model) (pointer model))
          new-val))) 

(defun command-noop (x)
  (declare (ignore x)))

(defun run-cmd (model c)
  (funcall (get-func c) model))

(defun command-start-loop (model)
  (let ((beg-pos (pointer model))
        (loop-commands (loop-commands model)))
    (loop :while (> (cell-value model beg-pos) 0) 
       :do (progn
             (setf (pointer model) beg-pos)
             (loop :for c :in loop-commands
                :do (run-cmd model c)))
       :finally (setf (commands model)
                      (subseq (commands model) (1+ (length loop-commands)))))))

(defun loop-commands (model)
  (butlast (loop :for c :in (commands model)
              :collect c
              :until (equalp c 'end-of-loop))))

(defun run-model (model)
  (loop :while (> (length (commands model)) 0) 
     :do (let ((cmd (car (commands model))))
           (setf (commands model) (cdr (commands model)))  
           (run-cmd model cmd))
     :finally (return model)))

(defun bf (bf-string)
  (let ((model (make-instance 'model :commands (parse-commands bf-string))))
    (run-model model)))

(defun bf-print (model)
  (loop :for c :across (cells model) :do (princ (code-char c))))

(defun bf-run (bf-string)
  (bf-print (bf bf-string)))

;;; TESTS

(defun run-tests()
  (flet ((run-test (str pointer init-cont)
           (let ((model (bf str))
                 (test (make-instance 'model
                                      :pointer pointer
                                      :cells (make-array 8 :initial-contents init-cont))))
             (progn
               (assert (equalp (cells model)   (cells test)))
               (assert (equalp (pointer model) (pointer test)))))))
    (progn
      (run-test "++"  0 '(2 0 0 0 0 0 0 0))
      (run-test "++-" 0 '(1 0 0 0 0 0 0 0))
      (run-test "++->" 1 '(1 0 0 0 0 0 0 0))
      (run-test "++-><" 0 '(1 0 0 0 0 0 0 0))
      (run-test "++>+++++" 1 '(2 5 0 0 0 0 0 0))
      (run-test "++[-]" 0 '(0 0 0 0 0 0 0 0))
      (run-test "++>+++++[<+>-]" 1 '(7 0 0 0 0 0 0 0))
      t)))


(run-tests)

(pprint (parse-commands "++-><.,"))

(pprint (parse-commands "++,++"))

(pprint (parse-commands "++[-]++"))


(bf "++,++")
(bf "++,")

(pprint (bf "++++"))

(pprint (bf "++[-]"))

(pprint (bf "++->+<>>"))

(bf-run "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
;; Prints 9


