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
      (setf (input-cb m) (lambda () (progn (print "please insert new byte: ") (- (char-int (read-char)) (char-int #\0)))))))

(defmethod print-object ((m model) stream)
  (print-unreadable-object (m stream :type t)
    (with-slots (cells pointer) m
      (format stream ":cells ~a :pointer ~d " cells pointer))))

(defun get-current-cell-value (model)
  (aref (cells model) (pointer model)))

;;; Commands

(defun parse-commands (string)
  (loop :for char :across string
     collect (parse-command char)))

(defun parse-command (char)
  (cond ((char= char #\+) #'command-increase)
        ((char= char #\-) #'command-decrease)
        ((char= char #\>) #'command-increase-pointer)
        ((char= char #\<) #'command-decrease-pointer)
        ((char= char #\.) #'command-run-output-cb)
        ((char= char #\,) #'command-run-input-cb)
        (t (error "Unknown brainfuck command: ~@c" char))))

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

(defun run-model (model)
  (loop :for c :in (commands model)
     :do (funcall c model)
     :finally (return model)))

(defun bf (bf-string)
  (let ((model (make-instance 'model :commands (parse-commands bf-string))))
    (run-model model)))

(pprint (parse-commands "++-><.,"))

(pprint (parse-commands "++,++"))

(pprint (bf "++++"))

(pprint (bf "++->+<>>"))


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
      ;(run-test "+[-]" 0 '(0 0 0 0 0 0 0 0))
      ;(run-test "++>+++++[<+>-]" 0 '(1 0 0 0 0 0 0 0))

      t)))


(run-tests)
