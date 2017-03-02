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
  (make-array 1 :element-type 'fixnum))  

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

(defun default-input ()
  (progn (print "please insert new char: ") (char-int (read-char))))

(defun default-output (x)
  (format t "~a" (code-char x)))

(defmethod initialize-instance :after ((m model) &rest args)
  (if (null (output-cb m))
      (setf (output-cb m) #'default-output))
  (if (null (input-cb m))
      (setf (input-cb m) #'default-input)))

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
        (t (error "Unknown brainfuck command: ~@a" command))))

(defun command-increase (model)
  (setf (aref (cells model) (pointer model)) ;; position
        (1+ (aref (cells model) (pointer model))))) ;; new value

(defun command-decrease (model)
  (setf (aref (cells model) (pointer model)) ;; position
        (1- (aref (cells model) (pointer model))))) ;; new value

(defun command-increase-pointer (model)
  ;; pointer is zero based
  (check-type (cells model) (simple-array fixnum 1))
  (let* ((pos (pointer model))
         (new-pos (1+ pos)))
    (if (= new-pos (length (cells model)))
        (let ((new (make-array (1+ (length (cells model))) :element-type 'fixnum)))
          (declare (optimize speed))
          (replace new (cells model) :start1 0 :end1 new-pos)
          (setf (cells model) new)))
    (setf (pointer model) new-pos)))

(defun command-decrease-pointer (model)
  (setf (pointer model)
        (1- (pointer model))))

(defun command-run-output-cb (model)
  (funcall (output-cb model) (get-current-cell-value model)))

(defun command-run-input-cb (model)
  (let ((new-val (funcall (input-cb model))))
    (setf (aref (cells model) (pointer model))
          new-val))) 

(defun run-cmd (model c)
  (funcall (get-func c) model))

(defun loop-commands (model)
  (butlast (loop :for c :in (commands model)
              :collect c
              :until (equalp c 'end-loop))))

(defun run-model (model)
  (loop :while (> (length (commands model)) 0) 
     :do (let ((cmd (car (commands model))))
           (setf (commands model) (cdr (commands model)))  
           (if (eq cmd 'start-loop)
               (let ((beg-pos (pointer model))
                     (loop-commands (loop-commands model)))
                 (loop :while (> (cell-value model beg-pos) 0) 
                    :do (progn
                          (setf (pointer model) beg-pos)
                          (loop :for c :in loop-commands
                             :do (run-cmd model c)))
                    :finally (setf (commands model)
                                   (subseq (commands model) (1+ (length loop-commands))))))
               (run-cmd model cmd)))
     :finally (return model)))

(defun model->chars (model)
  (loop :for c :across (cells model) :collect (code-char c)))

(defun bf (bf-string)
  (let ((model (make-instance 'model :commands (parse-commands bf-string))))
    (run-model model)))

(defun bf-print (bf-string)
  (print (coerce (model->chars (bf bf-string)) 'string)))

;;; TESTS

(defun run-tests()
  (flet ((run-test (&key str pointer cells)
           (let ((model (bf str))
                 (test (make-instance 'model
                                      :pointer pointer)))
             (progn
               (format t "Running test: ~a ~&Expected: ~%Cells: ~a to return ~a ~&Pointer: ~a to return ~a ~2%" str cells (cells model) pointer (pointer model))
               (assert (equalp (cells model)   cells))
               (assert (equalp (pointer model) (pointer test)))))))
    
    (progn
      (run-test  :pointer 0 :cells #(2)    :str "++")
      (run-test  :pointer 0 :cells #(1)    :str "++-")
      (run-test  :pointer 1 :cells #(1 0)  :str "++->")
      (run-test  :pointer 0 :cells #(1 0)  :str "++-><")
      (run-test  :pointer 1 :cells #(2 5)  :str "++>+++++")
      (run-test  :pointer 0 :cells #(0)    :str "++[-]")
      (run-test  :pointer 1 :cells #(7 0)  :str "++>+++++[<+>-]")
      (run-test  :pointer 0 :cells #(55 0) :str "++>+++++[<+>-]++++++++[<++++++>-]<.")
      t)))


(run-tests)

(pprint (parse-commands "++-><.,"))

(pprint (parse-commands "++,++"))

(pprint (parse-commands "++[-]++"))

(pprint (bf "++++"))
(pprint (bf "++[-]"))
(pprint (parse-commands "++>+++++[<+>-]+"))
(pprint (bf "++>+++++[<+>-]+"))
(pprint (bf "++>+++++[<+>-]++++++++"))
(bf-print "++>+++++")
(bf-run "++>+++++[<+>-]++++++++[<++++++>-]<.")
