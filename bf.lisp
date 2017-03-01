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
    :accessor input-cb)))

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

(defun bf (bf-string)
  (let ((model (make-instance 'model))
        (commands (parse-commands bf-string)))
    (progn
      (loop :for command :in commands
         :do (funcall command model))
      model)))

(pprint (parse-commands "++-><.,"))

(pprint (parse-commands "++,++"))

(pprint (bf "++,++"))

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
      )))

(run-tests)
