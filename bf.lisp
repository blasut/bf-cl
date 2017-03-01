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

(defclass model ()
  ((cells
    :initarg :cells
    :initform (make-cells)
    :accessor cells)
   (pointer
    :initform 0
    :accessor pointer)))

(defmethod print-object ((m model) stream)
  (print-unreadable-object (m stream :type t)
    (with-slots (cells pointer) m
      (format stream ":cells ~a :pointer ~d " cells pointer))))

(defun parse-commands (string)
  (loop :for char :across string
     collect (parse-command char)))

(defun parse-command (char)
  (cond ((char= char #\+) #'command-increase)
        ((char= char #\-) #'command-decrease)
        (t (error "Unknown brainfuck command: ~@c" char))))

(defun command-increase (model)
  (setf (aref (cells model) (pointer model)) ;; position
        (1+ (aref (cells model) (pointer model))))) ;; new value

(defun command-decrease (model)
  (setf (aref (cells model) (pointer model)) ;; position
        (1- (aref (cells model) (pointer model))))) ;; new value

(defun bf (bf-string)
  (let ((model (make-instance 'model))
        (commands (parse-commands bf-string)))
    (progn
      (loop :for command :in commands
         :do (funcall command model))
      model)))

(pprint (parse-commands "++-"))
(pprint (bf "++-"))

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
      (run-test "++-" 0 '(1 0 0 0 0 0 0 0)))))

(run-tests)
