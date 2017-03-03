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

(defun default-input ()
  (progn (print "please insert new char: ") (char-int (read-char))))

(defun match-forward (commands cp)
  (do ((count 1 (case (elt commands index)
                  (jump-forward (1+ count))
                  (jump-backward (1- count))
                  (t count)))
       (index (1+ cp) (1+ index)))
      ((zerop count) (1- index))))

(defun match-backward (commands cp)
  (do ((count 1 (case (elt commands index)
                  (jump-forward (1- count))
                  (jump-backward (1+ count))
                  (t count)))
       (index (1- cp) (1- index)))
      ((zerop count) (1+ index))))

(defun bf (str &optional debug)
  (let ((cells (make-array 30000 :initial-element 0 :element-type '(unsigned-byte 8)))
        (data-ptr 0)
        (command-pointer 0)
        (commands (parse-commands str)))
    (loop :while (< command-pointer (length commands))
       :do 
       (case (elt commands command-pointer)
         (inc           (incf (elt cells data-ptr)))
         (dec           (decf (elt cells data-ptr)))
         (inc-p         (incf data-ptr))
         (dec-p         (decf data-ptr))
         (output-cb     (print (code-char (aref cells data-ptr))))
         (input-cb      (setf (aref cells data-ptr) (funcall #'default-input)))
         (jump-forward  (when (zerop (aref cells data-ptr)) 
                          (setf command-pointer (match-forward commands command-pointer))))
         (jump-backward (unless (zerop (aref cells data-ptr))
                          (setf command-pointer (match-backward commands command-pointer)))))
       (incf command-pointer))
    (when debug (list :data-ptr data-ptr :command-pointer command-pointer :commands commands :clength (length commands)))))


(pprint (bf "+++[-]"))
(pprint (bf "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"))



(bf " ")
;;;; Tests

;; This is for testing i/o; give it a return followed by an EOF. (Try it both
;; with file input--a file consisting only of one blank line--and with
;; keyboard input, i.e. hit return and then ctrl-d (Unix) or ctrl-z
;; (Windows).)
;; It should give two lines of output; the two lines should be identical, and
;; should be lined up one over the other. If that doesn't happen, ten is not
;; coming through as newline on output.
;; The content of the lines tells how input is being processed; each line
;; should be two uppercase letters.
;; Anything with O in it means newline is not coming through as ten on input.
;; LK means newline input is working fine, and EOF leaves the cell unchanged
;; (which I recommend).
;; LB means newline input is working fine, and EOF translates as 0.
;; LA means newline input is working fine, and EOF translates as -1.
;; Anything else is fairly unexpected.
(bf ">,>+++++++++,>+++++++++++[<++++++<++++++<+>>>-]<<.>.<<-.>.>.<<.")

;; Goes to cell 30000 and reports from there with a #. (Verifies that the)
;; array is big enough.
(bf " ++++[>++++++<-]>[>+++++>+++++++<<-]>>++++<[[>[[>>+<<-]<]>>>-]>-[>+>+<<-]>] +++++[>+++++++<<++>-]>.<<. ")


;;These next two test the array bounds checking. Bounds checking is not
;;essential, and in a high-level implementation it is likely to introduce
;;extra overhead. In a low-level implementation you can get bounds checking
;;for free by using the OS's own memory protections; this is the best
;;solution, which may require making the array size a multiple of the page
;;size.
;;Anyway. These two programs measure the 'real' size of the array, in some
;;sense, in cells left and right of the initial cell respectively. They
;;output the result in unary; the easiest thing is to direct them to a file
;;and measure its size, or (on Unix) pipe the output to wc. If bounds
;;checking is present and working, the left should measure 0 and the right
;;should be the array size minus one.
(bf "
+[<+++++++++++++++++++++++++++++++++.]

+[>+++++++++++++++++++++++++++++++++.]
")

;; Tests for several obscure problems. Should output an H.
(bf " []++++++++++[>>+>+>++++++[<<+<+++>>>-]<<<<-] \"A*$\";?@![#>>+<<]>[>>]<<<<[>++<[-]]>.>. ")

;; Should ideally give error message "unmatched [" or the like, and not give any output. Not essential.
(bf "+++++[>+++++++>++<<-]>.>.[]")

;; Should ideally give error message "unmatched ]" or the like, and not give any output. Not essential.
(bf "+++++[>+++++++>++<<-]>.>.[]")



;;;;

(bf "++       Cell c0 = 2)
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

(bf "
,+[                         Read first character and start outer character reading loop
    -[                       Skip forward if character is 0
        >>++++[>++++++++<-]  Set up divisor (32) for division loop
                               (MEMORY LAYOUT: dividend copy remainder divisor quotient zero zero)
        <+<-[                Set up dividend (x minus 1) and enter division loop
            >+>+>-[>>>]      Increase copy and remainder / reduce divisor / Normal case: skip forward
            <[[>+<-]>>+>]    Special case: move remainder back to divisor and increase quotient
            <<<<<-           Decrement dividend
        ]                    End division loop
    ]>>>[-]+                 End skip loop; zero former divisor and reuse space for a flag
    >--[-[<->+++[-]]]<[         Zero that flag unless quotient was 2 or 3; zero quotient; check flag
        ++++++++++++<[       If flag then set up divisor (13) for second division loop
                               (MEMORY LAYOUT: zero copy dividend divisor remainder quotient zero zero)
            >-[>+>>]         Reduce divisor; Normal case: increase remainder
            >[+[<+>-]>+>>]   Special case: increase remainder / move it back to divisor / increase quotient
            <<<<<-           Decrease dividend
        ]                    End division loop
        >>[<+>-]             Add remainder back to divisor to get a useful 13
        >[                   Skip forward if quotient was 0
            -[               Decrement quotient and skip forward if quotient was 1
                -<<[-]>>     Zero quotient and divisor if quotient was 2
            ]<<[<<->>-]>>    Zero divisor and subtract 13 from copy if quotient was 1
        ]<<[<<+>>-]          Zero divisor and add 13 to copy if quotient was 0
    ]                        End outer skip loop (jump to here if ((character minus 1)/32) was not 2 or 3)
    <[-]                     Clear remainder from first division if second division was skipped
    <.[-]                    Output ROT13ed character from copy and clear it
    <-,+                     Read next character
]                            End character reading loop

")


(bf "
[bsort.b -- bubble sort
(c) 2016 daniel b. cristofani
http://brainfuck.org/]

>>,[>>,]<<[
[<<]>>>>[
<<[>+<<+>-]
>>[>+<<<<[->]>[<]>>-]
<<<[[-]>>[>+<-]>>[<<<+>>>-]]
>>[[<+>-]>>]<
]<<[>>+<<-]<<
]>>>>[.>>]

[this program sorts the bytes of its input by bubble sort.]
")

