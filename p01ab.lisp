;;;;    Advent of Code 2021 - Day 1 task A & B
;;;;    Solutions in Lisp
;;;;    Also solving in other languages like Haskell, Rust, Swift and (old school) BASIC
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    The count of increased measurements is: 1692
;;;;    The count of increased triplets is: 1724
;;;;
;;;;    (cl) by Arno Jacobs, 01-12-2021

;;;; First - read the data-set
(defun read-data (&optional (file "data/inputDay01_2021.txt"))
  "Returns the numerical data in FILE as a list."
  (with-open-file (data file)
    (let (*read-eval*)   ; set *read-eval* to NIL to avoid malicious code in the input file
      (loop :for line = (read-line data nil nil)
            :while line
            :collect (read-from-string (concatenate 'string line))))))
;;;         :collect (read-from-string (concatenate 'string "(" line ")"))))))

;;; Some constants
(defvar *DAY1* (read-data))

;;; Work list to find the number of increasing measurements
;;; Task one
(defun count-increases (numbers)
    (setq *rv* 0)
    (setq *pv* (car numbers))
    (loop for e1 in numbers do
        (if (> e1 *pv*) 
            (setq *rv* (+ *rv* 1)))
        (setq *pv* e1)) *rv* )

;;; Create a list of summed triplets from the input list
;;; Task two, part one
(defun sum3 (numbers)
    (setq *e1* (car numbers))
    (setq *e2* (car (cdr numbers)))
    (setq *e3* (car (cdr (cdr numbers))))
    (+ *e1* *e2* *e3*))

(defun sum-triplet (numbers rl)
    (setq *rv* (sum3 numbers)) 
    (setq *nrl* (append rl (cons *rv* '())))
    (if (> 4 (length numbers))
        '()
        (sum-triplet (cdr numbers) *nrl*)) *nrl*)

;;; The main program
(progn 
    (princ "Advent of Code 2021 - day 1 - both parts in Lisp")
    (princ #\newline)
    (princ "The count of increased measurements is: ")
    (princ (count-increases *day1*))
    (princ #\newline)
    (princ "The count of increased triplets is:     ")
    (princ (count-increases (sum-triplet *day1* '())))
    (princ #\newline)
    (princ "0K.")
    (princ #\newline)
    (princ #\newline)
)

