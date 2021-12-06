;;;;    Advent of Code 2021 - Day 6 task A & B
;;;;    Solutions in Lisp
;;;;    Also solving in other languages like Haskell
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    The number of lanternfish after  80 days is: 388739
;;;;    The number of lanternfish after 256 days is: 1741362314973
;;;;
;;;;
;;;;    (cl) by Arno Jacobs, 06-12-2021

;;;; 
(defvar *iter1* 80)
(defvar *iter2* 256)

;;; #\7 -> 7
(defun to-int (c)
    (if (characterp c)
        (parse-integer (string c))))

;;; For this task
;;; "1,2,3,4,5" -> '(1 2 3 4 5)
;;;
(defun string-to-digits-list (str)
    (mapcar #'to-int (remove #\, (coerce str 'list))))

;;; Read data and convert to numbers list in sublist of size 4
;;;
;;; Like: ((259 377 259 599) (120 758 977 758) (2 4 10 12))
;;;
;;; The first being a vertical line from (259,377) to (259,599)
;;; The second being a horizontal line from (120,758) to (977,758)
;;; The third being a 45 degrees diagonal line from (2,4) to (10,12)
(defun read-data (&optional (file "data/inputDay06_2021.txt"))
  "Returns the numerical data in FILE as a list."
  (with-open-file (data file)
    (let (*read-eval*)   
        (loop   :for        line = (read-line data nil nil)
                :while      line
                :collect    (string-to-digits-list line) ))))


;;; The input data converted to a list of digits
(defvar *DAY6* (car (read-data)))

;;; Return a 1 if the first element of 'el' is equal to 'e' 
;;; otherwise return a 0
(defun plus1 (e el)
    (if (= e (car el))
        1
        0))

;;; Count the number if digits 'e' from a list 'el'
(defun count-digit (e el)
    (if (eq el NIL)
        0
        (+ (plus1 e el) (count e (cdr el)))))
        
;;; Count all the digits [0..9] from a list 'dl'
;;; The 'dl' is the input list for this days tasks        
(defun count-digits (dl)
    (loop for d in '(0 1 2 3 4 5 6 7 8) do 
        (setq *dc* (count-digit d dl))
        collect *dc*))

;;; Calculate the sum of a list of numbers
(defun sum-list (lst) 
    (reduce '+ lst))

;;; iterate one step - the timers drops one count
;;; new fish are generated
(defun next-count (lst)
    (setq *zeros*   (car lst))
    (setq *sevens*  (nth 7 lst))
    (setq *new6*    (+ *zeros* *sevens*))
    (setq *eights*  (nth 8 lst))
    (setq *tail6*   (subseq lst 1 7))
    (setq *new678*  (cons *new6* (cons *eights* (cons *zeros* '()))))
    (append *tail6* *new678*))

;;; Let's work...
(defun endless-lanternfish (c dcl) 
    (if (= c 0)
        (sum-list dcl)
        (endless-lanternfish (- c 1) (next-count dcl))))

;;; The main program
(progn 
    (princ "Advent of Code 2021 - day 6 - both parts in Lisp")
    (princ #\newline)

    (setq *dcl* (count-digits *day6*))
    (princ "The number of lanternfish after  ")
    (princ *iter1*)
    (princ " days is: ")
    (princ (endless-lanternfish *iter1* *dcl*))
    (princ #\newline)
    
    (princ "The number of lanternfish after  ")
    (princ *iter2*)
    (princ " days is: ")
    (princ (endless-lanternfish *iter2* *dcl*))
    (princ #\newline)
    
    (princ "0K.")
    (princ #\newline)
    (princ #\newline))

