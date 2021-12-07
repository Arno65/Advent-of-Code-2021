;;;;    Advent of Code 2021 - Day 7 task A & B
;;;;    Solutions in Lisp
;;;;    Also solving in other languages like Haskell
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    Determine the horizontal position that the crabs can align to using 
;;;;    the least fuel possible.
;;;;    The amount of fuel they spend to align to that position is: 349769
;;;;    Now with the correct fuel consumption, 
;;;;    as each crab moves, moving further becomes more expensive. 
;;;;    The amount of fuel they spend to align to that position is: 99540554
;;;;    
;;;;
;;;;    (cl) by Arno Jacobs, 07-12-2021

;;;; 

;;; '(1 2 3) -> 123
(defun digits-to-number (nl)
    (setq *lln* (length nl))
    (setq *lwe* (subseq nl 0 (- *lln* 1)))
    (if (< *lln* 2)
        (car nl)
        (+ (car (last nl)) (* 10 (digits-to-number *lwe*)))))

;;; #\7 -> 7
(defun to-int (c)
    (if (characterp c)
        (parse-integer (string c))))

;;; dropWhile (/= SC) + one position
(defun nextSC (sc cl)
    (if (eq cl NIL)
        '()
        (if (char= (car cl) sc)
            (cdr cl)
            (nextSC sc (cdr cl)))))

;;; takeWhile (/= SC)
(defun splitOnPart (sc cl)
    (if (eq cl NIL)
        '()
        (if (char= (car cl) sc)
            '()
            (cons (to-int (car cl)) (splitOnPart sc (cdr cl))))))

;;; Get digits till SC and convert to integer number
;;; '(#\1 #\, #\2 #\, #\3 #\, #\4 #\, #\6, #\7, #\8) -> (1 2 3 4 678)
;;; with sc -> #\,
(defun splitOn (sc cl)
    (if (eq cl NIL)
        '()
        (cons (digits-to-number (splitOnPart sc cl)) (splitOn sc (nextSC sc cl)))))
        
;;; For this task
;;; "1,2,3,4,5" -> '(#\1 #\, #\2 #\, #\3 #\, #\4 #\, #\5)
;;;
(defun string-to-char-list (str)
    (coerce str 'list))

;;; Read data and convert to numbers list
(defun read-data (&optional (file "data/inputDay07_2021.txt"))
  "Returns the numerical data in FILE as a list."
  (with-open-file (data file)
    (let (*read-eval*)   
        (loop   :for        line = (read-line data nil nil)
                :while      line
                :collect    (splitOn #\, (string-to-char-list line)) ))))

;;; The input data converted to a list of digits
(defvar *DAY7* (car (read-data)))

;;; Return a 1 if the first element of 'el' is equal to 'e' 
;;; otherwise return a 0
(defun plus1 (e el)
    (if (= e (car el))
        1
        0))

;;; Count the number if digits 'e' from a list 'el'
(defun count-number (e el)
    (if (eq el NIL)
        0
        (+ (plus1 e el) (count e (cdr el)))))
        
;;; Count all the digits [0..maximum Crab position] from list 'cpl'
;;; The 'cpl' is the input list for this days tasks
(defun tally (cpl)
    (setq *mxp* (reduce #'max cpl))
    (loop for d from 0 to *mxp* do 
        (setq *dc* (count-number d cpl))
        collect *dc*))

;;; Calculate the sum of a list of numbers
(defun sum-list (lst) 
    (reduce '+ lst))

;;; Simple delta function, the (positive) difference between to numbers
(defun delta (a b)
    (abs (- a b)))

;;; Given a start point (sp) calculate the spend fuel -- rules part 1
(defun calculate-fuel-1 (sp cpl)
    (setq *sd* 0)
    (loop for ix from 0 to (- (length cpl) 1) do 
        (setq *pc* (nth ix cpl))
        (setq *d*  (delta ix sp))
        (setq *sd* (+ (* *pc* *d*) *sd*))) *sd*)

;;; Given a start point (sp) calculate the spend fuel -- rules part 2
(defun calculate-fuel-2 (sp cpl)
    (setq *sd* 0)
    (loop for ix from 0 to (- (length cpl) 1) do 
        (setq *pc* (nth ix cpl))
        (setq *d*  (delta ix sp))
        (setq *di* (/ (* *d* (+ *d* 1)) 2))
        (setq *sd* (+ (* *pc* *di*) *sd*))) *sd*)

;;; Calculate fuel consumption for all places on the horizontal plane
(defun calculate-fuel-all (part cpl)
    (loop for sp from 0 to (- (length cpl) 1) do
        (if (= part 1)
            (setq *cf* (calculate-fuel-1 sp cpl))
            (setq *cf* (calculate-fuel-2 sp cpl)))
        collect *cf*))

;;; Get the minimum value of fuel consumption
(defun minimum-fuel (part cpl)
    (reduce #'min (calculate-fuel-all part cpl))
)

;;; The main program
(progn 
    (princ "Advent of Code 2021 - day 7 - both parts in Lisp")
    (princ #\newline)
    (setq *cpl* (tally *day7*))         ;;; get Crabs position list
    (princ "The minimum amount of fuel spend at part 1 is: ")
    (princ (minimum-fuel 1 *cpl*))      ;;; part 1
    (princ #\newline)    
    (princ "The minimum amount of fuel spend at part 1 is: ")
    (princ (minimum-fuel 2 *cpl*))      ;;; part 2
    (princ #\newline)
    (princ "0K.")
    (princ #\newline)
    (princ #\newline))

