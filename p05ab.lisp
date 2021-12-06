;;;;    Advent of Code 2021 - Day 5 task A & B
;;;;    Solutions in Lisp
;;;;    Also solving in other languages like Haskell
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    Considering only horizontal and vertical lines, 
;;;;    the number of points where at least two lines overlap is: 6007
;;;;    Considering horizontal, vertical and only diagonal lines at 45 degrees, 
;;;;    the number of points where at least two lines overlap is: 19349
;;;;
;;;;
;;;;    (cl) by Arno Jacobs, 05-12-2021


;;; Test if character is element of [0..9]
(defun is-digit (c)
    (if (eq c NIL)
        NIL
        (and (char>= c #\0) (char<= c #\9))))

;;; Return the location of the first digit - starting from 'pc' 
(defun first-digit (sl pc)
    (if (is-digit (car sl))
        pc
        (first-digit (cdr sl) (+ pc 1))))

;;; Return the location of the last digit - starting from 'pc' 
(defun last-digit (sl pc)
    (if (is-digit (car sl))
        (last-digit (cdr sl) (+ pc 1))
        pc))

;;; '(#\A #\B #\C #\1 #\2 #\3 #\4 #\.) -> 1234
(defun number-from-list (sl)
    (setq *pfd* (first-digit sl 0))
    (setq *pld* (last-digit sl *pfd*))
    (cons (subseq sl *pfd* *pld*) (subseq sl *pld*)))

;;; '(#\A #\9 #\C #\1 #\X #\3 #\4 #\.) -> '(9 1 34)
(defun numbers-to-list (sl)
    (setq *nlrsl* (number-from-list sl))
    (setq *nn* (car *nlrsl*))
    (setq *rl* (cdr *nlrsl*))
    (if (eq *rl* NIL) 
        (cons *nn* '())
        (cons *nn* (numbers-to-list *rl*))))

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

;;; '((2 4) (8 5) (9 3)) -> '(24 85 93)
(defun convert-list (cl)
    (loop for sl in cl do
        (setq *cv* (digits-to-number (mapcar #'to-int sl)))
        collect *cv*))

;;; For this task
;;; "7,4 -> 23,42\n1,2 -> 3,4" -> '((7 4 23 42) (1 2 3 4))
;;; But will also do:
;;; "Forward 2, down 7\nForward 33, up 9" -> '((2 7) (33 9))
;;;
(defun numbers-from-string-to-list (str) 
    (convert-list (remove NIL (numbers-to-list (coerce str 'list)))))

;;; Read data and convert to numbers list in sublist of size 4
;;;
;;; Like: ((259 377 259 599) (120 758 977 758) (2 4 10 12))
;;;
;;; The first being a vertical line from (259,377) to (259,599)
;;; The second being a horizontal line from (120,758) to (977,758)
;;; The third being a 45 degrees diagonal line from (2,4) to (10,12)
(defun read-data (&optional (file "data/inputDay05_2021.txt"))
  "Returns the numerical data in FILE as a list."
  (with-open-file (data file)
    (let (*read-eval*)   
        (loop   :for        line = (read-line data nil nil)
                :while      line
                :collect    (numbers-from-string-to-list line) ))))


;;; Some constants
(defvar *DAY5* (read-data))


;;; The horizontal lines
(defun draw-hline (line)
    (setq *x1* (car line))
    (setq *x2* (car (cdr (cdr line))))
    (setq *y*  (car (cdr line)))
    (loop for x from (min *x1* *x2*) to (max *x1* *x2*) do
        (setq *le* (cons x (cons *y* '())))
        collect *le*))

(defun draw-if-horizontal (lines)
    (loop for line in lines do
        (if (= (car (cdr line)) (car (last line)))
            (setq *rl* (draw-hline line))
            (setq *rl* NIL))
        collect *rl*))
            
(defun draw-horizontal (lines)
    (remove NIL (draw-if-horizontal lines)))

;;; The vertical lines
(defun draw-vline (line)
    (setq *y1* (car (cdr line)))
    (setq *y2* (car (last line)))
    (setq *x*  (car line))
    (loop for y from (min *y1* *y2*) to (max *y1* *y2*) do
        (setq *le* (cons *x* (cons y '())))
        collect *le*))

(defun draw-if-vertical (lines)
    (loop for line in lines do
        (if (= (car line) (car (cdr ( cdr line))))
            (setq *rl* (draw-vline line))
            (setq *rl* NIL))
        collect *rl*))
            
(defun draw-vertical (lines)
    (remove NIL (draw-if-vertical lines)))

(defun flatten1 (xs)
  (if (eq xs NIL)
        NIL
        (append (car xs) (flatten1 (cdr xs)))))

(defun flatten-int (xs)
    (loop for x in xs do
        (setq *x1* (car x))
        collect *x1*))


;;; '(12 34) -> '(12034)
(defun one-value (xy)
    (cons (+ (* 1000 (car xy)) (car (cdr xy))) '()))

(defun is-double (x1 x2)
    (if (= x1 x2)
        x1))

(defun select-all-doubles (xs)
    (setq *prev* 0)
    (loop for x in xs do
        (setq *x1* (is-double *prev* x)) 
        (setq *prev* x)
        collect *x1*))

(defun select-doubles (sl)
    (remove NIL (select-all-doubles sl)))

(defun is-single (x1 x2)
    (if (/= x1 x2)
        x1))

(defun select-all-singles (xs)
    (setq *prev* 0)
    (loop for x in xs do
        (setq *x1* (is-single *prev* x)) 
        (setq *prev* x)
        collect *x1*))

(defun select-singles (sl) 
    (remove NIL (select-all-singles sl)))

(defun overlap_horz_vert (lines) 
    (setq *hl* (flatten1 (draw-horizontal lines)))
    (setq *vl* (flatten1 (draw-vertical   lines)))
    (setq *al* (append *hl* *vl*))
    (setq *nc* (flatten-int (mapcar #'one-value *al*)))
    (setq *sl* (sort *nc* #'<))
    (length (select-singles (select-doubles *sl*))))

(defun overlap_horz_vert_d45 (lines) -19349)

;;; The main program
(progn 
    (princ "Advent of Code 2021 - day 5 - both parts in Lisp")
    (princ #\newline)

    (princ "Considering only horizontal and vertical lines,")
    (princ #\newline)
    (princ "the number of points where at least two lines overlap is: ")
    (princ (overlap_horz_vert *day5*))
    (princ #\newline)

;;;    (princ "Considering horizontal, vertical and only diagonal lines at 45 degrees,") 
;;;    (princ #\newline)
;;;    (princ "the number of points where at least two lines overlap is: ")
;;;    (princ (overlap_horz_vert_d45 *day5*))
;;;    (princ #\newline)
    
    (princ "0K.")
    (princ #\newline)
    (princ #\newline))



