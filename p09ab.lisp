;;;;    Advent of Code 2021 - Day 9 task A (& not yet... B)
;;;;    Solutions in Lisp
;;;;    Also solving in other languages like Haskell
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;     The sum of the risk levels of all low points on your heightmap is: 591
;;;;     The product of the sizes of the three largest basins is:
;;;;
;;;;    (cl) by Arno Jacobs, 09-12-2021

;;; From nested:    (((1 2) (3 4)) ((9 8) (6 5)))
;;; To:             ( (1 2) (3 4)   (9 8) (6 5) )
(defun one-list (points)
    (if (eq points NIL)
        NIL
        (append (car points) (one-list (cdr points)))))

;;; #\7 -> 7
(defun to-int (c)
    (if (characterp c)
        (parse-integer (string c))))

(defun string-to-list (s)
    (loop for c across s 
        collect (to-int c)))

;;;; First - read the data-set
(defun read-data (&optional (file "data/inputDay09_2021.txt"))
  "Returns the numerical data in FILE as a list."
  (with-open-file (data file)
    (let (*read-eval*)   
      (loop :for     line = (read-line data nil nil)
            :while   line
            :collect (string-to-list line ) ))))

;;; Some constants
(defvar *DAY9* (read-data))
(defvar *border* 9)


;;;; get content of grid(x,y) - but safe
;;;; return *border* if (x,y) is outside the grid
;;;; First the unsafe part
(defun get-height-point (x y grid)
    (nth x (nth y grid)))

;;;; Now with range checking
(defun get-safe-height-point (x y mxx mxy grid)
    (cond   ((< x 0)    *border*)
            ((> x mxx)  *border*)
            ((< y 0)    *border*)
            ((> y mxy)  *border*)
            (T          (get-height-point x y grid))))

;;;; Check: North - East - South - West
(defun check-low-points (x y mxx mxy grid)
    (setq *N* (get-safe-height-point x (- y 1) mxx mxy grid))
    (setq *E* (get-safe-height-point (+ x 1) y mxx mxy grid))
    (setq *S* (get-safe-height-point x (+ y 1) mxx mxy grid))
    (setq *W* (get-safe-height-point (- x 1) y mxx mxy grid))
    (setq *X* (get-safe-height-point x y mxx mxy grid))
    (if (and (< *X* *N*) (< *X* *E*) (< *X* *S*) (< *X* *W*))
        (cons x (cons y '()))))

(defun find-low-points-x (y mxx mxy grid)
    (loop for x from 0 to *maxx* 
        collect (check-low-points x y mxx mxy grid)))

(defun find-low-points-y (grid)
    (setq *maxx* (- (length (car grid)) 1))
    (setq *maxy* (- (length grid) 1))
    (loop for y from 0 to *maxy* 
        collect (remove NIL (find-low-points-x y *maxx* *maxy* grid))))

(defun find-low-points (grid)
    (one-list (remove NIL (find-low-points-y grid))))

(defun calculated-risk-levels (grid) 
    (setq *lpl* (find-low-points grid))
    (loop for xy in *lpl*
        collect (+ 1(get-height-point (car xy) (car (cdr xy)) grid))))

(defun sum-calculated-risk-levels (grid) 
    (reduce '+ (calculated-risk-levels grid)))


(defun product-largest-basins (grid) NIL)

;;; The main program
(progn 
    (princ "Advent of Code 2021 - day 9 - both parts in Lisp")
    (princ #\newline)
    (princ "The sum of the risk levels of all low points on your heightmap is: ")
    (princ (sum-calculated-risk-levels *day9*))
    (princ #\newline)
    (princ "The product of the sizes of the three largest basins is:           ")
    (princ (product-largest-basins *day9*))
    (princ #\newline)
    (princ "0K.")
    (princ #\newline)
    (princ #\newline)
)

