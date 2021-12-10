;;;;    Advent of Code 2021 - Day 3 task A & B
;;;;    Solutions in Lisp
;;;;    Also solving in other languages like Haskell, ... 
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    The power consumption of the submarine:   841526
;;;;
;;;;    (cl) by Arno Jacobs, 03-12-2021

;;;; First - read the data-set
(defun read-data (&optional (file "data/inputDay03_2021.txt"))
  "Returns the numerical data in FILE as a list."
  (with-open-file (data file)
    (let (*read-eval*)   
      (loop :for     line = (read-line data nil nil)
            :while   line
            :collect line ))))

;;; Some constants
(defvar *DAY3* (read-data))

(defun string-to-list (s)
    (loop for c across s collect c))

(defun rev (list)
  (do ((list list (rest list))
       (reversed '() (list* (first list) reversed)))
      ((endp list) reversed)))

(defun bin-dec (bl)
    (/ (bin-dec-rev (rev bl)) 2))

(defun bin-dec-rev (blst)
    (cond 
        ((null blst) 0 )
        (t (* 2 (+ (car blst) (bin-dec-rev (cdr blst)))))))

(defun check-sum (range check b1 b2)
    (if (>= check range) b1 b2 ))

(defun to-commons (allbits)
    (setq *lln* (length allbits))
    (setq *wln* (- (length (car allbits)) 1 ))
    (setq *gamma*   '())
    (setq *epsilon* '())
    (setq *bs* 0)
    (loop for bitcnt from 0 to *wln* do
        (loop for cw in allbits do
            (if (char= (nth bitcnt (string-to-list cw)) #\1)
                (setf *bs* (+ *bs* 1))))
        (setf *gamma*   (cons (check-sum *lln* (* 2 *bs*) 1 0 ) *gamma*   ))
        (setf *epsilon* (cons (check-sum *lln* (* 2 *bs*) 0 1 ) *epsilon* ))
        (setf *bs* 0)) 
    (setf *gamma*   (cons (rev *gamma*)   '())) 
    (setf *epsilon* (cons (rev *epsilon*) '()))
    (append *gamma* *epsilon*))

(defun power-consumption (allbits)
    (setq *rv* (to-commons allbits))
    (setq *gamma* (car *rv*))
    (setq *epsilon* (car (cdr *rv*)))
    (* (bin-dec *gamma*) (bin-dec *epsilon*)))
    
;;; The main program
(progn 
    (princ "Advent of Code 2021 - day 3 - both parts in Lisp")
    (princ #\newline)
    (princ "The power consumption of the submarine:   ")
    (princ (power-consumption *day3*))
    (princ #\newline)

    (princ "0K.")
    (princ #\newline)
    (princ #\newline)
)

