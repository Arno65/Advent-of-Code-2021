;;;;    Advent of Code 2021 - Day 3 task A & B
;;;;    Solutions in Lisp
;;;;    Also solving in other languages like Haskell, ... 
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    The power consumption of the submarine:   841526
;;;;    The life support rating of the submarine: 4790390
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
    





(defun swap-bits (bits)
    (setq *bl* (string-to-list bits))
    (setq *ln* (- (length *bl*) 1))
    (setq *rv* '())
    (loop for c from 0 to *ln* do
            (if (char= (nth c *bl*) #\1) 
                (setq *rv* (cons 0 *rv*)) 
                (setq *rv* (cons 1 *rv*)))) 
    (format nil "~{~A~}"  *rv*))    ;;; list to string

;;;  select :: Eq a => [[a]] -> [a] -> Int -> [[a]]
;;;  select bl cl bp = filter (\l -> l !! bp == bit) bl
;;;      where bit = cl !! bp
(defun select (allbits cbits bitposition)
    (cons '(1 0 1 0) '())
)

;;;  recToCommons :: [[Int]] -> [Int] -> Int -> Bool -> [Int]
;;;  recToCommons bl cl bp inv   | length sl == 1    = head sl
;;;                              | otherwise         = recToCommons sl ncl (bp+1) inv
;;;      where
;;;          sl  = select bl cl bp
;;;          hcl = toCommons sl 
;;;          ncl = if inv then swapBits hcl else hcl

(defun recursive-to-commons (allbits cbits bitposition inv)
    (setq *sl* (select allbits cbits bitposition))
    (setq *rv* (car *sl*))
    (setq *hcl* (to-commons *sl*))
    (setq *ncl* *hcl*)
    (if inv (setq *ncl* (swap-bits *hcl*)))
    (if (> (length *sl*) 1)
        (recursive-to-commons *sl* *ncl* (+ bitposition 1) inv )) 
    *rv*)


(defun life-support-rating (allbits)
    (setq *rv* (to-commons allbits))
    (setq *gamma* (car *rv*))
    (setq *epsilon* (car (cdr *rv*)))
    (setq *oxygen* (recursive-to-commons allbits *gamma*   0 nil))
    (setq *co-two* (recursive-to-commons allbits *epsilon* 0 t  ))
    (* (bin-dec *oxygen*) (bin-dec *co-two*)))



;;; The main program
(progn 
    (princ "Advent of Code 2021 - day 3 - both parts in Lisp")
    (princ #\newline)
    (princ "The power consumption of the submarine:   ")
    (princ (power-consumption *day3*))
    (princ #\newline)

;;;    (princ (length *day3*))
;;;    (princ #\newline)
;;;    (princ (car *day3*))
;;;    (princ #\newline)
;;;    (princ (swap-bits (car *day3*)))
;;;    (princ #\newline)
;;;    (princ #\newline)

;;;    (princ "The life support rating of the submarine: ")
;;;    (princ (life-support-rating *day3*))
;;;    (princ #\newline)
    (princ "0K.")
    (princ #\newline)
    (princ #\newline)
)

