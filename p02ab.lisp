;;;;    Advent of Code 2021 - Day 2 task A & B
;;;;    Solutions in Lisp
;;;;    Also solving in other languages like Haskell, Rust and Swift
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    With the first set of rules the result if you multiply the final 
;;;;    horizontal position by the final depth is: 1762050
;;;;    With the second set of rules the result if you multiply the final 
;;;;    horizontal position by the final depth is: 1855892637
;;;;
;;;;    (cl) by Arno Jacobs, 02-12-2021

;;;; First - read the data-set
(defun read-data (&optional (file "data/inputDay02_2021.txt"))
  "Returns the numerical data in FILE as a list."
  (with-open-file (data file)
    (let (*read-eval*)   ; set *read-eval* to NIL to avoid malicious code in the input file
      (loop :for line = (read-line data nil nil)
            :while line
;;;         :collect (read-from-string (concatenate 'string line))))))
            :collect (read-from-string (concatenate 'string "(" line ")"))))))

;;; Some constants
(defvar *DAY2* (read-data))

;;; Work list (with the first rules) to find the product of the end position 
(defun trip-one (directions)
    (setq *hp* 0)
    (setq *vp* 0)
    (loop for next-step in directions do
        (setq *direction*   (car next-step))
        (setq *distance*    (car (cdr next-step)))
        (cond   ((eq *direction* 'FORWARD)  (setq *hp* (+ *hp* *distance*)))
                ((eq *direction* 'UP)       (setq *vp* (- *vp* *distance*)))
                ((eq *direction* 'DOWN)     (setq *vp* (+ *vp* *distance*)))))
    (* *hp* *vp*))

;;; Work list (with the new rules) to find the product of the end position 
(defun trip-two (directions)
    (setq *hp* 0)
    (setq *vp* 0)
    (setq *aim* 0)
    (loop for next-step in directions do
        (setq *direction*   (car next-step))
        (setq *distance*    (car (cdr next-step)))
        (cond   ((eq *direction* 'FORWARD)  (setq *hp*  (+ *hp* *distance*))
                                            (setq *vp*  (+ *vp* (* *aim* *distance*))))
                ((eq *direction* 'UP)       (setq *aim* (- *aim* *distance*)))
                ((eq *direction* 'DOWN)     (setq *aim* (+ *aim* *distance*)))))
    (* *hp* *vp*))

;;; The main program
(progn 
    (princ "Advent of Code 2021 - day 2 - both parts in Lisp")
    (princ #\newline)
    (princ "The product of the end position after task one is: ")
    (princ (trip-one *day2*))
    (princ #\newline)
    (princ "The product of the end position after task two is: ")
    (princ (trip-two *day2*))
    (princ #\newline)
    (princ "0K.")
    (princ #\newline)
    (princ #\newline)
)
