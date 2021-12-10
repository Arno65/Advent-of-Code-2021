;;;;    Advent of Code 2021 - Day 10 task A & B
;;;;    Solutions in Lisp
;;;;    Also solving in other languages like Haskell
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    The total syntax error score for corrupt strings is:     464991
;;;;    The middle syntax error score for incomplete strings is: 3662008566
;;;;
;;;;    (cl) by Arno Jacobs, 10-12-2021

;;; Tokens list
(defvar *ocs* (coerce ">}])0([{<" 'list))

(defun get-index (s1)
    (- (get-index-strs s1 *ocs* 0) 4))

(defun get-index-strs (s1 ts ix)
    (if (eq ts NIL)
        10003                ;;; 10003 = 9999 + 4
        (if (eq (car ts) s1)
            ix
            (get-index-strs s1 (cdr ts) (+ ix 1)))))

(defun index-line (line)    
        (loop for c across line 
            collect (get-index c)))

(defun index-all-lines (lines)
    (loop for line in lines
        collect (index-line line)))

;;;; First - read the data-set
(defun read-data (&optional (file "data/inputDay10_2021.txt"))
  "Returns the numerical data in FILE as a list."
  (with-open-file (data file)
    (let (*read-eval*)   
      (loop :for     line = (read-line data nil nil)
            :while   line
            :collect line) )))

(defvar *DAY10* (index-all-lines (read-data)))

(defvar *points1* '(3 57 1197 25137))
(defvar *points2* '(1 2 3 4)) ;; Not used - only a reminder
(defvar *factor2*   5)
(defvar *corrupt*   333)

;;; Parsing tokens -- for part 1 and 2
;;;
;;; (parse-list '(1 2 3 -3 -1 -2) '())
;;; parts - B - A - main
(defun parse-list-B (delimiters checklist)
    (if (= 0 (+ (car checklist) (car delimiters)))
        (parse-list (cdr delimiters) (cdr checklist))
        (cons *corrupt* (cons (* (car delimiters) -1) checklist))))
                
(defun parse-list-A (delimiters checklist)
    (setq *da* (car delimiters))
    (if (> *da* 0)
        (parse-list (cdr delimiters) (cons *da* checklist))
        (parse-list-B delimiters checklist)))

(defun parse-list (delimiters checklist)
    (if (eq delimiters NIL)
        checklist
        (parse-list-A delimiters checklist)))

;;; Part 1
;;; get score for 'corrupt' list
(defun get-score-corrupt (ix)
    (nth (- ix 1) *points1*))

;;;; Parsing line - if 'corrupt' get score
(defun score-corrupted-line (l)
    (setq *rv* (parse-list l '()))
    (if (= (car *rv*) *corrupt*)
        (get-score-corrupt (car (cdr *rv*)))
        0 ))

;;; Line by line...    
(defun score-per-corrupted-line (lines)
    (loop for l in lines 
        collect (score-corrupted-line l)))

;;; The end score is the sum of all scores per line    
(defun score-corrupted-lines (lines)
    (setq *scl* (score-per-corrupted-line lines))
    (reduce '+ *scl*))

;;; Part 2
;;;
;;; progressive score...
(defun get-score-incomplete (ix sc)
    (setq *hrv* (* *factor2* sc))
    (if (eq ix NIL)
        sc
        (get-score-incomplete (cdr ix) (+ (car ix) *hrv*))))
        
;;; Get 'incomplete' score for given line        
(defun score-incomplete-line (l)
    (setq *rv* (parse-list l '()))
    (if (/= (car *rv*) *corrupt*)  
        (get-score-incomplete *rv* 0)
        0 ))
        
;;; Work line by line        
(defun score-per-incomplete-line (lines)
    (loop for l in lines 
        collect (score-incomplete-line l)))
    
(defun score-incomplete-lines (lines)
    ;;; Get all 'incomplete' scores
    (setq *scl*   (score-per-incomplete-line lines))
    ;;; remove all zeros and sort the list 
    (setq *sscli* (sort (remove 0 *scl*) #'<))
    ;;; Get the (floor) half of the (always for this task odd) list lenght
    (setq *hll*   (floor (length *sscli*) 2))
    ;;; Pick the middle value of the sorted score list
    (nth *hll* *sscli*))

;;; The main program
(progn 
    (princ "Advent of Code 2021 - day 10 - both parts in Lisp")
    (princ #\newline)
    (princ "The total syntax error score for corrupt strings is:     ")
    (princ (score-corrupted-lines *day10*))
    (princ #\newline)
    (princ "The middle syntax error score for incomplete strings is: ")
    (princ (score-incomplete-lines *day10*))
    (princ #\newline)
    (princ "0K.")
    (princ #\newline)
    (princ #\newline)
)



