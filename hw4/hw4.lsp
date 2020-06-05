;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta)
	(backtrack-search n delta `())
)

;backtrack-search is a helper function for sat?.
;It returns nil if CNF is not satisfiable,
;otherwise, it returns a list of possible assignments to CNF.
;param n: number of variables in CNF
;param CNF: a CNF represented as a list of lists
;param model: a list of assignment of values to variables
;
;backtrack-search first checks if the current model is a 
;possible assignment for CNF using the check-CNF function. 
;If the current model is not possible, we return nil.
;If the current model is possible, we further check if n has reached 0.
;If n = 0, that means we get a complete model that satisfies CNF, so
;we return model. If n is not 0, we recursively do backtrack-search on n,
;the next variable to assign, and decrement n by 1. We assign n with all possible 
;values n and -n, then we or the two cases together, because either model might satrisfy CNF.
(defun backtrack-search (n CNF model)
	(cond ((check-CNF CNF model)
		(cond ((= n 0) model)
			(t (or (backtrack-search (- n 1) CNF (append (list n) model))
				(backtrack-search (- n 1) CNF (append (list (- 0 n)) model))))
		))
		(t nil)
	)
)

;check-CNF takes in a list of CNF and a list of model,
;and it returns nil if one of its clauses evaluates to nil,
;otherwise it returns true.
(defun check-CNF (CNF model)
	(cond ((null CNF) t)
		(t (and (check-clause (first CNF) model) (check-CNF (rest CNF) model)))
	)
)

; check-clause takes in a list of clause and a list of model,
; and it returns nil if all elements in clause are assigned by model,
; and the clause evaluates to false in that case.
; Otherwise, it returns true.
(defun check-clause (clause model)
	(cond ((null clause) nil)
		(t (or (check-literal (first clause) model) (check-clause (rest clause) model)))
	)
)

; check-literal takes in a signed integer literal and a list of model,
; and it returns nil if literal is the opposite of one of the elements in model,
; otherwise, it returns t
(defun check-literal (literal model)
	(cond ((null model) t)
		((atom model) (not (= (+ literal model) 0)))
		((atom model) t)
		(t (and (check-literal literal (first model)) (check-literal literal (rest model))))
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

