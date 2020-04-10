
; #1 TREE-CONTAINS 
; Arguments: N -> a number, TREE -> an ordered tree
; Output: returns T if N appears in TREE; otherwise, the function returns NIL. 
(defun TREE-CONTAINS (N TREE) 
	(cond((not TREE) nil)  ;base case when tree is empty
		((atom TREE) (equal N TREE))  ;base case when tree has only one item
		(t (or(TREE-CONTAINS N (car TREE))(TREE-CONTAINS N (cdr TREE))))
;recursively check if the first element or the rest of the tree contains N 
	)
)

; #2 TREE-MIN
; Argument: TREE -> an ordered tree
; Output: returns the minimum number in TREE
(defun TREE-MIN (TREE)
	(if(atom TREE) ;base case: return TREE when it has one element
		TREE
		(TREE-MIN (car TREE)) ; recusively do TREE-MIN on the fisrt element of TREE
	)
)

; #3 TREE-ORDER
; Argument: TREE -> an ordered tree
; Output: returns an pre-ordered list of the numbers in TREE
(defun TREE-ORDER (TREE)
	(cond ((atom TREE) (cons TREE nil)) ;base case: when TREE has one element, return a list of that element
		(t (append (TREE-ORDER(second TREE)) (TREE-ORDER(first TREE)) (TREE-ORDER(third TREE)) ))
	) ; recusively append m L R together for a tree of the form (L m R)
)

; #4 SUB-LIST
; Arguments: L -> a list, START -> a non-negative integer, LEN -> a non-negative integer
; Output: returns the sub-list of L starting at position START and having length LEN
(defun SUB-LIST (L START LEN) 
	(cond ((= LEN 0) nil)
		((not(= START 0)) (SUB-LIST (cdr L) (- START 1) LEN)) ;recursively get the rest of the list until START decrements to 0	
		(t (cons (car L) (SUB-LIST (cdr L) START (- LEN 1)))) ;recursively append the rest of the sublist to the first element of the sublist
	)
)

; #5 SPLIT-LIST
; Argument: L -> a list
; Output: a list of two lists L1 and L2 in that order, such that
; L is the result of appending L1 and L2, and length(L1) - length(L2) = 0 or 1
(defun SPLIT-LIST (L)
	(let ((list-len (Length L)))  ;set the value of list-len to the length of L
		(cond ((evenp list-len)
			(list (SUB-LIST L 0 (/ list-len 2)) (SUB-LIST L (/ list-len 2) (/ list-len 2))))
;if list-len is even, L1 and L2 would both have length list-len/2
			(t (list (SUB-LIST L 0 (/ (+ list-len 1) 2)) (SUB-LIST L (/ (+ list-len 1) 2) (- (/ (+ list-len 1) 2) 1))))
;if list-len is odd, L1 has length (list-len+1)/2, and L2 has length ((list-len+1)/2)-1
		)
	)
)

; #6 BTREE-HEIGHT
; Arguments: TREE -> a binary tree
; Output: returns the height of TREE
(defun BTREE-HEIGHT (TREE)
	(cond ((atom TREE) 0)  ;bese case: when leaf node height is 0
		(t (let ((l-height (BTREE-HEIGHT(first TREE))) ;get the height of the left child
		(r-height (BTREE-HEIGHT(second TREE)))) ; get the height of the right child
			(cond ((> l-height r-height) (+ l-height 1)) ; height of the current node is the max height of its children + 1
				(t (+ r-height 1))
			)
		))
	)
)

; #7 LIST2BTREE
; Argument: LEAVES -> a non-empty list of atoms
; Output: returns a binary tree, such that
; the leaves are the elements of LEAVES
; for any internal node in tree, the number of leaves in its left branch minus the number of leaves in its right branch is 0 or 1
(defun LIST2BTREE (LEAVES)
	(cond ((null (rest LEAVES)) (first LEAVES)) ;base case: if LEAVES has only one element, return the element
		((null (third LEAVES)) LEAVES) ;base case: if LEAVES only has two elements, return the list of two elements
		((= (length LEAVES) 3) (list (list (first LEAVES) (second LEAVES)) (third LEAVES))) ;base case: return the binary tree when LEAVES has three elements
		((= (length LEAVES) 4) (SPLIT-LIST LEAVES)) ;base case: use the SPLIT-LIST function on LEAVES with 4 elements
		(t (let ((SPLIT-L (SPLIT-LIST LEAVES))) 
			(list (LIST2BTREE (first SPLIT-L)) (LIST2BTREE (second SPLIT-L)))) ;when LEAVES has more than 4 elements, do SPLIT-LIST on LEAVES and recursively append the binary trees of the two sublists together 

		)
	)
)

; #8 BTREE2LIST
; Argument: TREE  -> a binary tree
; Output: returns a list of atoms that represents the leaves of TREE
(defun BTREE2LIST (TREE)
	(cond ((atom TREE) (list TREE)) ;base case: when TREE has one element, return a list of the element
		(t (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE)))) ;recursively call BTREE2LIST on the two children of TREE and append them together
	)
)

; #9 IS-SAME
; Arguments: E1 -> a LISP expression, E2 -> a LISP expression (a LISP expression is either an atom or a list of LISP expressions)
; Output: return T if E1 and E2 are equal; otherwise, return NIL
(defun IS-SAME (E1 E2)
	(cond 	((and (not  E1) (not E2)) T) ;if E1 and E2 are both NIL, they are equal
		((and (not E1) E2) NIL)
		((and (not E2) E1) NIL) ;if either E1 or E2 is NIL, return NIL
		((and (atom E1) (not (atom E2))) NIL)
		((and (atom E2) (not (atom E1))) NIL) ;if  either E1 or E2 is  not an atom, return NIL
		((and (atom E1) (atom E2)) (= E1 E2)) ;if E1 and E2 are both atoms, compare them
		(t (and (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2)))) ;recursively check if the first element and the rest of E1 and E2 are equal, E1 and E1 are equal only if both parts are equal
	)
)
