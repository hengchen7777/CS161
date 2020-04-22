;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
;This function recursively passes each row to the goal-test-row function 
;to test if each individual row meets the goal requirement. The function
;returns t if all rows meets the goal requirement.
(defun goal-test (s)
	(cond ((null s) t)
		(t (and (goal-test-row (first s)) (goal-test (rest s))))
	)
);end defun

;This function takes a row in the state, and returns t if the row
;does not contain any Box or Keeper; otherwise, it returns nil.
(defun goal-test-row (r)
	(cond ((null r) t)
		((atom r) (not (or (isBox r) (isKeeper r))))
		(t (and (goal-test-row (first r)) (goal-test-row (rest r))))
	)
)

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;get-square takes in a state s, a row number r,  a column number c,
;and it returns the integer content of state s at square (r,c). 
;If the square is outside the scope of the problem, the value of a wall is returned.
;
;For get-square, we first determine if (r,c) is outside the scope of the problem,
;if (r,c) is outside of s, we return the value of wall. 
;If (r,c) is within s, we return the integer content of s at (r,c).
(defun get-square (s r c)
	(cond  ((or (>= c (length s)) (>= r (length (first s))) (< c 0) (< r 0)) wall)
		(t (nth r (nth c s)))
	)
)

;set-square takes in a state s, a row number r, a column number c, and a sqare content v (integer).
;The function retuns a new state that is obtained by setting the square (r, c) to v
;
;set-square recursively appends the first element of s to the set-square of the rest of s with c decremented
;by 1. If we are on the row with column nunber c, we send the first element of s to set-square-column.
(defun set-square (s r c v)
	(cond ((null s) nil)
		((= c 0) (cons (set-square-column (car s) r v) (set-square (cdr s) r (- c 1) v)))
		(t (cons (car s) (set-square (cdr s) r (- c 1) v)))
	)
)

;set-square-column is a helper funtion for set-square. It takes in a row s, a row number r,
;and an integer v. the funciton returns a new row that is obtained by setting the element at
;row number r to v.
;
;set-square-column recursively appends the first element of the row to the set-squre-column of the 
;rest of the row s with r decremented by 1. If we found the position we want to change, we append v 
;to the rest of the result.
(defun set-square-column (s r v)
	(cond ((null s) nil)
		((= r 0) (cons v (set-square-column (cdr s) (- r 1) v)))
		(t (cons (car s) (set-square-column (cdr s) (- r 1) v)))
	)
)

;try-move takes in a state s, a direction d, the position of the keeper (x,y), and 
;it returns the state that is the result of moving the keeper in state s in direction d.
;
;Depending on the direction we are moving, we pass in the current position that the keeper
;is standing at (x,y), the next step in direction d, and the next next step
;in direction d into the move function, which actually does the moving.
(defun try-move (s d x y)
	(cond   ((equal d `UP)    (move s x y x (- y 1) x (- y 2)))
		((equal d `DOWN)  (move s x y x (+ y 1) x (+ y 2)))
		((equal d `LEFT)  (move s x y (- x 1) y (- x 2) y))
		((equal d `RIGHT) (move s x y (+ x 1) y (+ x 2) y))
	)
)

;move takes in a state s, the position of the keeper (ax, ay), the next step the keeper can take (bx, by),
;and the next next step in the moving direction of the keeper (cx, cy), and it returns the state that is the 
;result of moving the keeper in s at (ax, ay) towards the (bx, by) (cx, cy) direction.
;
;First, we set a to the value of square (ax, ay), b to the value of square (bx, by), c to the value
;of square (cx, cy). Then, we use cond to discuss all possible combinations of what is a, b, c. 
;If b is a wall, we return nil. If b is a box or boxstar, and c is a wall, a box, or a boxstar, we return nil.
;In the cases where the keeper can make a valid move:
;1. If the keeper is moving in the direction of a blank or goal:
;We set the new state based on whether a is a keeper or a keeperstar, and whether b is a blank or star.
;2. If the keeper is pushing a box:
;We set the new state based on whether a is a keeper/keeperstar, whether b is a box/boxstar, and whether c is a blank/star. 
(defun move (s ax ay bx by cx cy)
	(let ((a (get-square s ax ay)) (b (get-square s bx by)) (c (get-square s cx cy)))
		(cond   ((isWall b) nil)
			((and (or (isBox b) (isBoxstar b)) (or (isWall c) (isBox c) (isBoxStar c))) nil)
			((and (isBlank b) (isKeeper a)) (set-square (set-square s ax ay blank) bx by keeper))
			((and (isStar b) (isKeeper a)) (set-square (set-square s ax ay blank) bx by keeperstar))
			((and (isBlank b) (isKeeperStar a)) (set-square (set-square s ax ay star) bx by keeper))
			((and (isStar b) (isKeeperStar a)) (set-square (set-square s ax ay star) bx by keeperstar))
			((and (isBox b) (isBlank c) (isKeeper a)) (set-square (set-square (set-square s ax ay blank) bx by keeper) cx cy box))
			((and (isBox b) (isStar c) (isKeeper a)) (set-square (set-square (set-square s ax ay blank) bx by keeper) cx cy boxstar))
			((and (isBoxStar b) (isBlank c) (isKeeper a)) (set-square (set-square (set-square s ax ay blank) bx by keeperstar) cx cy box))
			((and (isBoxStar b) (isStar c) (isKeeper a)) (set-square (set-square (set-square s ax ay blank) bx by keeperstar) cx cy boxstar))
			((and (isBox b) (isBlank c) (isKeeperStar a)) (set-square (set-square (set-square s ax ay star) bx by keeper) cx cy box))
			((and (isBox b) (isStar c) (isKeeperStar a)) (set-square (set-square (set-square s ax ay star) bx by keeper) cx cy boxstar))
			((and (isBoxStar b) (isBlank c) (isKeeperStar a)) (set-square (set-square (set-square s ax ay star) bx by keeperstar) cx cy box))
			((and (isBoxStar b) (isStar c) (isKeeperStar a)) (set-square (set-square (set-square s ax ay star) bx by keeperstar) cx cy boxstar))
		)
	)
)

;next-states takes in a state s and returns the list of all states that can be reached from s in one move.
;
;First, we get the position of the keeper (x, y), and then call try-move in all 4 directions and pass in x, y.
;We append the 4 return values of try-move into a list called result, 
;and use cleanUpList to get rid of the nil's in result.
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s `UP x y) (try-move s `DOWN x y) (try-move s `LEFT x y) (try-move s `RIGHT x y)))
	 )
    (cleanUpList result);end
  );end let
);

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
;h0 returns the constant 0.
(defun h0 (s)
	0
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
;h1 is an admissible heuristic, because
;the number of misplaced boxes in s <= the number of steps to reach the goal,
;so h1 will never overestimate the actual cost to reach the goal state.
;
;In the base case, if s is empty, return 0. 
;If s is an atom that is a box not on goal position, return 1.
;If s is an atom in other cases, return 0.
;If s is a list, recursively call h1 on the first element of s
;and the rest of the list s to sum them up.
(defun h1 (s)
	(cond ((null s) 0)
		((and (atom s) (isBox s)) 1)
		((atom s) 0)
		(t (+ (h1 (first s)) (h1 (rest s))))
	)  
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
;h504923897 takes in a state s, and returns an integer.
;If there are still boxes not in goal, the function returns 
;the sum of the minimum manhattan distance from each box to a goal
;and the minimum manhattan distance from the keeper to the closest box.
;If all boxes are in goal, the function returns the minimum
;manhattan distance from the keeper to a goal.
;h504923897 is an admissible heruistic, 
;because it never overestimates the actual cost to reach the goal state.
;
;We first call getKeeperPosition to get the coordinate (x, y) of the keeper,
;and call getlist to get the list of coordinates of boxes and goals.
;Then, if boxes are empty, we call min-manhattan to get the min manhattan 
;distance from the keeper to a goal.
;Otherwise, we call sum-manhattan to get the sum of min manhattan distance
;from each box in boxes to a goal, and we sum the output up with the min
;manhattan distance from the keeper to the closest box.
(defun h504923897 (s)
	(let* ((pos (getKeeperPosition s 0)) 
		(x (car pos))
		(y (cadr pos))
		(boxes (getlist s 0 box)) 
		(goals (getlist s 0 star))
		)
		(cond ((null boxes) (min-manhattan x y goals))
			(t (+ (sum-manhattan boxes goals) (min-manhattan x y boxes)))
		)
	)
)

;sum-manhattan takes a list of coordinates of boxes, and a list of coordinates of goals.
;It returns tbe sum of min-manhattan distance from each box in boxes to goals.
;
;In the base case, if boxes is empty, we return 0.
;Otherwise, we recursively pass the first box in boxes to min-manhattan, and 
;sum the output with sum-mahattan of the rest of boxes.
(defun sum-manhattan (boxes goals)
	(cond ((null boxes) 0)
		(t (+ (min-manhattan (first (first boxes)) (second (first boxes)) goals) (sum-manhattan (rest boxes) goals)))
	)
)

;min-manhattan takes in the coordinate (x, y) of a suqare, and the list of corrdinates of the target itmes, goals.
;It returns the minimum manhattan distance from (x, y) to any of the items in goals.
;
;In the base case: if goals is empty, we return 0
;if goals has one last item, we return the manhattan distance from (x, y) to the last item.
;Otherwise, we recusively return the min of the manhattan distance of the first item in goals and 
;the min-manhattan of the rest of goals.
(defun min-manhattan (x y goals)
	(cond ((null goals) 0)
		((null (rest goals)) (min (+ (abs(- x (first(first goals)))) (abs(- y (second(first goals))))) MOST-POSITIVE-SHORT-FLOAT))
		(t (min (+ (abs(- x (first(first goals)))) (abs(- y (second(first goals))))) (min-manhattan x y (rest goals))))
	)
)

;getlist takes in a state s, a column number c, and the item we're looking for x. It 
;returns a list of coordinates for x in s.
;
;In the base case, if s is empty, we return nil.
;Otherwise, we recursively call the helper function getlistColumn on the current row,
;and append the answer to the rest of the result.
(defun getlist (s c x)
	(cond ((null s) nil)
		(t (append (getlistColumn (first s) 0 c x) (getlist (rest s) (+ c 1) x)))
	)
)

;getlistColumn takes in a row of the state l, current row number r, current column number c,
;and the item we're looking for x. It returns a list of coordinates for x in the row c.
;
;In the base case, if l is empty, we return nil.
;if the first element of l is x, we append its coordinate to the rest of the result.
;Otherwise, we recursively call getlistColumn on the rest of l.
(defun getlistColumn (l r c x)
	(cond ((null l) nil)
		((= (first l) x) (cons (list r c) (getlistColumn (rest l) (+ r 1) c x)))
		(t (getlistColumn (rest l) (+ r 1) c x))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
