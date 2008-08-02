; Play a Roshambo match, returning how many games each player won.

(define (play-roshambo player1 player2 num-rounds)
  (let loop ((round 0) (prev1 #f) (prev2 #f) (wins1 0) (wins2 0))
    (if (<= num-rounds round)
	(list wins1 wins2)
	(let ((move1 (player1 prev2))
	      (move2 (player2 prev1)))
	  (case (judge move1 move2)
	    ((-1) (loop (+ round 1) move1 move2 wins1 (+ wins2 1)))
	    ((0)  (loop (+ round 1) move1 move2 wins1 wins2))
	    ((1)  (loop (+ round 1) move1 move2 (+ wins1 1) wins2)))))))

(define (judge move1 move2)
  (cond ((beats? move1 move2) 1)
	((beats? move2 move1) -1)
	(else 0)))

(define (beats? move1 move2)
  (or (and (legal? move1) (not (legal? move2)))
      (member (list move1 move2)
	      '((rock scissors) (scissors paper) (paper rock)))))

(define (legal? move)
  (member move '(rock scissors paper)))

play-roshambo
