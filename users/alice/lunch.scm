;; Voting on where to go out for lunch.
;; http://www.cap-lore.com/CapTheory/Language/Lunch.html

(define (bill options)
  (vote-for "Casa Lupe" options)
  (vote-for "Mandarin" options))

(define (bob options)
  (vote-for "Mandarin" options))

(define lunch-crowd (list bill bob))
(define restaurants '("Mandarin" "Casa Lupe" "Coffee Shop"))

(define (vote-for restaurant options)
  ((cdr (assoc restaurant options))))

(define (start-voting)

  (define ballot-box
    (map (lambda (r) (box 0)) restaurants))

  (define (poll! voter)
    (voter (map (lambda (r b) 
		  (cons r (make-one-shot (lambda () (inc! b 1)))))
		restaurants
		ballot-box)))

  (define (tally-votes)
    (map (lambda (r b) (list r (get b)))
	 restaurants
	 ballot-box))

  (for-each poll! lunch-crowd)
  tally-votes)

(import (use "stuff.scm") make-one-shot inc!)

start-voting
