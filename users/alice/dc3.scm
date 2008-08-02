; A slightly less simple Roshambo strategy.
; This player uses the previous move (of both players) to predict the
; opponent's next move, then trump it.  We predict that he'll play the
; same as he did the last time we saw the same move-pair.

(define (make-dc3)
    
  (define unprintable '((paper 0) (scissors 1) (rock 2)))
  (define printable   '((0 paper) (1 scissors) (2 rock)))

  (define (convert table x)
    (cadr (assoc x table)))

  (define prediction (vector (box 0) (box 1) (box 2)
			     (box 0) (box 1) (box 2)
			     (box 0) (box 1) (box 2)))

  (define (predict mine his)
    (vector-ref prediction (+ (* 3 his) mine)))

  (define (trump move)
    (modulo (+ move 1) 3))

  (define (move mine2 his2 mine1 his1)
    (put! (predict mine2 his2) his1)
    (trump (get (predict mine1 his1))))

  (define mine2 (box 0))
  (define his2  (box 0))
  (define mine1 (box 0))

  (define (answer his1)
    (let ((m2 (get mine2)) (h2 (get his2)) (m1 (get mine1)))
      (let ((result (move m2 h2 m1 his1)))
	(put! mine2 m1)
	(put! his2  his1)
	(put! mine1 result)
	result)))

  (lambda (his-last-move)
    (convert printable 
	     (answer (convert unprintable (or his-last-move 'paper))))))
