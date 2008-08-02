; A simple Roshambo strategy.

(define (trump move)
  (case move
    ((rock) 'paper)
    ((paper) 'scissors)
    ((scissors) 'rock)
    (else 'rock)))

(define (shortsight opt-his-last-move)
  (if opt-his-last-move
      (trump opt-his-last-move)
      'rock))
