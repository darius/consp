(define play (use "umpire.scm"))
(define shortsight (use "shortsight.scm"))
(define make-dc3 (use "dc3.scm"))

(define (yay)
  (define dc3 (make-dc3))
  (play shortsight dc3 20))
