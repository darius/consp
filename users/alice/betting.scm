; Betting on Roshambo matches.
; This stuff isn't really complete, e.g. we don't ensure beforehand
; that the bettor can cover their liability if they lose.
; See TEST-BETTING in test.scm for a more realistic scenario.

(define play (use "umpire.scm"))

(import (use "money.scm") purse-transfer!)
(import (use "stuff.scm") make-one-shot)

(define (make-match player1 player2 rounds)
  (define observers (box '()))
  (define (add-observer! observer)
    (put! observers (cons observer (get observers))))
  (define (run-and-report)
    (let ((results (play player1 player2 rounds)))
      (for-each (lambda (observe) (apply observe results))
                (get observers))
      results))
  (define run (make-one-shot run-and-report))
  (export add-observer! run))

(define (make-bet-observer amount purse1 purse2)
  (lambda (wins1 wins2)
    (cond ((< wins1 wins2) (purse-transfer! amount purse1 purse2))
          ((< wins2 wins1) (purse-transfer! amount purse2 purse1)))))

(export make-match make-bet-observer)
