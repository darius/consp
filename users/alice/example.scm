; Betting example.  We'll want to make it fancier after we implement
; a real multiuser environment (see loadme.scm now).

(import (use "betting.scm") make-match make-bet-observer)
(import (use "money.scm")   make-money purse-balance)

(define shortsight (use "shortsight.scm"))
(define dc3        ((use "dc3.scm")))

(define alice (make-money 10))
(define bob   (make-money 20))

; Alice bets Bob $5 on shortsight beating dc3 in 10 rounds.
(import (make-match shortsight dc3 10) add-observer! run)
(add-observer! (make-bet-observer 5 alice bob))

; Now we find out how it goes.
(define outcome (run))

(list outcome (purse-balance alice) (purse-balance bob))
