;; Secure funds transfer.

(import (make-sealer 'purse) seal unseal sealed?)

(define purse? sealed?)

;; This facet should be held only by the bank, because it creates money!
(define (make-money amount)
  (if (< amount 0) ; I wonder if there'll be a need for destroying money, too.
      (panic "Negative amount")
      (seal (box amount))))

(define (make-purse)
  (make-money 0))

(define (purse-balance purse)
  (get (unseal purse)))

(define (purse-transfer! amount from to) 
  (let ((from-box (unseal from)) 
	(to-box   (unseal to))) 
    (cond ((< amount 0) (panic "Negative amount"))
	  ((< (get from-box) amount) (panic "Insufficient funds"))
	  (else (put! from-box (- (get from-box) amount)) 
		(put! to-box   (+ (get to-box) amount))) )))

(export purse? make-money make-purse purse-balance purse-transfer!)
