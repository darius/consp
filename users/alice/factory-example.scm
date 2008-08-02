; Alice has a program P whose source code she doesn't want to give out:
(define p '(lambda (x) (+ x x)))

; Bob has data D he wants to run P on, without revealing D to Alice.
(define d 21)

; So Alice gives Bob a p-factory:
(define p-factory (make-factory p))

; Bob receives it and verifies that it's really a factory, and thus
; safe to run with private data (modulo side-channel attacks):
(define p-instance (factory-yield p-factory))

; Bob finally calls the object created by the factory:
(define result (p-instance d))
