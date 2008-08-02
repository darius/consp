;; Tests of the loaded system from loadme.scm.

(define (regress)
  (i-am 'alice '((use "regress.scm") (current-output-port))))

(define (do-tests)
  (expect '((sender . alice) (subject . "Test message") (body . "Hello, Bob!"))
          test-mail)
  (expect 42 test-factory)
  (expect '(("Mandarin" 2) ("Casa Lupe" 0) ("Coffee Shop" 1))
          test-voting)
  (expect 40 test-betting)
  "All tests passed")

(define (expect expected-value thunk)
  (let ((value (thunk)))
    (cond ((not (equal? value expected-value))
           (panic "Unexpected test result" thunk value)))))

(define (test-mail)
  (assert (not (i-am 'bob '(fetch-mail!))))
  (i-am 'alice '(send 'bob `((subject . "Test message") 
                             (body . "Hello, Bob!"))))
  (i-am 'bob   '(fetch-mail!)))

(define (test-factory)

  ;; Alice has a program P whose source code she doesn't want to give out:
  (i-am 'alice '(define p '(lambda (x) (+ x x))))

  ;; Bob has data D he wants to run P on, without revealing D to Alice.
  (i-am 'bob   '(define d 21))

  ;; So Alice gives Bob a p-factory:
  (i-am 'alice '(publish! 'p (make-factory p)))

  ;; Bob receives it and verifies that it's really a factory, and thus
  ;; safe to run with private data (modulo side-channel attacks):
  (i-am 'bob   '(define p-factory (lookup 'alice 'p)))
  (i-am 'bob   '(define p-instance (factory-yield p-factory)))

  ;; Bob finally calls the object created by the factory:
  (i-am 'bob   '(p-instance d)))

;; Voting on where to go out for lunch.
;; http://www.cap-lore.com/CapTheory/Language/Lunch.html
(define (test-voting)
  (i-am 'alice
   '(begin
     (import (use "voting.scm") start-voting)
     (define explanation
       "Time to vote on our lunch meeting!  Pick restaurants from the
options in this message and call the corresponding thunks.")
     (define restaurants '("Mandarin" "Casa Lupe" "Coffee Shop"))
     (define lunch-crowd '(bob carol))
     (define (mail-ballots)
       (define voters (map (lambda (user)
                             (lambda (options)
                               (send user
                                     `((subject . "Lunch vote")
                                       (body . ,explanation)
                                       ,@options))))
                           lunch-crowd))
       (start-voting voters restaurants))
     (define tally (mail-ballots))))
  (i-am 'bob   '(begin (define _ (fetch-mail!))
                       ((cdr (assoc "Mandarin" _)))))
  (i-am 'carol '(begin (define _ (fetch-mail!))
                       ((cdr (assoc "Mandarin" _)))
                       ((cdr (assoc "Coffee Shop" _)))))
  (i-am 'alice '(tally)))

;; Yow, this was a lot of typing.
(define (test-betting)
  
  ;; First set up the banking system...
  (i-am 'alice 
        '(begin
          (import (use "money.scm") 
                  purse? make-money make-purse purse-balance purse-transfer!)
          (publish! 'money              ;N.B. we don't publish MAKE-MONEY.
                    (export purse? make-purse purse-balance purse-transfer!))
          (send 'bob   `((subject . "Here's some dough")
                         (purse . ,(make-money 20))))
          (send 'carol `((subject . "Here's some dough")
                         (purse . ,(make-money 30))))))
  (i-am 'bob   '(define (fetch key) (cdr (assoc key _))))
  (i-am 'carol '(define (fetch key) (cdr (assoc key _))))
  (i-am 'bob   '(begin (define _ (fetch-mail!))
                       (import (lookup 'alice 'money) 
                        purse? make-purse purse-balance purse-transfer!)
                       (define my-purse (fetch 'purse))))
  (i-am 'carol '(begin (define _ (fetch-mail!))
                       (import (lookup 'alice 'money) 
                        purse? make-purse purse-balance purse-transfer!)
                       (define my-purse (fetch 'purse))))
  
  ;; Now set up a bet.
  (i-am 'bob   '(begin
                 (send 'carol
                  `((body . "Betcha my Roshambo player can maul yours.
$10 on 20 rounds?")))))
  (i-am 'carol '(begin
                 (define _ (fetch-mail!))
                 (send 'bob 
                  `((body . "You're on. Let's ask Alice to judge it.")))
                 (define dc3-factory
                   (make-factory (snarf-program "dc3-player.scm")))
                 (define stake (make-purse))
                 (purse-transfer! 10 my-purse stake)
                 (send 'alice
                  `((body . "Please run a 20-round Roshambo match
between this player factory and Bob's.
The stakes are $10 at even odds.")
                    (player . ,dc3-factory)
                    (purse . ,stake)))))
  (i-am 'bob   '(begin
                 (define _ (fetch-mail!))
                 (define shortsight-factory
                   (make-factory (snarf-program "shortsight.scm")))
                 (define stake (make-purse))
                 (purse-transfer! 10 my-purse stake)
                 (send 'alice
                  `((body . "Please run a 20-round Roshambo match
between this player factory and Carol's.
The stakes are $10 at even odds.")
                    (player . ,shortsight-factory)
                    (purse . ,stake)))))

  ;; Alice judges it.
  (i-am 'alice '(define (fetch key) (cdr (assoc key _))))
  (i-am 'alice '(begin
                 (define _ (fetch-mail!))
                 (define carol-purse (fetch 'purse))
                 (define carol-player (fetch 'player))
                 (define (assert x)
                   (if (not x) (panic "Assertion failed")))
                 (assert (= 10 (purse-balance carol-purse)))
                 (define _ (fetch-mail!))
                 (define bob-purse (fetch 'purse))
                 (define bob-player (fetch 'player))
                 (assert (= 10 (purse-balance bob-purse)))
                 (define play (use "umpire.scm"))
                 (define results (play (factory-yield bob-player)
                                       (factory-yield carol-player)
                                       20))
                 (define winner 
                   (cond ((apply > results) 'bob)
                         ((apply < results) 'carol)
                         (else #f)))
                 (define total-purse (make-purse))
                 (purse-transfer! 10 bob-purse total-purse)
                 (purse-transfer! 10 carol-purse total-purse)
                 (send winner
                       `((body . "You won!")
                         (results ,results)
                         (purse . ,total-purse)))))

  ;; Carol retrieves her winnings.
  (i-am 'carol '(begin
                 (define _ (fetch-mail!))
                 (purse-transfer! 20 (fetch 'purse) my-purse)
                 (purse-balance my-purse))))
