(import (use "stuff.scm") make-one-shot inc!)

;; Each voter can vote for any subset of the choices, but they can
;; cast at most one vote for each.  A voter is a procedure that takes
;; an a-list mapping choices to vote-thunks.  It calls a vote-thunk to
;; cast a vote for the corresponding choice.  START-VOTING calls all
;; the voters and returns a thunk to be called when it's time to tally
;; the results.
(define (start-voting voters choices)

  (define ballot-box
    (map (lambda (c) (box 0)) choices))

  (define (poll! voter)
    (voter (map (lambda (c b) 
		  (cons c (make-one-shot (lambda () (inc! b 1)))))
		choices
		ballot-box)))

  (define (tally-votes)
    (map (lambda (c b) (list c (get b)))
	 choices
	 ballot-box))

  (for-each poll! voters)
  tally-votes)

(define (vote-for choice options)
  ((cdr (assoc choice options))))

(export start-voting vote-for)

;; We should probably supply a means of notification that people
;; are done voting.

;; An auditable voting procedure might be an interesting problem...
;; Something like this: when you cast your vote you get an opaque
;; receipt.  The final tally object can be checked against the
;; receipts somehow.

;; A bit more concretely: we use some variation on the factory pattern
;; to vouch for the provenance of the procedures used and of the
;; result.  So the result is a record containing the tallies and a
;; list of receipts.  Each receipt holder can check the list and make
;; sure they're on it...  

;; Now this leaves some remaining threats:
;;
;; - The tally-sealer might not be closely-held, but given out to
;; someone who seals a fake tally.  This is probably easily avoidable.
;;
;; - The vote-collector and tally-unsealer might not be the ones
;; created by the publically-inspectable source code.  We could have a
;; notary that stamps its product with the source code that created
;; it.  (But that reduces to the same sort of trust in the notary.)
;;
;; - The voting software might've been run under a debugger.  Defer
;; this issue until we have some debugging infrastructure...

;; Perhaps more simply, the voter could just remember the identity of
;; the voting object they used.  The vote tallies and the voting
;; object are then securely bundled in the result announcement, and
;; the voter checks the identity.  This still requires a link between
;; the voting sourcecode and the voting object.
