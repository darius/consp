;; Silly regression test, try everything out.  To run it:
;; ((use "regress.scm") (current-output-port))

(define (regress port)

  (define (print x) 
    (write x port)
    (newline port))

; This one won't work currently because different imports of money.scm
; lead to different purse types, after we disabled caching in USE.
;  (print (use "example.scm"))

  (print ((use "junk.scm")))
  (print (use "factory-example.scm"))
  (print (((use "lunch.scm")))))
