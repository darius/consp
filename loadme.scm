;; System-specific configuration

;; Replace this with your Scheme system's error procedure.
;(define panic @error)
(define (panic message . irritants)
  please-supply-a-real-error-procedure  ; causes an unbound-variable error
  'should-not-return)

(define file-separator "/")   ; or "\\" on Windows

;; End of system-specific configuration


;; Load the system.
(define (report x)
  (cond (#f (display x)
	    (newline))))

(define (loud-load . path-components)
  (let ((filename (make-pathname path-components)))
    (report filename)
    (load filename)))

(define (make-pathname path-components)
  (apply string-append
	 (intercalate file-separator path-components)))

(define (intercalate between elements)
  (cons (car elements)
	(let loop ((rest (cdr elements)))
	  (if (null? rest)
	      '()
	      `(,between ,(car rest) ,@(loop (cdr rest)))))))

(loud-load "users" "admin" "scheme" "preadapter.scm")
(loud-load "users" "admin" "consp.scm")
(report "All loaded")


;; Top-level driver program
(define (make-top-level program)
  (meta-eval `((lambda () ,program)) meta-unsafe-scope))

;; To get an interactive prompt, enter (i-am 'alice) [or bob or carol].
(define i-am
  (make-top-level 
   (snarf-program (make-pathname (list "users" "admin" "boot.scm")))))

;; The initial set of users:
(i-am 'admin '(add-ordinary-user! 'alice))
(i-am 'admin '(add-ordinary-user! 'bob))
(i-am 'admin '(add-ordinary-user! 'carol))


(load "tests.scm")
