;; Load the consp interpreter metacircularly inside the consp system.

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

(loud-load "metacirc" "preadapter.scm")
(loud-load "consp.scm")
(report "All loaded")


;; Top-level driver program
(define (make-top-level program)
  (meta-eval `((lambda () ,program)) meta-unsafe-scope))

;; To get an interactive prompt at the metalevel, enter:
;;  (define i-am (make-meta))
;;  (i-am 'admin)
(define (make-meta)
  (make-top-level (snarf-program "boot.scm")))
