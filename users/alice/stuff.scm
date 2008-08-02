; Misc useful functions

(define (make-one-shot procedure)
  (let ((runnable? (box #t)))
    (lambda arguments
      (cond ((get runnable?)
             (put! runnable? #f)
             (apply procedure arguments))
            (else (panic "Attempt to call a discharged one-shot"))))))

(define (all ok? ls)
  (or (null? ls)
      (and (ok? (car ls))
           (all ok? (cdr ls)))))

(define (filter include? ls)
  (cond ((null? ls) '())
        ((include? (car ls))
         (cons (car ls) (filter include? (cdr ls))))
        (else (filter include? (cdr ls)))))

(define (inc! box increment)
  (put! box (+ (get box) increment)))

(define (scope-values scope)
  (map (lambda (var) (eval var scope))
       (scope-variables scope)))

(export make-one-shot all filter inc! scope-values)
