; Random crap

;; Return (map FN (list 0 ... N-1)).
(define (map-n fn n)
  (do ((i (- n 1) (- i 1))
       (acc '() (cons (fn i) acc)))
      ((< i 0) acc)))

(define (make-record-type name arity mutable?)
  (let ((tag (cons record-tag name)))
    (define (make-instance . args)
      (cond ((not (= (length args) arity))
             (panic "Wrong number of arguments to record constructor")))
      (list->vector (cons tag args)))
    (define (instance? x)
      (and (vector? x)
           (< 0 (vector-length x))
           (eq? tag (vector-ref x 0))))
    (define (make-getter index) 
      (let ((i (+ index 1)))
        (lambda (record)
          (cond ((not (instance? record))
                 (panic "Not an instance of" name record)))
          (vector-ref record i))))
    (define (make-setter index) 
      (let ((i (+ index 1)))
        (lambda (record value)
          (cond ((not (instance? record))
                 (panic "Not an instance of" name record)))
          (vector-set! record i value)
          void)))
    (let ((getters (map-n make-getter arity))
          (setters (if mutable? (map-n make-setter arity) '())))
      `(,make-instance ,instance? ,@getters ,@setters))))

(define record-tag (list 'record))

(define (record? x)
  (and (vector? x)
       (< 0 (vector-length x))
       (pair? (vector-ref x 0))
       (eq? record-tag (car (vector-ref x 0)))))

(define (write-record record port)
  (display "#<record: " port)
  (display (cdr (vector-ref record 0)) port)
  (display ">" port))

(define void ((car (make-record-type 'void 0 #f))))

(define box-type (make-record-type 'box 1 #t))
(define box  (car box-type))
(define box? (cadr box-type))
(define get  (caddr box-type))
(define put! (cadddr box-type))


(define (assert-vector v)
  (cond ((record? v)
         (panic "Not really a vector" v)))
  void)

(define do-vector?       (lambda (x)
                           (and (vector? x) (not (record? x)))))
(define do-vector-length (lambda (v)
                           (assert-vector v)
                           (vector-length v)))
(define do-vector->list  (lambda (v)
                           (assert-vector v)
                           (vector->list v)))
(define do-vector-ref    (lambda (v i)
                           (assert-vector v)
                           (vector-ref v i)))


(define (voidify procedure)
  (lambda args
    (apply procedure args)
    void))

(define do-for-each          (voidify for-each))
(define do-write-char        (voidify write-char))
(define do-newline           (voidify newline))
(define do-close-input-port  (voidify close-input-port))
(define do-close-output-port (voidify close-output-port))

(define (do-output writer)
  (lambda (x . opt-port)
    (let ((port (cond ((null? opt-port) (current-output-port))
                      ((null? (cdr opt-port)) (car opt-port))
                      (else (panic "Wrong number of arguments to" writer)))))
      (if (and (vector? x)  ; redundant first check for speed
               (record? x))
          (write-record x port)
          (writer x port))
      void)))

(define do-write (do-output write))
(define do-display (do-output display))
