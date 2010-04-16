;; This implements the Consp core language and primitives.  It's
;; written to run under either R4RS Scheme or itself, if you first
;; load the appriopriate preadapter.


;; Misc helper functions

(define (assert ok?)
  (cond ((not ok?)
         (panic "Assertion failed")))
  void)

(define (snarf-program filename)
  `(begin ,@(snarf filename)))

(define (snarf filename)
  (call-with-input-file filename
    (lambda (port)
      (let reading ()
        (let ((x (read port)))
          (if (eof-object? x) 
              '()
              (cons x (reading))))))))

(define (all ok? ls)
  (or (null? ls)
      (and (ok? (car ls))
           (all ok? (cdr ls)))))

(define gentemp
  (let ((counter (box 0)))
    (lambda ()
      (put! counter (+ 1 (get counter)))
      (string->symbol
       (string-append "#!g_" (number->string (get counter)))))))

(define (listify improper-list)
  (if (pair? improper-list)
      (cons (car improper-list) (listify (cdr improper-list)))
      (list improper-list)))

(define (list-end x)
  (if (pair? x)
      (list-end (cdr x))
      x))

(define (acons key value a-list)
  (cons (cons key value) a-list))

(define (delq x xs)
  (cond ((null? xs) '())
        ((eq? x (car xs)) (cdr xs))
        (else (cons (car xs) (delq x (cdr xs))))))


;; Reduce expression E to kernel-Scheme form.

(define (elaborate e)
  (cond ((symbol? e) e)
        ((not (pair? e)) (list 'quote e))
        (else (let ((oper (car e))
                    (rands (cdr e)))
                (insist e (list? rands))
                (case oper
                  ((quote) 
                   (insist e (= 1 (length rands)))
                   e)
                  ((lambda) 
                   (insist e (valid-formals? (car rands)))
                   `(lambda ,(car rands)
                      ,(elaborate `(begin ,@(cdr rands)))))
                  ((if) 
                   (insist e (<= 2 (length rands) 3))
                   `(if ,(elaborate (car rands))
                        ,(elaborate (cadr rands))
                        ,(elaborate (if.alternative rands))))
                  ((define)
                   (insist e (or (and (symbol? (car rands))
                                      (= 2 (length rands)))
                                 (and (valid-formals? (car rands))
                                      (<= 2 (length rands)))))
                   `(define ,(define.name rands) 
                      ,(elaborate (define.value rands))))
                  ((begin)
                   (if (null? rands)
                       `',void
                       (let loop ((e (car rands)) (rands (cdr rands)))
                         (if (null? rands)
                             (elaborate e)
                             `(begin ,(elaborate e)
                                     ,(loop (car rands) (cdr rands)))))))
                  (else
                   (cond ((get-expander oper)
                          => (lambda (expander)
                               (elaborate (expander e rands))))
                         (else (map elaborate e)))))))))

(define (if.alternative rands)
  (if (null? (cddr rands))
      #f
      (caddr rands)))

(define (define.name rands)
  (if (symbol? (car rands))
      (car rands)
      (caar rands)))

(define (define.value rands)
  (if (symbol? (car rands))
      (cadr rands)
      `(lambda ,(cdar rands) ,@(cdr rands))))

; True iff formals is a (proper or improper) list of symbols, all distinct.
(define (valid-formals? formals) 
  (let loop ((formals formals) (vars '()))
    (define (valid-var? formal)
      (and (symbol? formal)
           (not (memq formal vars))))
    (or (null? formals)
        (valid-var? formals)
        (and (pair? formals)
             (valid-var? (car formals))
             (loop (cdr formals)
                   (cons (car formals) vars))))))


;; Macros

(define expanders (box '()))

(define (get-expander tag)
  (cond ((assq tag (get expanders)) => cdr)
        (else #f)))

(define (define-expander tag expander)
  (put! expanders (acons tag expander (get expanders))))

(define (syntax-error message . irritants)
  (panic "Syntax error" message irritants))

(define (insist exp ok?)
  (cond ((not ok?)
         (syntax-error "Bad syntax" exp))))


;; R4RS special forms
;; This is a hacked extract from the UTS Scheme implementation at
;; http://wry.me/~darius

(define-expander 'let
  (lambda (expr rands)
    (insist expr (pair? rands))
    (if (symbol? (car rands)) ; named-let form
        (begin
         (insist expr (valid-let? (cdr rands)))
         (let ((proc (car rands)) 
               (decls (cadr rands)) 
               (body (cddr rands)))
           `((letrec ((,proc (lambda ,(map car decls) ,@body)))
               ,proc)
             ,@(map cadr decls))))
        (begin
         (insist expr (valid-let? rands))
         (let ((names (map car (car rands)))
               (exps (map cadr (car rands)))
               (body (cdr rands)))
           (if (null? names)
               `(begin ,@body)
               `((lambda ,names ,@body) ,@exps)))))))

(define-expander 'letrec
  (lambda (expr rands)
    (insist expr (valid-let? rands))
    (let ((vars (map car (car rands)))
          (exps (map cadr (car rands)))
          (body (cdr rands)))
      `(let ()
         ,@(map (lambda (var exp) `(define ,var ,exp)) vars exps)
         ,@body))))

(define-expander 'let*
  (lambda (expr rands)
    (insist expr (pair? rands))
    (let ((decls (car rands))
          (body (cdr rands)))
      (if (null? decls)
          `(let () ,@body)
          `(let (,(car decls))
             (let* ,(cdr decls) ,@body))))))

(define-expander 'and
  (lambda (expr rands)
    (case (length rands)
      ((0) #t)
      ((1) (car rands))
      (else `(if ,(car rands) (and ,@(cdr rands)) #f)))))

(define-expander 'or
  (lambda (expr rands)
    (case (length rands)
      ((0) #f)
      ((1) (car rands))
      (else (let ((head (gentemp)))
              `(let ((,head ,(car rands)))
                 (if ,head ,head (or ,@(cdr rands)))))))))

(define-expander 'cond
  (lambda (expr rands)
    (cond
      ((null? rands) `',void)
      ((not (pair? (car rands)))
       (syntax-error "Invalid cond clause" (car rands)))
      ((eq? (caar rands) 'else)
       (if (null? (cdr rands))
           `(begin ,@(cdar rands))
           (syntax-error "Else-clause is not last" rands)))
      ((null? (cdar rands))
       `(or ,(caar rands) (cond ,@(cdr rands))))
      ((and (pair? (cdar rands)) (eq? (cadar rands) '=>))
       (cond ((not (and (list? (car rands))
                        (= (length (car rands)) 3)))
              (syntax-error "Bad cond clause syntax" rands)))
       (let ((test-var (gentemp)))
         `(let ((,test-var ,(caar rands)))
            (if ,test-var
                (,(caddar rands) ,test-var)
                (cond ,@(cdr rands))))))
      (else `(if ,(caar rands) 
                 (begin ,@(cdar rands))
                 (cond ,@(cdr rands)))))))

(define-expander 'case
  (lambda (expr rands)
    (insist expr (pair? rands))
    (let ((test (car rands))
          (sym (gentemp)))
      `(let ((,sym ,test))
         (cond 
           ,@(map 
              (lambda (clause)
                (cond
                  ((eq? (car clause) 'else)
                   clause)
                  ((null? (cdar clause))
                   `((,equal? ,sym ',(caar clause))
                     ,@(cdr clause)))
                  (else
                   `((,member ,sym ',(car clause))
                     ,@(cdr clause)))))
              (cdr rands)))))))

(define-expander 'do
  (lambda (expr rands)
    (insist expr (and (<= 2 (length rands))
                 (let valid-clauses? ((clauses (car rands)))
                   (if (null? clauses)
                       #t
                       (let ((clause (car clauses)))
                         (and (pair? clause)
                              (symbol? (car clause))
                              (pair? (cdr clause))
                              (if (null? (cddr clause))
                                  #t
                                  (null? (cdddr clause)))
                              (valid-clauses? (cdr clauses))))))
                 (pair? (cadr rands))))
    (let ((loop (gentemp))
          (variables (map car (car rands)))
          (inits (map cadr (car rands)))
          (steps (map (lambda (clause)
                        (if (null? (cddr clause))
                            (car clause) ;default step leaves var unchanged.
                            (caddr clause)))
                      (car rands)))
          (test (caadr rands))
          (result (cdadr rands))
          (body (cddr rands)))
      `(letrec ((,loop 
                 (lambda ,variables
                   (if ,test
                       (begin ,@result)
                       (begin ,@body
                              (,loop ,@steps))))))
         (,loop ,@inits)))))

(define-expander 'quasiquote
  (lambda (expr rands)
    (insist expr (= (length rands) 1))
    (expand-quasiquote (car rands) 0)))


;; LET forms

; True iff `(LET ,rands) is a valid let-expression (simple, not 
; named LET).  This is helpful in parsing named LET and LETREC, too.
(define (valid-let? rands)
  (and (<= 2 (length rands))
       (list? (car rands))
       (all (lambda (decl) 
              (and (pair? decl)
                   (pair? (cdr decl))
                   (null? (cddr decl))
                   (symbol? (car decl))))
            (car rands))
       (valid-formals? (map car (car rands)))))


;; Backquote expansion
;; Based on _Paradigms of AI Programming_ p. 824
;; (changed to expand nested quasiquotes as in R4RS,
;; and to be hygienic)

(define (expand-quasiquote exp nesting)
  (define (insist ok?)
    (cond ((not ok?) (syntax-error "Bad syntax" exp))))
  (cond
   ((vector? exp)
    `(,list->vector ,(expand-quasiquote (vector->list exp) nesting)))
   ((not (pair? exp)) 
    (if (constant? exp) exp (list 'quote exp)))
   ((eq? (car exp) 'unquote)
    (insist (= (length exp) 2))
    (if (= nesting 0)
        (cadr exp)
        (combine-skeletons ''unquote 
                           (expand-quasiquote (cdr exp) (- nesting 1))
                           exp)))
   ((eq? (car exp) 'quasiquote)
    (insist (= (length exp) 2))
    (combine-skeletons ''quasiquote 
                       (expand-quasiquote (cdr exp) (+ nesting 1))
                       exp))
   ((and (pair? (car exp))
         (eq? (caar exp) 'unquote-splicing))
    (insist (= (length (car exp)) 2))
    (if (= nesting 0)
        (if (null? (cdr exp))
            (cadar exp)
            `(,append ,(cadar exp)
                      ,(expand-quasiquote (cdr exp) nesting)))
        (combine-skeletons (expand-quasiquote (car exp) (- nesting 1))
                           (expand-quasiquote (cdr exp) nesting)
                           exp)))
   (else (combine-skeletons (expand-quasiquote (car exp) nesting)
                            (expand-quasiquote (cdr exp) nesting)
                            exp))))

(define (combine-skeletons left right exp)
  (define (my-eval constant)
    (if (pair? constant)       ;; must be quoted constant
        (cadr constant)
        constant))
  (if (and (constant? left) (constant? right)) 
      (if (and (eqv? (my-eval left) (car exp))
               (eqv? (my-eval right) (cdr exp)))
          (list 'quote exp)
          (list 'quote (cons (my-eval left) (my-eval right))))
      `(,cons ,left ,right)))

(define (constant? exp)
  (if (pair? exp)
      (eq? (car exp) 'quote)
      (not (symbol? exp))))


;; New Consp special forms

(define-expander 'export
  (lambda (expr rands)
    (define (expand-export clause)
      `(,cons ',(fooport-key clause) ,(fooport-value clause)))
    (insist expr (valid-import/export? rands))
    `(list ,@(map expand-export rands))))

(define-expander 'import
  (lambda (expr rands)
    (insist expr (and (<= 1 (length rands))
                      (valid-import/export? (cdr rands))))
    (let ((subject (gentemp)))
      (define (expand-import clause)
        `(define ,(fooport-value clause)
           (,cdr (,assv ',(fooport-key clause) ,subject))))
      `(begin
        (define ,subject ,(car rands))
        ,@(map expand-import (cdr rands))))))

(define (valid-import/export? clauses)
  (define (valid-clause? clause)
    (or (symbol? clause)
        (and (list? clause) 
             (= (length clause) 2)
             (symbol? (car clause))
             (symbol? (cadr clause)))))
  (all valid-clause? clauses))

(define (fooport-key clause)
  (if (symbol? clause) clause (car clause)))

(define (fooport-value clause)
  (if (symbol? clause) clause (cadr clause)))


;; Scopes
;; #f represents the empty scope.
;; Nonempty scopes use a scope record.

(define scope-type       (make-record-type 'scope 3 #t))
(define make-scope       (list-ref scope-type 0))
(define scope?           (list-ref scope-type 1))
(define scope.enclosing  (list-ref scope-type 2))
(define scope.frame      (list-ref scope-type 3))
(define scope.mutable?   (list-ref scope-type 4))
(define scope.set-frame! (list-ref scope-type 6))

(define (extend r vars vals)
  (cond ((and r (not (scope? r)))
         (panic "Not a scope" r)))
  (make-scope r (map cons vars vals) #f))

(define (meta-sprout-mutable r)
  (cond ((and r (not (scope? r)))
         (panic "Not a scope" r)))
  (make-scope r '() #t))

(define (meta-sprout r)
  (extend r '() '()))

(define (lookup r var)
  (cond ((not r) (panic "Unbound variable" var))
        ((assq var (scope.frame r)) => cdr)
        (else (lookup (scope.enclosing r) var))))

(define (meta-define! r var val)
  (scope.set-frame! 
   r
   (acons var val
          (cond ((assq var (scope.frame r))
                 => (lambda (binding) 
                      (cond ((not (scope.mutable? r))
                             (panic "DEFINE misused to assign" var)))
                      (delq binding (scope.frame r))))
                (else (scope.frame r))))))

(define (meta-scope-variables r)
  (if (not r)
      '()
      (append (map car (scope.frame r))
              (meta-scope-variables (scope.enclosing r)))))


;; The interpreter

(define (meta-eval e r)
  (really-evaluate (elaborate e) r))

(define (really-evaluate e r)
  (let ev ((e e))
    (cond ((symbol? e) (lookup r e))
          (else (let ((oper (car e))
                      (rands (cdr e)))
                  (case oper
                    ((quote)
                     (car rands))
                    ((lambda) 
                     (if (symbol? (list-end (car rands)))
                         (let ((params (listify (car rands))))
                           (lambda args
                             (really-evaluate
                              (cadr rands)
                              (extend r params
                                      (varargs-vals params args)))))
                         (lambda args
                           (really-evaluate (cadr rands)
                                            (extend r (car rands) args)))))
                    ((define)
                     (let ((value (ev (cadr rands))))
                       (meta-define! r (car rands) value)
                       value))
                    ((if) 
                     (if (ev (car rands))
                         (ev (cadr rands))
                         (ev (caddr rands))))
                    ((begin)
                     (ev (car rands))
                     (ev (cadr rands)))
                    (else
                     (apply (ev oper) (map ev rands)))))))))

(define (varargs-vals vars vals)
  (if (null? (cdr vars))
      (list vals)
      (cons (car vals)
            (varargs-vals (cdr vars) (cdr vals)))))


;; R4RS primitive procedures

(define safe-primitives
  '(* + - / < <= = > >= abs acos append apply asin assoc assq assv 
    atan boolean? caaaar caaadr caaar caadar caaddr caadr caar
    cadaar cadadr cadar caddar cadddr caddr cadr car cdaaar cdaadr cdaar
    cdadar cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr cddr
    cdr ceiling char->integer char-alphabetic? char-ci<=? char-ci<? 
    char-ci=? char-ci>=? char-ci>? char-downcase char-lower-case? 
    char-numeric? char-upcase char-upper-case? char-whitespace? char<=? 
    char<? char=? char>=? char>? char? complex? cons cos eof-object? eq? 
    equal? eqv? even? exact->inexact exact? exp expt floor gcd 
    inexact->exact inexact? input-port? integer->char integer? 
    lcm length list list->string list->vector list-ref list-tail list? 
    log map max member memq memv min modulo negative? not null? 
    number->string number? odd? output-port? pair? positive? procedure? 
    quotient rational? real? remainder reverse round sin sqrt string
    string->list string->number string->symbol string-append string-ci<=? 
    string-ci<? string-ci=? string-ci>=? string-ci>? string-length 
    string-ref string<=? string<? string=? string>=? string>? string?
    substring symbol->string symbol? tan truncate vector zero?))
(define safe-primitive-values
  (list * + - / < <= = > >= abs acos append apply asin assoc assq assv 
    atan boolean? caaaar caaadr caaar caadar caaddr caadr caar
    cadaar cadadr cadar caddar cadddr caddr cadr car cdaaar cdaadr cdaar
    cdadar cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr cddr
    cdr ceiling char->integer char-alphabetic? char-ci<=? char-ci<? 
    char-ci=? char-ci>=? char-ci>? char-downcase char-lower-case? 
    char-numeric? char-upcase char-upper-case? char-whitespace? char<=? 
    char<? char=? char>=? char>? char? complex? cons cos eof-object? eq? 
    equal? eqv? even? exact->inexact exact? exp expt floor gcd 
    inexact->exact inexact? input-port? integer->char integer? 
    lcm length list list->string list->vector list-ref list-tail list? 
    log map max member memq memv min modulo negative? not null? 
    number->string number? odd? output-port? pair? positive? procedure? 
    quotient rational? real? remainder reverse round sin sqrt string
    string->list string->number string->symbol string-append string-ci<=? 
    string-ci<? string-ci=? string-ci>=? string-ci>? string-length 
    string-ref string<=? string<? string=? string>=? string>? string?
    substring symbol->string symbol? tan truncate vector zero?))

(define meta-safe-scope
  (extend #f safe-primitives safe-primitive-values))

(meta-define! meta-safe-scope 'vector?           do-vector?)
(meta-define! meta-safe-scope 'vector-length     do-vector-length)
(meta-define! meta-safe-scope 'vector->list      do-vector->list)
(meta-define! meta-safe-scope 'vector-ref        do-vector-ref)

(meta-define! meta-safe-scope 'for-each          do-for-each)

(define (require-1-arg procedure)
  (lambda (arg0) (procedure arg0)))

(define (require-2-args procedure)
  (lambda (arg0 arg1) (procedure arg0 arg1)))

(meta-define! meta-safe-scope 'peek-char         (require-1-arg peek-char))
(meta-define! meta-safe-scope 'read-char         (require-1-arg read-char))
(meta-define! meta-safe-scope 'read              (require-1-arg read))
(meta-define! meta-safe-scope 'write-char        (require-2-args 
                                                  do-write-char))
(meta-define! meta-safe-scope 'write             (require-2-args do-write))
(meta-define! meta-safe-scope 'display           (require-2-args do-display))
(meta-define! meta-safe-scope 'newline           (require-1-arg do-newline))
(meta-define! meta-safe-scope 'close-input-port  (require-1-arg
                                                  do-close-input-port))
(meta-define! meta-safe-scope 'close-output-port (require-1-arg
                                                  do-close-output-port))


(define unsafe-primitives
  '(peek-char read-char read 
    ;; The names above shadow 'safe' versions; the ones below don't:
    current-input-port current-output-port 
    open-input-file open-output-file
    call-with-input-file call-with-output-file))
(define unsafe-primitive-values
  (list peek-char read-char read 
    ;; The names above shadow 'safe' versions; the ones below don't:
    current-input-port current-output-port 
    open-input-file open-output-file
    call-with-input-file call-with-output-file))


(define meta-unsafe-scope
  (extend meta-safe-scope unsafe-primitives unsafe-primitive-values))

(meta-define! meta-unsafe-scope 'write-char        do-write-char)
(meta-define! meta-unsafe-scope 'write             do-write)
(meta-define! meta-unsafe-scope 'display           do-display)
(meta-define! meta-unsafe-scope 'newline           do-newline)
(meta-define! meta-unsafe-scope 'close-input-port  do-close-input-port)
(meta-define! meta-unsafe-scope 'close-output-port do-close-output-port)


;; New Consp primitive procedures and constants

(meta-define! meta-safe-scope 'box?             box?)
(meta-define! meta-safe-scope 'box              box)
(meta-define! meta-safe-scope 'get              get)
(meta-define! meta-safe-scope 'put!             put!)

(meta-define! meta-safe-scope 'make-record-type make-record-type)
(meta-define! meta-safe-scope 'record?          record?)

(meta-define! meta-safe-scope 'eval             meta-eval)
(meta-define! meta-safe-scope 'sprout           meta-sprout)
(meta-define! meta-safe-scope 'sprout-mutable   meta-sprout-mutable)
(meta-define! meta-safe-scope 'define!          meta-define!)
(meta-define! meta-safe-scope 'scope-variables  meta-scope-variables)

(meta-define! meta-safe-scope 'panic            panic)

(meta-define! meta-safe-scope 'void             void)
(meta-define! meta-safe-scope 'file-separator   file-separator)


(meta-define! meta-unsafe-scope 'safe-scope (meta-sprout meta-safe-scope))


(meta-define! meta-unsafe-scope 'unsafe-scope meta-unsafe-scope)
