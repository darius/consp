;; This is the first code to start running in the CONSP system.  It
;; inherits all privileges via the unsafe scope, and gives out subsets
;; of the root privileges to users, initially just the 'admin' user
;; which has the power to create new users.


;; Utility functions

(define (snarf-port port)
  (let reading ()
    (let ((x (read port)))
      (if (eof-object? x) 
          '()
          (cons x (reading))))))

(define (acons key value a-list)
  (cons (cons key value) a-list))

(define (push! element list-box)
  (put! list-box (cons element (get list-box))))

(define (pop! list-box)
  (let ((result (car (get list-box))))
    (put! list-box (cdr (get list-box)))
    result))

(define (enqueue! list-box element)
  (put! list-box (append (get list-box) (list element))))

(define (all ok? ls)
  (or (null? ls)
      (and (ok? (car ls))
           (all ok? (cdr ls)))))

(define (filter include? ls)
  (cond ((null? ls) '())
        ((include? (car ls))
         (cons (car ls) (filter include? (cdr ls))))
        (else (filter include? (cdr ls)))))

(define (scope-values scope)
  (map (lambda (var) (eval var scope))
       (scope-variables scope)))

(define (string-index key string)
  (let ((L (string-length key))
        (M (string-length string)))
    (define (matches-at? i k)
      (cond ((= k L) i)
            (else (and (char=? (string-ref string (+ i k))
                               (string-ref key k))
                       (matches-at? i (+ k 1))))))
    (let loop ((i 0))
      (and (<= (+ i L) M)
           (or (matches-at? i 0)
               (loop (+ i 1)))))))

(define (make-sealer name)
  (define p (make-record-type name 1 #f))
  `((seal . ,(car p)) (unseal . ,(caddr p)) (sealed? . ,(cadr p))))


;; A public directory for initial introductions.

(define repository (box '()))

(define (lookup publisher key) 
  (cond ((assoc (cons publisher key) (get repository)) => cdr)
        (else #f)))

(define (make-publish! publisher) 
  (lambda (key object) 
    (put! repository
          (acons (cons publisher key) object (get repository)))))


;; Factories.
;; http://www.cap-lore.com/CapTheory/Language/Scheme/SchemeFactory.html
;; This can be defined at the user level, but we provide it here so
;; all users have the same factory type from the beginning.

(import (make-sealer 'factory) seal unseal)

(define (make-factory expression)
  (if (obviously-immutable? expression)
      (seal (eval `(lambda () ,expression) safe-scope))
      (panic "Program is not obviously immutable" expression)))

(define (factory-yield factory)
  ((unseal factory)))

;; This has to ensure that the expression contains no objects that
;; might provide a communication channel.  (It'd be reasonable to put
;; this requirement into our Scheme dialect's syntax and have it
;; checked by the compiler instead of by this code here, but we aren't
;; taking that route.  Maybe we should.)

;; Return true iff X is guaranteed to be transitively immutable.  This
;; is almost, but not quite, like selflessness in E (an immutable
;; object can still have EQ?-identity).
(define (obviously-immutable? x)
  (and (immutable-type? x)
       (all obviously-immutable? (components x))))

(define (components x)
  (cond ((pair? x) (list (car x) (cdr x)))
        ((vector? x) (vector->list x))
        (else '())))

(define (immutable-type? x)
  (or (null? x)
      (boolean? x) 
      (number? x)
      (symbol? x)
      (eof-object? x)
      (string? x)                       ; Of course, pairs, vectors, and 
      (pair? x)                         ; strings are not immutable in 
      (vector? x)                       ; standard Scheme.
      (and (procedure? x) (memq x primitive-procedures))))

(define primitive-procedures
  (filter procedure? (scope-values safe-scope)))


;; A crude module system.

(define use-entry-type (make-record-type 'use-entry 2 #t))
(define make-use-entry          (list-ref use-entry-type 0))
(define use-entry?              (list-ref use-entry-type 1))
(define use-entry.contents      (list-ref use-entry-type 2))
(define use-entry.value         (list-ref use-entry-type 3))
(define use-entry.set-contents! (list-ref use-entry-type 4))
(define use-entry.set-value!    (list-ref use-entry-type 5))

(define circular-use-tag (list 'circular-use))

(define (make-use snarf scope)

  ; A-list from filenames to use-entries.
  (define use-table (box '()))

  (define (eval-use entry)
    (use-entry.set-value! entry circular-use-tag)
    (let ((value (eval `(begin ,@(use-entry.contents entry))
                       (sprout scope))))
      (use-entry.set-value! entry value)
      value))

  (lambda (filename)
    (let ((contents (snarf filename)))
      (cond ((and #f                    ; TODO: reenable this after we can
                                        ; ensure the caching is safe.
                  (assoc filename (get use-table)))
             => (lambda (pair)
                  (let ((entry (cdr pair)))
                    (if (equal? (use-entry.contents entry) contents)
                        (let ((value (use-entry.value entry)))
                          (if (eq? value circular-use-tag)
                              (panic "Circular USE" filename))
                          value)
                        (begin (use-entry.set-contents! entry contents)
                               (eval-use entry))))))
            (else
             (let ((entry (make-use-entry contents '*)))
               ; (put! use-table (acons filename entry (get use-table)))
               (eval-use entry)))))))


;; Support for limited filesystems.

(define (assert-safe-path filename)
  (if (or (= 0 (string-length filename))
          (char=? #\/ (string-ref filename 0))
          (string-index ".." filename))
      (panic "Illegal USE filename" filename))
  void)


;; Mailboxes

(define mailbox-type (make-record-type 'mailbox 1 #f))
(define make-mailbox  (list-ref mailbox-type 0))
(define mailbox?      (list-ref mailbox-type 1))
(define mailbox.inbox (list-ref mailbox-type 2))

(define (build-mailbox)
  (make-mailbox (box '())))

(define (mailbox-empty? mailbox)
  (null? (get (mailbox.inbox mailbox))))

(define (mailbox-enqueue! mailbox message)
  (enqueue! (mailbox.inbox mailbox) message))

(define (make-fetch-mail! receiver)
  (lambda ()
    (let ((mailbox (lookup receiver 'mailbox)))
      (and (not (mailbox-empty? mailbox))
           (pop! (mailbox.inbox mailbox))))))

(define (make-send sender)
  (lambda (receiver message)
    (mailbox-enqueue! (lookup receiver 'mailbox)
                      (acons 'sender sender message))))


;; A user's domain.  A user has a NAME, a mutable scope to interact
;; with the interpreter within, a publishing space, a mailbox, and a
;; filesystem subtree headed by ROOT.

(define (make-user name root)

  (define (reroot filename)
    (assert-safe-path filename)
    (string-append root filename))

  (define (wrap-open-file open-file)
    (lambda (filename)
      (open-file (reroot filename))))

  (define (wrap-cwf call-with-file)
    (lambda (filename procedure)
      (call-with-file (reroot filename) procedure)))

  (define wrapped-call-with-input-file (wrap-cwf call-with-input-file))

  (define (snarf filename)
    (wrapped-call-with-input-file filename snarf-port))

  (define pure-scope        (sprout safe-scope))
  (define base-scope        (sprout unsafe-scope))
  (define interactive-scope (sprout-mutable base-scope))

  (define (do-load filename)
    (eval `(begin ,@(snarf filename)) interactive-scope))

  (define use (make-use snarf pure-scope))

  (define (define-safe! name value)
    (define! pure-scope name value)
    (define! base-scope name value))

  (define-safe! 'make-factory make-factory)
  (define-safe! 'factory-yield factory-yield)
  (define-safe! 'use use)
  (define-safe! 'make-sealer make-sealer)

  (define! base-scope 'who-am-i name) ;Hm, should this be in the pure scope?
  (define! base-scope 'snarf-program (lambda (filename)
                                       `(begin ,@(snarf filename))))

  ; XXX should load be defined in base-scope, but extend interactive-scope?
  (define! base-scope 'load                  do-load)
  (define! base-scope 'open-input-file       (wrap-open-file open-input-file))
  (define! base-scope 'open-output-file      (wrap-open-file open-output-file))
  (define! base-scope 'call-with-input-file  wrapped-call-with-input-file)
  (define! base-scope 'call-with-output-file (wrap-cwf call-with-output-file))

  (define! base-scope 'lookup                lookup)
  (define! base-scope 'publish!              (make-publish! name))

  (define! base-scope 'unsafe-scope          base-scope)
  (define! base-scope 'interactive-scope     interactive-scope)

  ((make-publish! name) 'mailbox (build-mailbox))
  (define! base-scope 'fetch-mail!           (make-fetch-mail! name))
  (define! base-scope 'send                  (make-send name))

  interactive-scope)


;; The read-eval-print loop.  Enter "quit" to exit.

(define (interactive-loop scope)
  (display "> ")
  (let ((e (read)))
    (cond ((or (eof-object? e) (eq? 'quit e))
           void)
          (else (print (eval e scope))
                (interactive-loop scope)))))

(define (print x)
  (cond ((not (eq? x void))
         (write x)
         (newline))))


;; The set of users -- initially just the administrator.

;; The admin is an ordinary user with a couple of extra powers: to add
;; to and view the user list, and to edit the system source code
;; (since that's placed in the admin's directory tree).  (The admin
;; can delete a user by adding a dummy with the same name; to mess
;; with the user any further they'd have to hack the system source and
;; restart.)  A full non-toy system might need some extra
;; administrator powers.  It's tempting to give the admin full root
;; privileges to be able to cope with anything that can possibly come
;; up -- I'm curious how needful that would be in practice.  It's not
;; even possible in an open distributed system.

(define users (box '()))

(define (list-users)
  (map car (get users)))

(define (add-user! name user)
  (put! users (acons name user (get users))))

(define (add-ordinary-user! name)
  (add-user! name (make-ordinary-user name)))

(define (make-ordinary-user name)
  ;; N.B. This should also create the user's directory, but there's
  ;; no R4RS procedure to create a directory.
  (make-user name
             (string-append "users" 
                            file-separator
                            (symbol->string name)
                            file-separator)))

(define admin-scope (make-ordinary-user 'admin))

(define! admin-scope 'list-users         list-users)
(define! admin-scope 'add-user!          add-user!)
(define! admin-scope 'add-ordinary-user! add-ordinary-user!)

(add-user! 'admin admin-scope)

;; And this is the login program.  We don't bother with passwords or
;; anything in this demo.
(lambda (username . opt-program)
  (let ((scope (cdr (assq username (get users)))))
    (if (null? opt-program)
        (interactive-loop scope)
        (eval (car opt-program) scope))))
