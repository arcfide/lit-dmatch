#!chezscheme
(module (@< =>)
  (import-only (chezscheme))

(define-syntax @< 
  (syntax-rules (=>)
    [(_ (name c ...) => (e ...) b1 b2 ...)
     (for-all identifier? #'(name c ... e ...))
     (module-form name (c ...) (e ...) b1 b2 ...)]
    [(_ (name c ...) b1 b2 ...)
     (value-form name (c ...) b1 b2 ...)]))
(define-syntax (build-value-form x) 
  (syntax-case x ()
    [(_ id (ic ...) body ...)
     (with-syntax ([(oc ...) (datum->syntax #'id (syntax->datum #'(ic ...)))])
       #'(let () (alias ic oc) ... body ...))]))
(define-syntax value-form 
  (syntax-rules ()
    [(_ name (c ...) body ...)
     (define-syntax (name x)
       (syntax-case x ()
         [id (identifier? #'id)
          #'(build-value-form id (c ...) ((... ...) body) ...)]
         [(id . rest)
          #'((build-value-form id (c ...) ((... ...) body) ...) 
             . rest)]))]))
(define-syntax (build-module-form x) 
  (syntax-case x ()
    [(_ id (ic ...) (ie ...) body ...)
     (with-syntax ([(oc ...) (datum->syntax #'id (syntax->datum #'(ic ...)))]
                   [(oe ...) (datum->syntax #'id (syntax->datum #'(ie ...)))])
       #'(module (oe ...)
           (alias ic oc) ...
           (module (ie ...) body ...)
           (alias oe ie) ...))]))
(define-syntax module-form 
  (syntax-rules ()
    [(_ name (c ...) (e ...) body ...)
     (define-syntax (name x)
       (syntax-case x ()
         [id (identifier? #'id)
          #'(build-module-form id (c ...) (e ...)
              ((... ...) body) ...)]))]))
(indirect-export @< 
  module-form value-form build-module-form build-value-form)
)
(@< (Helper\x20;macros ) => (ppat )
(define-syntax ppat
  (syntax-rules (unquote)
    [(_ v (unquote var) kt kf) (let ([var v]) kt)]
    [(_ v (x . y) kt kf)
     (if (pair? v)
	 (let ([vx (car v)] [vy (cdr v)])
	   (ppat vx x (ppat vy y kt kf) kf))
	 kf)]
    [(_ v lit kt kf) (if (equal? v (quote lit)) kt kf)]))

)

(@< (Error\x20;handling\x20;procedures pkg-clause ) => (no-matching-pattern ambiguous-pattern/guard )
(define (no-matching-pattern name v-expr v)
  (printf "dmatch ~@[~d~] failed~n~d ~d~n" name v-expr v)
  (error 'dmatch "match failed"))
(define (ambiguous-pattern/guard name v-expr v pkg*)
  (printf "dmatch ~@[~d~] ambiguous matching clauses~n" name)
  (printf "with ~d evaluating to ~d~n" v-expr v)
  (printf "────────────────────────────────────~n")
  (printf "~{~d~n~}" (map pkg-clause pkg*)))

)

(@< (Package\x20;procedures ) => (pkg pkg-thunk pkg-clause )
(define (pkg cls thk) (cons cls thk))
(define (pkg-clause pkg) (car pkg))
(define (pkg-thunk pkg) (cdr pkg))

)

(@< (Helper\x20;procedures no-matching-pattern ambiguous-pattern/guard pkg-thunk ) => (run-a-thunk )
(define (run-a-thunk v-expr v name pkg*)
  (cond
    [(null? pkg*) (no-matching-pattern name v-expr v)]
    [(null? (cdr pkg*)) ((pkg-thunk (car pkg*)))]
    [else (ambiguous-pattern/guard name v-expr v pkg*)]))

)

(@< (Main\x20;\x7C;dmatch\x7C;\x20;rules run-a-thunk ppat pkg ) => (dmatch )
(define-syntax dmatch
  (syntax-rules ()
    [(_ v (e ...) ...)
     (let ([pkg* (dmatch-remexp v (e ...) ...)])
       (run-a-thunk 'v v #f pkg*))]
    [(_ v name (e ...) ...)
     (let ([pkg* (dmatch-remexp v (e ...) ...)])
       (run-a-thunk 'v v 'name pkg*))]))
(implicit-exports #t)
(define-syntax dmatch-remexp
  (syntax-rules ()
    [(_ (rator rand ...) cls ...)
     (let ([v (rator rand ...)])
       (dmatch-aux v cls ...))]
    [(_ v cls ...) (dmatch-aux v cls ...)]))
(define-syntax dmatch-aux
  (syntax-rules (guard)
    [(_ v) '()]
    [(_ v (pat (guard g ...) e0 e ...) cs ...)
     (let ([fk (lambda () (dmatch-aux v cs ...))])
       (ppat v pat
         (if (not (and g ...))
             (fk)
             (cons (pkg '(pat (guard g ...) e0 e ...) (lambda () e0 e ...)) 
		   (fk)))
         (fk)))]
    [(_ v (pat e0 e ...) cs ...)
     (let ([fk (lambda () (dmatch-aux v cs ...))])
       (ppat v pat
         (cons (pkg '(pat e0 e ...) (lambda () e0 e ...)) 
	       (fk))
         (fk)))]))

)

(module (dmatch guard unquote)
  (import (chezscheme))
  (implicit-exports #t)
  Package\x20;procedures
  Error\x20;handling\x20;procedures
  Helper\x20;procedures
  Helper\x20;macros
  Main\x20;\x7C;dmatch\x7C;\x20;rules)
