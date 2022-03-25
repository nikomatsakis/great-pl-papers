#lang racket
(require redex/reduction-semantics "grammar.rkt")
(provide fresh-substitution
         )

(define-metafunction ml
  instantiate-with-fresh : (Xs Term_bound-in) Term_fresh-in -> (Xs Term)

  ((instantiate-with-fresh ((X ...) Term_bound-in) Term_fresh-in)
   ((X_fresh ...) (apply-substitution ((X X_fresh) ...) Term_bound-in))
   (where/error (X_fresh ...) ,(variables-not-in Term_fresh-in (X ...)))
   )
  )

(define-metafunction ml
  fresh-substitution : Xs Term_fresh-in -> θ

  ((fresh-substitution (X ...) Term_fresh-in)
   ((X X_fresh) ...)
   (where/error (X_fresh ...) ,(variables-not-in Term_fresh-in (X ...)))
   )
  )

(define-metafunction ml
  apply-substitution : θ Term -> Term

  ((apply-substitution () Term) Term)

  ((apply-substitution ((X T) (X_1 T_1) ...) Term)
   (apply-substitution ((X_1 T_1) ...) (substitute Term X T))
   )
  )

