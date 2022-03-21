#lang racket
(require redex/reduction-semantics "grammar.rkt" racket/set)
(provide )

(define-metafunction ml
  is-closed-configuration : Configuration -> boolean
  )

(define-metafunction ml
  free-variables : Term -> (X ...)

  [(free-variables Z) (Z)]
  [(free-variables M) (M)]

  [; don't consider constants "free" (even though they technically are...
   (free-variables C) ()]

  [(free-variables (λ Z T))
   ,(set-subtract (term (Z_free-in-T ...)) (term (Z)))
   (where/error (Z_free-in-T ...) (free-variables T))]

  [(free-variables (let Z = T_1 in T_2))
   ,(set-union
     (term (Z_free-in-T1 ...))
     (set-subtract (term (Z_free-in-T2 ...)) (term (Z)))
     )
   (where/error (Z_free-in-T1 ...) (free-variables T_1))
   (where/error (Z_free-in-T2 ...) (free-variables T_2))
   ]

  [(free-variables (Term ...))
   ,(apply set-union (term ((Z ...) ...)))
   (where/error ((Z ...) ...) ((free-variables Term) ...))]

  )

(module+ test

  (redex-let*
   ml
   ((T (term (let inc = (λ y ((+ 1) y)) in (other-fn1 (other-fn2 (inc 21)))))))
   (test-equal
    (term (free-variables T))
    (term (other-fn2 other-fn1)))
   )
  )