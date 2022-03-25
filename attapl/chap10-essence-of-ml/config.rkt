#lang racket
(require redex/reduction-semantics "grammar.rkt" racket/set)
(provide )

(define-metafunction ml
  closed-configuration? : Configuration -> boolean

  [(closed-configuration? (t / μ))
   ,(subset? (term Xs) (term Ms))
   (where/error Xs (free-variables t))
   (where/error Ms (heap-domain μ))
   ]
  )

(define-metafunction ml
  heap-domain : μ -> Ms

  [(heap-domain ((M v) ...)) (M ...)]
  )

(define-metafunction ml
  free-variables : Term -> (X ...)

  [(free-variables z) (z)]
  [(free-variables M) (M)]

  [; don't consider constants "free" (even though they technically are...
   (free-variables C) ()]

  [(free-variables (λ z t))
   ,(set-subtract (term (z_free-in-t ...)) (term (z)))
   (where/error (z_free-in-t ...) (free-variables t))]

  [(free-variables (let z = t_1 in t_2))
   ,(set-union
     (term (z_free-in-T1 ...))
     (set-subtract (term (z_free-in-T2 ...)) (term (z)))
     )
   (where/error (z_free-in-T1 ...) (free-variables t_1))
   (where/error (z_free-in-T2 ...) (free-variables t_2))
   ]

  [(free-variables (Term ...))
   ,(apply set-union (term ((z ...) ...)))
   (where/error ((z ...) ...) ((free-variables Term) ...))]

  )

(module+ test

  (redex-let*
   ml
   ((t (term (let inc = (λ y ((+ 1) y)) in (other-fn1 (other-fn2 (inc 21)))))))
   (test-equal
    (term (free-variables t))
    (term (other-fn2 other-fn1)))
   (test-equal
    (term (closed-configuration? (t / ())))
    #f)
   (test-equal
    (term (closed-configuration? (t / ((other-fn2 22) (other-fn1 23)))))
    #t)
   )
  )