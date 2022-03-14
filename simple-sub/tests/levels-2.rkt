#lang racket
(require redex/reduction-semantics "../grammar.rkt" "../env.rkt" "../typing.rkt" "../user-ty.rkt")

(module+ test
  (redex-let*
   simple-sub
   [(Env (term EmptyEnv))
    (Expr_test (term (let pair = (λ f -> (Cons 22 f)) in pair)))
    ((Env_test Ty_test) (term (type-of-expr () Env Expr_test)))
    ]
   (test-equal (term (coalesce-ty Env_test Ty_test))
               (term (X1 -> (Cons Int X1))))
   )
  )