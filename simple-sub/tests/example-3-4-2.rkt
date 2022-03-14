#lang racket
(require redex/reduction-semantics "../grammar.rkt" "../env.rkt" "../typing.rkt" "../user-ty.rkt")

(module+ test
  (redex-let*
   simple-sub
   [(Env (term EmptyEnv))
    (Expr_test (term (λ f -> (λ x -> (f (f x))))))
    ((Env_test Ty_test) (term (type-of-expr () Env Expr_test)))]
   (test-equal (term Env_test)
               (term ((L 0)
                      ((X3 (L 0) () ())
                       (X2 (L 0) () ())
                       (X1 (L 0) () ())
                       (X (L 0) () ((X2 -> X3) (X1 -> X2)))))))
   (test-equal (term Ty_test)
               (term (X -> (X1 -> X3))))
   (test-equal (term (coalesce-ty Env_test Ty_test))
               (term ((⊓ (X2 -> X3) (X1 -> X2)) -> (X1 -> X3))))
   )
  )