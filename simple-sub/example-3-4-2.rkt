#lang racket
(require redex/reduction-semantics "grammar.rkt" "env.rkt" "typing.rkt" "user-ty.rkt")

(module+ test
  (redex-let*
   simple-sub
   [(Env (term EmptyEnv))
    (Expr_test (term (Lambda f -> (Lambda x -> (f (f x))))))
    ((Env_test Ty_test) (term (type-of-expr Env Expr_test)))]
   (test-equal (term Env_test)
               (term (()
                      ((X3 () ())
                       (X2 () ())
                       (X1 () ())
                       (X () ((X2 -> X3) (X1 -> X2)))))))
   (test-equal (term Ty_test)
               (term (X -> (X1 -> X3))))
   (current-traced-metafunctions '(coalesce-ty -go))
   (test-equal (term (coalesce-ty Env_test Ty_test))
               (term ((⊓ (X2 -> X3) (X1 -> X2)) -> (X1 -> X3))))
   )
  )