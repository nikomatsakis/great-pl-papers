#lang racket
(require redex/reduction-semantics "grammar.rkt" "env.rkt" "typing.rkt")

(module+ test
  (redex-let*
   simple-sub
   [(Env (term EmptyEnv))
    (Expr_test (term (Lambda f -> (Lambda x -> (f (f x))))))]
   (test-equal
    (judgment-holds (has-type
                     Env
                     Expr_test
                     Env_out
                     Ty_out)
                    (Env_out Ty_out))
    (term (((()
             ((X3 () ())
              (X2 () ())
              (X1 () ())
              (X () ((X2 -> X3) (X1 -> X2)))))
            (X -> (X1 -> X3))))))
   )
  )