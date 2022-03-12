#lang racket
(require redex/reduction-semantics "grammar.rkt" "env.rkt")
(provide (all-defined-out))

(define-metafunction simple-sub
  level-of-ty : Env Ty -> Level

  [(level-of-ty Env Int)
   (L 0)]

  [(level-of-ty Env (Ty_arg -> Ty_ret))
   (min-level (level-of-ty Env Ty_arg) (level-of-ty Env Ty_ret))]

  [(level-of-ty Env (Tuple (Ty ...)))
   (min-level Level_ty ...)
   (where (Level_ty ...) ((level-of-ty Env Ty) ...))]

  [(level-of-ty Env Id)
   (level-of-var-in-env Env Id)]
  )

(define-metafunction simple-sub
  min-level : Level ... -> Level

  [(min-level (L number) ...)
   (L number_min)
   (where number_min ,(apply min (term (number ...))))
   ]
  )

(module+ test

  (test-equal (term (level-of-ty EmptyEnv Int)) (term (L 0)))

  (redex-let*
   simple-sub
   [((Id_x Env) (term (env-with-fresh-var EmptyEnv)))]
   (test-equal (term (level-of-ty Env (Tuple (Int Id_x (Id_x -> Int)))))
               (term (L 0))))

  )