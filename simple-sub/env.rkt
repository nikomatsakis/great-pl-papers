#lang racket
(require redex/reduction-semantics racket/set "grammar.rkt")
(provide (all-defined-out))

(define-term EmptyEnv ((L 0) ()))

(define-metafunction simple-sub
  ;; introduce (and return) a sequence of fresh type variables with no bounds
  env-with-fresh-vars : Env number -> (Ids Env)

  [(env-with-fresh-vars Env 0) (() Env)]

  [(env-with-fresh-vars Env number_n)
   ((Id_n-1 ... Id_n) Env_n)
   (where/error number_n-1 ,(- (term number_n) 1))
   (where/error ((Id_n-1 ...) Env_n-1) (env-with-fresh-vars Env number_n-1))
   (where/error (Id_n Env_n) (env-with-fresh-var Env_n-1))]
  )

(define-metafunction simple-sub
  ;; introduce (and return) a fresh type variable with no bounds
  env-with-fresh-var : Env -> (Id Env)

  [(env-with-fresh-var Env)
   (Id_fresh (Level ((Id_fresh Level () ()) TyVarDef ...)))
   (where/error Id_fresh (fresh-var Env))
   (where/error (Level (TyVarDef ...)) Env)
   ]
  )

(define-metafunction simple-sub
  ;; introduce (and return) a fresh type variable with no bounds
  fresh-var : any -> Id

  [(fresh-var any)
   ,(variable-not-in (term any) 'X)]
  )

(define-metafunction simple-sub
  ty-var-def : Env Id -> TyVarDef

  [(ty-var-def Env Id)
   (Id Level Tys_lower Tys_upper)
   (where/error (_ (_ ... (Id Level Tys_lower Tys_upper) _ ...)) Env)
   ]
  )

(define-metafunction simple-sub
  env-upper-bounds : Env Id -> Tys

  [(env-upper-bounds Env Id)
   Tys_upper
   (where/error (Id _ _ Tys_upper) (ty-var-def Env Id))
   ]
  )

(define-metafunction simple-sub
  env-lower-bounds : Env Id -> Tys

  [(env-lower-bounds Env Id)
   Tys_lower
   (where/error (Id _ Tys_lower _) (ty-var-def Env Id))
   ]
  )

(define-metafunction simple-sub
  env-polar-bounds : Env Id Polarity -> Tys

  [(env-polar-bounds Env Id +) (env-lower-bounds Env Id)]
  [(env-polar-bounds Env Id -) (env-upper-bounds Env Id)]
  )

(define-metafunction simple-sub
  env-with-fresh-bound : Env Bound -> Env or ()

  [(env-with-fresh-bound Env (Id <= Ty))
   (Level_env (TyVarDef_0 ... (Id Level_id Tys_lower Tys_upper1) TyVarDef_1 ...))

   (where/error (Level_env (TyVarDef_0 ... (Id Level_id Tys_lower Tys_upper0) TyVarDef_1 ...)) Env)
   (side-condition (term (pretty-print (term ("env-with-fresh-bound" (Id <= Ty) Tys_upper0)))))
   (where/error Tys_upper1 ,(set-add (term Tys_upper0) (term Ty)))
   (where #f (equal-terms Tys_upper0 Tys_upper1))
   ]

  [(env-with-fresh-bound Env (Id >= Ty))
   (Level_env (TyVarDef_0 ... (Id Level_id Tys_lower1 Tys_upper) TyVarDef_1 ...))

   (where/error (Level_env (TyVarDef_0 ... (Id Level_id Tys_lower0 Tys_upper) TyVarDef_1 ...)) Env)
   (side-condition (term (pretty-print (term ("env-with-fresh-bound" (Ty <= Id) Tys_lower0)))))
   (where/error Tys_lower1 ,(set-add (term Tys_lower0) (term Ty)))
   (where #f (equal-terms Tys_lower0 Tys_lower1))
   ]

  [(env-with-fresh-bound Env Bound) ()]

  )

(define-metafunction simple-sub
  equal-terms : any any -> boolean

  [(equal-terms any any) #t]
  [(equal-terms _ _) #f]
  )

(module+ test

  (redex-let*
   simple-sub
   [(Env_e (term EmptyEnv))
    ((Id_0 Env_0) (term (env-with-fresh-var Env_e)))
    ((Id_1 Env_1) (term (env-with-fresh-var Env_0)))]

   (test-equal (term (env-upper-bounds Env_1 Id_0))
               (term ()))

   (test-equal (term (env-lower-bounds Env_1 Id_1))
               (term ()))
   )

  (redex-let*
   simple-sub
   [(Env_e (term EmptyEnv))
    ((Id_0 Env_0) (term (env-with-fresh-var Env_e)))
    (Env_1 (term (env-with-fresh-bound Env_0 (Id_0 <= int))))
    (() (term (env-with-fresh-bound Env_1 (Id_0 <= int))))]

   (test-equal (term (env-upper-bounds Env_1 Id_0))
               (term (int)))

   (test-equal (term (env-lower-bounds Env_1 Id_0))
               (term ()))
   )

  )