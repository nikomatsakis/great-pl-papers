#lang racket
(require redex/reduction-semantics racket/set "grammar.rkt")
(provide (all-defined-out))

(define-term EmptyEnv ((L 0) ()))

(define-metafunction simple-sub
  ;; return an environment with an incremented level counter
  env-with-adjusted-level : Env number -> Env

  [(env-with-adjusted-level Env number_offset)
   ((L number_next) TyVarDefs)
   (where/error ((L number) TyVarDefs) Env)
   (where/error number_next ,(+ (term number_offset) (term number)))]

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
  env-with-fresh-var-in-level : Env Level -> (Id Env)

  [(env-with-fresh-var-in-level Env Level_var)
   (Id_fresh (Level_env ((Id_fresh Level_var () ()) TyVarDef ...)))
   (where/error Id_fresh (fresh-var Env))
   (where/error (Level_env (TyVarDef ...)) Env)
   ]
  )

(define-metafunction simple-sub
  ;; introduce (and return) a fresh type variable with no bounds
  fresh-var : any -> Id

  [(fresh-var any)
   ,(variable-not-in (term any) 'X)]
  )

(define-metafunction simple-sub
  var-def-in-env : Env Id -> TyVarDef

  [(var-def-in-env Env Id)
   (Id Level Tys_lower Tys_upper)
   (where/error (_ (_ ... (Id Level Tys_lower Tys_upper) _ ...)) Env)
   ]
  )

(define-metafunction simple-sub
  level-of-var-in-env : Env Id -> Level

  [(level-of-var-in-env Env Id)
   Level
   (where/error (Id Level _ _) (var-def-in-env Env Id))
   ]
  )

(define-metafunction simple-sub
  upper-bounds-of-var-in-env : Env Id -> Tys

  [(upper-bounds-of-var-in-env Env Id)
   Tys_upper
   (where/error (Id _ _ Tys_upper) (var-def-in-env Env Id))
   ]
  )

(define-metafunction simple-sub
  lower-bounds-of-var-in-env : Env Id -> Tys

  [(lower-bounds-of-var-in-env Env Id)
   Tys_lower
   (where/error (Id _ Tys_lower _) (var-def-in-env Env Id))
   ]
  )

(define-metafunction simple-sub
  ;; Relate Id and Ty in a way that is "appropriate" for the given polarity.
  appropriate-bound : Polarity Id Ty -> Bound

  [; Polarity + context: We are producing an `Id` from a `Ty`, so `Id`
   ; must be a supertype of `Ty`.
   (appropriate-bound + Id Ty) (Id >= Ty)]

  [; Polarity - context: We are storing an `Id` into a `Ty`, so `Id`
   ; must be a subtype of `Ty`.
   (appropriate-bound - Id Ty) (Id <= Ty)]
  )

(define-metafunction simple-sub
  ;; For polarity +, returns the lower bounds of `Id` (i.e., Id is a supertype of those things).
  ;;
  ;; For polarity -, returns the upper bounds of `Id` (i.e., Id is a subtype of those things).
  ;;
  ;; Why are these the "appropriate" bounds? They are the ones useful for representing `Id`
  ;; in a context of the given polarity.
  ;;
  ;; Example: Polarity `+` is the "return value" from a function, i.e., a generated value.
  ;; In this case, you represent `Id` as the *union of its lower bounds*, since those are the
  ;; values it could have come from.
  ;;
  ;; Polarity `-` is the input to a function. In that case, you represent it as the *intersection*
  ;; of its *upper bounds*, i.e., the callee must meet all of those bounds.
  appropriate-bounds-of-var-in-env : Env Id Polarity -> Tys

  [(appropriate-bounds-of-var-in-env Env Id +) (lower-bounds-of-var-in-env Env Id)]
  [(appropriate-bounds-of-var-in-env Env Id -) (upper-bounds-of-var-in-env Env Id)]
  )


(define-metafunction simple-sub
  ;; Add the given bound to the given variable.
  ;;
  ;; `()` if such a bound already exists.
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

   (test-equal (term (upper-bounds-of-var-in-env Env_1 Id_0))
               (term ()))

   (test-equal (term (lower-bounds-of-var-in-env Env_1 Id_1))
               (term ()))
   )

  (redex-let*
   simple-sub
   [(Env_e (term EmptyEnv))
    ((Id_0 Env_0) (term (env-with-fresh-var Env_e)))
    (Env_1 (term (env-with-fresh-bound Env_0 (Id_0 <= int))))
    (() (term (env-with-fresh-bound Env_1 (Id_0 <= int))))]

   (test-equal (term (upper-bounds-of-var-in-env Env_1 Id_0))
               (term (int)))

   (test-equal (term (lower-bounds-of-var-in-env Env_1 Id_0))
               (term ()))
   )

  )