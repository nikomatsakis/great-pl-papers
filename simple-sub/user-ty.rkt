#lang racket
(require redex/reduction-semantics "grammar.rkt" "env.rkt" "typing.rkt")
(provide coalesce-ty)

(define-metafunction simple-sub
  coalesce-ty : Env Ty -> UserTy

  [(coalesce-ty Env Ty)
   UserTy
   (where/error (UserTy _) (-go Env Ty + () ()))]
  )

(define-metafunction simple-sub
  -go : Env Ty Polarity PolarIds PolarIdVars -> (UserTy PolarIdVars)

  [(-go Env Int _ _ PolarIdVars) (Int PolarIdVars)]

  [(-go Env (Ty_arg -> Ty_ret) Polarity PolarIds PolarIdVars)
   ((UserTy_arg -> UserTy_ret) PolarIdVars_ret)
   (where/error (UserTy_arg PolarIdVars_arg) (-go Env Ty_arg (invert-polarity Polarity) PolarIds PolarIdVars))
   (where/error (UserTy_ret PolarIdVars_ret) (-go Env Ty_ret Polarity PolarIds PolarIdVars_arg))]

  [(-go Env (Cons Ty_car Ty_cdr) Polarity PolarIds PolarIdVars)
   ((Cons UserTy_car UserTy_cdr) PolarIdVars_cdr)
   (where/error (UserTy_car PolarIdVars_car) (-go Env Ty_car Polarity PolarIds PolarIdVars))
   (where/error (UserTy_cdr PolarIdVars_cdr) (-go Env Ty_cdr Polarity PolarIds PolarIdVars_car))]

  [; type variable, recursive case: check polar-id-vars to see if it is present already.
   ; if so, return that variable.
   (-go Env Id Polarity PolarIds PolarIdVars)
   (Id_μ PolarIdVars)
   (where (_ ... (Polarity Id) _ ...) PolarIds)
   (where (_ ... ((Polarity Id) Id_μ) _ ...) PolarIdVars)
   ]

  [; type variable, recursive case: check polar-id-vars to see if it is present already.
   ; if not, create a fresh variale.
   (-go Env Id Polarity PolarIds PolarIdVars)
   (Id_fresh PolarIdVars_fresh)
   (where (_ ... (Polarity Id) _ ...) PolarIds)
   (where/error Id_fresh (fresh-var (Env PolarIdVars)))
   (where/error (PolarIdVar ...) PolarIdVars)
   (where/error PolarIdVars_fresh (((Polarity Id) Id_fresh) (PolarIdVar ...)))
   ]

  [; type variable, not recursive case: check if there are no bounds for this variable.
   ; If so, generate a fresh variable.
   (-go Env Id Polarity (PolarId ...) PolarIdVars)
   (Id PolarIdVars)
   (where () (appropriate-bounds-of-var-in-env Env Id Polarity))
   (where/error Id_fresh (fresh-var (Env PolarIdVars)))
   ]

  [; type variable, not recursive case: if there are bounds for this variable, then we recurse
   ; on those bounds. If those bounds wind up referencing this same variable with same polarity,
   ; we make this a μ type -- we *could* remove `(Polarity Id)` from the map at this point, I believe,
   ; but we don't, which is just because I am lazy (also because the paper doesn't).
   (-go Env Id Polarity (PolarId ...) PolarIdVars)
   (UserTy_r PolarIdVars_b)
   (where/error Tys_b (appropriate-bounds-of-var-in-env Env Id Polarity))
   (where/error PolarIds_1 ((Polarity Id) PolarId ...))
   (where/error (UserTys_b PolarIdVars_b) (-go-fold Env Tys_b Polarity PolarIds_1 PolarIdVars))
   (where/error UserTy_m (-merge Polarity UserTys_b))
   (where/error UserTy_r (-maybe-recursive PolarIdVars (Polarity Id) UserTy_m))
   ]

  )

(define-metafunction simple-sub
  -go-fold : Env Tys Polarity PolarIds PolarIdVars -> (UserTys PolarIdVars)

  [(-go-fold Env () Polarity PolarIds PolarIdVars)
   (() PolarIdVars)]

  [(-go-fold Env (UserTy_f UserTy_r ...) Polarity PolarIds PolarIdVars)
   ((UserTy_f0 UserTy_r0 ...) PolarIdVars_r)
   (where/error (UserTy_f0 PolarIdVars_f) (-go Env UserTy_f Polarity PolarIds PolarIdVars))
   (where/error ((UserTy_r0 ...) PolarIdVars_r) (-go-fold Env (UserTy_r ...) Polarity PolarIds PolarIdVars_f))]

  )


(define-metafunction simple-sub
  -merge : Polarity UserTys -> UserTy

  [(-merge Polarity (UserTy)) UserTy]

  [(-merge Polarity (UserTy_0 UserTy_r ...))
   (-set-op-tys Polarity UserTy_0 UserTy_1)
   (where/error UserTy_1 (-merge Polarity (UserTy_r ...)))]

  )

(define-metafunction simple-sub
  ;; Returns an appropriate type to combine T0 and T1 based on the polarity:
  ;;
  ;; `+`, or function output, these are two possibilities, so use union.
  ;;
  ;; `-`, or function input, these are two constraints it must meet, so use intersection
  ;; (the value must be both Ty0 and Ty1 at same time).
  -set-op-tys : Polarity UserTy UserTy -> UserTy

  [(-set-op-tys + UserTy_0 UserTy_1)
   (⨆ UserTy_0 UserTy_1)]

  [(-set-op-tys - UserTy_0 UserTy_1)
   (⊓ UserTy_0 UserTy_1)]

  )

(define-metafunction simple-sub
  -maybe-recursive : PolarIdVars PolarId UserTy -> UserTy

  [(-maybe-recursive (PolarIdVar_0 ... (PolarId Id_μ) PolarIdVar_1 ...) PolarId UserTy)
   (μ Id_μ UserTy)]

  [(-maybe-recursive _ PolarId UserTy) UserTy]

  )
