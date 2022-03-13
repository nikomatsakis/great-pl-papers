#lang racket
(require redex/reduction-semantics "grammar.rkt" "env.rkt" "level.rkt")
(provide (all-defined-out))

(define-metafunction simple-sub
  type-of-expr : IdTys Env Expr -> (Env Ty)

  [(type-of-expr IdTys Env Expr)
   (Env_out Ty_out)
   (where ((Env_out Ty_out)) ,(judgment-holds (has-type
                                               IdTys
                                               Env
                                               Expr
                                               Env_out
                                               Ty_out)
                                              (Env_out Ty_out)))
   ]
  )

(define-judgment-form simple-sub
  #:mode (has-type I I I O O)
  #:contract (has-type IdTys Env Expr Env Ty)

  [---------------
   (has-type IdTys Env number Env Int)
   ]

  [(where (_ ... (Id Ty) _ ...) IdTys)
   ---------------
   (has-type IdTys Env Id Env Ty)
   ]

  [(has-type IdTys Env Expr_car Env_car Ty_car)
   (has-type IdTys Env Expr_cdr Env_cdr Ty_cdr)
   ---------------
   (has-type IdTys Env (Cons Expr_car Expr_cdr) Env_cdr (Cons Ty_car Ty_cdr))
   ]

  [(has-type IdTys Env Expr_f Env_f Ty_f)
   (has-type IdTys Env_f Expr_a Env_a Ty_a)
   (where/error (Id_fresh Env_fresh) (env-with-fresh-var Env_a))
   (where Env_out (constrain Env_fresh (Ty_f <= (Ty_a -> Id_fresh))))
   ---------------
   (has-type IdTys Env (Expr_f Expr_a) Env_out Id_fresh)
   ]

  [(has-type IdTys Env Expr_o Env_o Ty_o)
   (where/error (Id_car Env_car) (env-with-fresh-var Env_o))
   (where/error (Id_cdr Env_cdr) (env-with-fresh-var Env_car))
   (where Env_out (constrain Env_cdr (Ty_o <= (Cons Id_car Id_cdr))))
   ---------------
   (has-type IdTys Env (Car Expr_o) Env_out Id_car)
   ]

  [(has-type IdTys Env Expr_o Env_o Ty_o)
   (where/error (Id_car Env_car) (env-with-fresh-var Env_o))
   (where/error (Id_cdr Env_cdr) (env-with-fresh-var Env_car))
   (where Env_out (constrain Env_cdr (Ty_o <= (Cons Id_car Id_cdr))))
   ---------------
   (has-type IdTys Env (Cdr Expr_o) Env_out Id_cdr)
   ]

  [(where/error (Id_fresh Env_fresh) (env-with-fresh-var Env))
   (has-type ((Id_arg Id_fresh) IdTy ...) Env_fresh Expr_body Env_body Ty_body)
   (where/error Ty_λ (Id_fresh -> Ty_body))
   ---------------
   (has-type (IdTy ...) Env (λ Id_arg -> Expr_body) Env_body Ty_λ)
   ]

  [(where/error Env_let (env-with-adjusted-level Env +1))
   (where/error (Id_fresh Env_fresh) (env-with-fresh-var Env_let))
   (has-type ((Id Id_fresh) IdTy ...) Env_fresh Expr_body Env_body Ty_body)
   (where/error Env_body-1 (env-with-adjusted-level Env_body -1))
   (has-type ((Id Id_fresh) IdTy ...) Env_body-1 Expr_rest Env_rest Ty_rest)
   ---------------
   (has-type (IdTy ...) Env (Let Id = Expr_body in Expr_rest) Env_rest Ty_rest)
   ]
  )

(define-metafunction simple-sub
  ;; Constrains `Ty_0` to be a subtype of `Ty_1`, yielding a
  ;; new environment (`Env_out`) that may contain additional bounds.
  constrain : Env_in (Ty_0 <= Ty_1) -> Env_out or Error

  [(constrain Env (Ty <= Ty)) Env]

  [(constrain Env ((Ty_arg0 -> Ty_ret0) <= (Ty_arg1 -> Ty_ret1)))
   (constrain Env_arg (Ty_ret0 <= Ty_ret1) Env_ret)
   (where Env_arg (constrain Env (Ty_arg1 <= Ty_arg0)))
   ]

  [(constrain Env ((Ty_arg0 -> Ty_ret0) <= (Ty_arg1 -> Ty_ret1)))
   Error
   (where Error (constrain Env (Ty_arg1 <= Ty_arg0)))
   ]

  [(constrain Env ((Cons Ty_car0 Ty_cdr0) <= (Cons Ty_car1 Ty_cdr1)))
   (constrain Env_car (Ty_cdr0 <= Ty_cdr1))
   (where Env_car (constrain Env (Ty_car0 <= Ty_car0)))
   ]

  [(constrain Env ((Cons Ty_car0 Ty_cdr0) <= (Cons Ty_car1 Ty_cdr1)))
   Error
   (where Error (constrain Env (Ty_car0 <= Ty_car0)))
   ]

  [(constrain Env (Id <= Ty))
   Env
   (where (_ ... Ty _ ...)  (upper-bounds-of-var-in-env Env Id))]

  [(constrain Env (Ty <= Id))
   Env
   (where (_ ... Ty _ ...)  (lower-bounds-of-var-in-env Env Id))]

  [(constrain Env (Id <= Ty))
   (constrain-all Env_a (Tys_lb <= (Ty)))
   (where/error Level_id (level-of-var-in-env Env Id))
   (where/error Level_ty (level-of-ty Env Ty))
   (where #t (level-at-or-below Level_ty Level_id))
   (where/error Env_a (env-with-fresh-bound Env (Id <= Ty)))
   (where/error Tys_lb (lower-bounds-of-var-in-env Env_a Id))
   ]

  [(constrain Env (Ty <= Id))
   (constrain-all Env_a ((Ty) <= Tys_ub))
   (where/error Level_id (level-of-var-in-env Env Id))
   (where/error Level_ty (level-of-ty Env Ty))
   (where #t (level-at-or-below Level_ty Level_id))
   (where/error Env_a (env-with-fresh-bound Env (Id >= Ty)))
   (where/error Tys_ub (upper-bounds-of-var-in-env Env_a Id))
   ]

  [(constrain Env (Id <= Ty))
   (constrain Env_out (Id <= Ty_extruded))

   ; In case of a level mismatch, extrude with polarity `-` --
   ; that means that `Ty` is a "input", so we get "some subtype 
   ; of Ty" back (but at a suitable level).
   (where/error Level_id (level-of-var-in-env Env Id))
   (where/error Level_ty (level-of-ty Env Ty))
   (where #f (level-at-or-below Level_ty Level_id))
   (where (Ty_extruded Env_out) (extrude Env Level_id - Ty))
   ]

  [(constrain Env (Ty <= Id))
   (constrain Env_out (Ty_extruded <= Id))

   ; In case of a level mismatch, extrude with polarity `+` --
   ; that means that `Ty` is an "output", so we get "some
   ; supertype of Ty" back (but at a suitable level).
   (where/error Level_id (level-of-var-in-env Env Id))
   (where/error Level_ty (level-of-ty Env Ty))
   (where #f (level-at-or-below Level_ty Level_id))
   (where (Ty_extruded Env_out) (extrude Env Level_id + Ty))
   ]

  [(constrain Env (_ <= _))
   Error]

  )

(define-metafunction simple-sub
  ;; Constrain the `i`th type in `Tys_0` with the `i`th type
  ;; in `Tys_1`. `Tys_1` may be longer than `Tys_0`, in which case
  ;; any extra types are ignored.
  constrain-zip : Env_in (Tys_0 <= Tys_1) -> Env_out or Error

  [(constrain-zip Env (() <= _))
   Env]

  [(constrain-zip Env ((Ty_0a Ty_0b ...) <= (Ty_1a Ty_1b ...)))
   (constrain-zip Env_a ((Ty_0b ...) <= (Ty_1b ...)))
   (where Env_a (constrain Env (Ty_0a <= Ty_1a)))
   ]

  [(constrain-zip Env (_ <= _))
   Error]

  )

(define-metafunction simple-sub
  ;; Constrain each type in `Tys_0` to be <= each type in
  ;; `Tys_1`. Right now only works if either `Tys_0` or `Tys_1`
  ;; is of length 1, but that's because I am lazy.
  constrain-all : Env_in (Tys_0 <= Tys_1) -> Env or Error

  [(constrain-all Env (() <= _)) Env]

  [(constrain-all Env (_ <= ())) Env]

  [(constrain-all Env ((Ty_first Ty_rest ...) <= (Ty_1)))
   (constrain-all Env_f ((Ty_rest ...) <= (Ty_1)))
   (where Env_f (constrain Env (Ty_first <= Ty_1)))
   ]

  [(constrain-all Env ((Ty_0) <= (Ty_first Ty_rest ...)))
   (constrain-all Env_f ((Ty_0) <= (Ty_rest ...)))
   (where Env_f (constrain Env (Ty_0 <= Ty_first)))
   ]

  [(constrain-all Env (_ <= _))
   Error]
  )

(module+ test

  (redex-let*
   simple-sub
   [(Env (term EmptyEnv))]
   (test-equal
    (judgment-holds (has-type
                     ()
                     Env
                     22
                     Env_out
                     Ty_out)
                    (Env_out Ty_out))
    (term ((Env Int))))
   )

  (redex-let*
   simple-sub
   [((Id_0 Env_0) (term (env-with-fresh-var EmptyEnv)))
    ((Id_1 Env_1) (term (env-with-fresh-var Env_0)))
    (Env_2 (term (constrain Env_1 (Id_0 <= Id_1))))
    ]
   (test-equal
    (term (lower-bounds-of-var-in-env Env_2 Id_1))
    (term ()))

   (test-equal
    (term (upper-bounds-of-var-in-env Env_2 Id_0))
    (term (Id_1)))
   )

  (redex-let*
   simple-sub
   [((Id_0 Env_0) (term (env-with-fresh-var EmptyEnv)))
    ((Id_1 Env_1) (term (env-with-fresh-var Env_0)))
    ((Id_2 Env_2) (term (env-with-fresh-var Env_1)))
    ((Id_3 Env_3) (term (env-with-fresh-var Env_2)))
    (Env_4 (term (constrain Env_3 (Id_0 <= Id_1))))
    (Env_5 (term (constrain Env_4 (Id_0 <= Id_2))))
    (Env_6 (term (constrain Env_5 (Id_3 <= Id_0))))
    ]
   (test-equal
    (term (lower-bounds-of-var-in-env Env_6 Id_3))
    (term ()))

   (test-equal
    (term (upper-bounds-of-var-in-env Env_6 Id_3))
    (term (Id_0)))

   (test-equal
    (term (upper-bounds-of-var-in-env Env_6 Id_0))
    (term (Id_2 Id_1)))

   (test-equal
    (term (lower-bounds-of-var-in-env Env_6 Id_0))
    (term ()))

   (test-equal
    (term (lower-bounds-of-var-in-env Env_6 Id_2))
    (term ()))

   (redex-let*
    simple-sub
    [(Env_7 (term (constrain Env_6 (Int <= Id_0))))
     ]

    (test-equal
     (term (lower-bounds-of-var-in-env Env_7 Id_0))
     (term (Int)))
    (test-equal
     (term (lower-bounds-of-var-in-env Env_7 Id_1))
     (term (Int)))
    (test-equal
     (term (lower-bounds-of-var-in-env Env_7 Id_2))
     (term (Int)))
    (test-equal
     (term (lower-bounds-of-var-in-env Env_7 Id_3))
     (term ()))
    )
   )
  )