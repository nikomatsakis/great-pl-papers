#lang racket
(require redex/reduction-semantics "grammar.rkt" "env.rkt")
(provide (all-defined-out))

(define-judgment-form simple-sub
  #:mode (has-type I I O O)
  #:contract (has-type Env Expr Env Ty)

  [---------------
   (has-type Env number Env Int)
   ]

  [(where ((_ ... (Id Ty) _ ...) _ ) Env)
   ---------------
   (has-type Env Id Env Ty)
   ]

  [(has-types Env (Expr_f ...) Env_f (Ty_f ...))
   ---------------
   (has-type Env (struct ((FieldId_f Expr_f) ...)) Env_f (struct ((FieldId_f Ty_f) ...)))
   ]

  [(has-type Env Expr_f Env_f Ty_f)
   (has-type Env_f Expr_a Env_a Ty_a)
   (where/error (Id_fresh Env_fresh) (env-with-fresh-var Env_a))
   (where Env_out (constrain Env_fresh (Ty_f <= (Ty_a -> Id_fresh))))
   ---------------
   (has-type Env (Expr_f Expr_a) Env_out Id_fresh)
   ]

  [(has-type Env Expr_o Env_o Ty_o)
   (where/error (Id_fresh Env_fresh) (env-with-fresh-var Env_o))
   (where Env_out (constrain Env_fresh (Ty_o <= (struct ((FieldId Id_fresh))))))
   ---------------
   (has-type Env (Expr_o -> FieldId) Env_out Id_fresh)
   ]

  [(where/error (Id_fresh Env_fresh) (env-with-fresh-var Env))
   (where/error Env_arg (env-with-let-var Env_fresh Id_arg Id_fresh))
   (has-type Env_arg Expr_body Env_body Ty_body)
   (where/error (_ BoundedIds_out) Env_body)
   (where/error (IdTys_out _) Env)
   (where/error Ty_out (Id_fresh -> Ty_body))
   ---------------
   (has-type Env (Lambda Id_arg -> Expr_body) (IdTys_out BoundedIds_out) Ty_out)
   ]
  )

(define-judgment-form simple-sub
  #:mode (has-types I I O O)
  #:contract (has-types Env Exprs Env Tys)

  [---------------
   (has-types Env () Env ())
   ]

  [(has-type Env Expr_0 Env_0 Ty_0)
   (has-types Env (Expr_1 ...) Env (Ty_1 ...))
   ---------------
   (has-types Env (Expr_0 Expr_1 ...) Env (Ty_0 Ty_1 ...))
   ]
  )

(define-metafunction simple-sub
  ;; Constrains `Ty_0` to be a subtype of `Ty_1`, yielding a
  ;; new environment (`Env_out`) that may contain additional bounds.
  constrain : Env_in (Ty_0 <= Ty_1) -> Env_out or Error

  [(constrain Env (Ty <= Ty)) Env]

  [(constrain Env ((Ty_arg0 -> Ty_ret0) <= (Ty_arg1 -> Ty_ret1)))
   (constrain Env_arg (Ty_ret0 <= Ty_ret1) Env_ret)
   (where Env_arg (constrain Env (Ty_arg1 <= Ty_arg0) Env_arg))
   ]

  [(constrain Env ((Ty_arg0 -> Ty_ret0) <= (Ty_arg1 -> Ty_ret1)))
   Error
   (where Error (constrain Env (Ty_arg1 <= Ty_arg0)))
   ]

  [(constrain Env ((struct FieldTys_0) <= (struct FieldTys_1)))
   (constrain-fields Env (FieldTys_0 <= FieldTys_1))
   ]

  [(constrain Env (Id <= Ty))
   Env
   (where (_ ... Ty _ ...)  (env-upper-bounds Env Id))]

  [(constrain Env (Id <= Ty))
   (constrain-all Env_a (Tys_lb <= (Ty)))
   (where/error Env_a (env-with-fresh-bound Env (Id <= Ty)))
   (where/error Tys_lb (env-lower-bounds Env_a Id))
   ]

  [(constrain Env (Ty <= Id))
   Env
   (where (_ ... Ty _ ...)  (env-lower-bounds Env Id))]

  [(constrain Env (Ty <= Id))
   (constrain-all Env_a ((Ty) <= Tys_ub))
   (where/error Env_a (env-with-fresh-bound Env (Id >= Ty)))
   (where/error Tys_ub (env-upper-bounds Env_a Id))]

  [(constrain Env (_ <= _))
   Error]

  )

(define-metafunction simple-sub
  constrain-fields : Env_in (FieldTys_0 <= FieldTys_1) -> Env_out or Error

  [(constrain-fields Env (() <= _))
   Env]

  [(constrain-fields Env (((Field Ty_0) FieldTy_rest ...) <= FieldTys_1))
   (constrain-fields Env_f (FieldTy_rest ...) FieldTys_1)
   (where (_ ... (Field Ty_1) _ ...) FieldTys_1)
   (where Env_out (constrain Env Ty_0 Ty_1 Env_f))
   ]

  [(constrain-fields Env (((Field Ty_0) FieldTy_rest ...) <= FieldTys_1))
   Error]

  )

(define-metafunction simple-sub
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
    (term (env-lower-bounds Env_2 Id_1))
    (term ()))

   (test-equal
    (term (env-upper-bounds Env_2 Id_0))
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
    (term (env-lower-bounds Env_6 Id_3))
    (term ()))

   (test-equal
    (term (env-upper-bounds Env_6 Id_3))
    (term (Id_0)))

   (test-equal
    (term (env-upper-bounds Env_6 Id_0))
    (term (Id_2 Id_1)))

   (test-equal
    (term (env-lower-bounds Env_6 Id_0))
    (term ()))

   (test-equal
    (term (env-lower-bounds Env_6 Id_2))
    (term ()))

   (redex-let*
    simple-sub
    [(Env_7 (term (constrain Env_6 (Int <= Id_0))))
     ]

    (test-equal
     (term (env-lower-bounds Env_7 Id_0))
     (term (Int)))
    (test-equal
     (term (env-lower-bounds Env_7 Id_1))
     (term (Int)))
    (test-equal
     (term (env-lower-bounds Env_7 Id_2))
     (term (Int)))
    (test-equal
     (term (env-lower-bounds Env_7 Id_3))
     (term ()))
    )
   )
  )