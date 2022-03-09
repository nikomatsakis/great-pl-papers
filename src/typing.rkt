#lang racket
(require redex/reduction-semantics "grammar.rkt")
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
   (constrain Env_fresh Ty_f (Ty_a -> Id_fresh) Env_out)
   ---------------
   (has-type Env (Expr_f Expr_a) Env_out Id_fresh)
   ]

  [(has-type Env Expr_o Env_o Ty_o)
   (where/error (Id_fresh Env_fresh) (env-with-fresh-var Env_o))
   (constrain Env_fresh Ty_o (struct ((FieldId Id_fresh))) Env_out)
   ---------------
   (has-type Env (Expr_o -> FieldId) Env_out Id_fresh)
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

(define-judgment-form simple-sub
  ;; Constrains `Ty_0` to be a subtype of `Ty_1`, yielding a
  ;; new environment (`Env_out`) that may contain additional bounds.
  #:mode (constrain I I I O)
  #:contract (constrain Env_in Ty_0 Ty_1 Env_out)

  [---------------
   (constrain Env Int Int Env)
   ]

  [(constrain Env Ty_arg1 Ty_arg0 Env_arg)
   (constrain Env_arg Ty_ret0 Ty_ret1 Env_ret)
   ---------------
   (constrain Env (Ty_arg0 -> Ty_ret0) (Ty_arg1 -> Ty_ret1) Env_ret)
   ]

  [(constrain-fields Env FieldTys_0 FieldTys_1 Env_out)
   ---------------
   (constrain Env (struct FieldTys_0) (struct FieldTys_1) Env_out)
   ]

  [(where (_ ... Ty _ ...)  (env-upper-bounds Env Id))
   ---------------
   (constrain Env Id Ty Env)
   ]

  [(where Env_a (env-with-fresh-bound (Id <= Ty) Env))
   (where/error Tys_lb (env-lower-bounds Env_a Id))
   (constrain-all Env_a Tys_lb (Ty) Env_out)
   ---------------
   (constrain Env Id Ty Env_out)
   ]

  [(where (_ ... Ty _ ...)  (env-lower-bounds Env Id))
   ---------------
   (constrain Env Ty Id Env)
   ]

  [(where Env_a (env-with-fresh-bound (Id >= Ty) Env))
   (where/error Tys_ub (env-upper-bounds Env_a Id))
   (constrain-all Env_a (Ty) Tys_ub Env_out)
   ---------------
   (constrain Env Id Ty Env_out)
   ]

  )

(define-judgment-form simple-sub
  #:mode (constrain-fields I I I O)
  #:contract (constrain-fields Env_in FieldTys_0 FieldTys_1 Env_out)

  [---------------
   (constrain-fields Env () _ Env)]

  [(where (_ ... (Field Ty_1) _ ...) FieldTys_1)
   (constrain Env Ty_0 Ty_1 Env_f)
   (constrain-fields Env_f (FieldTy_rest ...) FieldTys_1 Env_out)
   ---------------
   (constrain-fields Env ((Field Ty_0) FieldTy_rest ...) FieldTys_1 Env_out)]
  )

(define-judgment-form simple-sub
  ;; Constraints each `Ty_0` to be a subtype of each `Ty_1`.
  ;;
  ;; For now assumes one of the lists has length at most 1.
  #:mode (constrain-all I I I O)
  #:contract (constrain-all Env_in Tys_0 Tys_1 Env_out)

  [---------------
   (constrain-all Env () _ Env)]

  [---------------
   (constrain-all Env _ () Env)]

  [(constrain Env Ty_first Ty_1 Env_f)
   (constrain-all Env_f (Ty_rest ...) (Ty_1) Env_out)
   ---------------
   (constrain-all Env (Ty_first Ty_rest ...) (Ty_1) Env_out)]

  [(constrain Env Ty_0 Ty_first Env_f)
   (constrain-all Env_f (Ty_0) (Ty_rest ...) Env_out)
   ---------------
   (constrain-all Env (Ty_0) (Ty_first Ty_rest ...) Env_out)]
  )

(module+ test
  (test-match simple-sub
              Goal
              (term (All ())))
  )