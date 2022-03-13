#lang racket
(require redex/reduction-semantics "grammar.rkt" "env.rkt")
(provide level-of-ty
         extrude
         instantiate-above
         level-at-or-below)

(define-metafunction simple-sub
  level-of-ty : Env Ty -> Level

  [(level-of-ty Env Int)
   (L 0)]

  [(level-of-ty Env (Ty_arg -> Ty_ret))
   (min-level (level-of-ty Env Ty_arg) (level-of-ty Env Ty_ret))]

  [(level-of-ty Env (Cons Ty_car Ty_cdr))
   (min-level (level-of-ty Env Ty_car) (level-of-ty Env Ty_cdr))]

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

(define-metafunction simple-sub
  level-at-or-below : Level_1 Level_2 -> boolean

  [(level-at-or-below (L number_1) (L number_2))
   ,(<= (term number_1) (term number_2))]
  )

(define-metafunction simple-sub
  level-below : Level_1 Level_2 -> boolean

  [(level-below (L number_1) (L number_2))
   ,(< (term number_1) (term number_2))]
  )

(define-metafunction simple-sub
  extrude : Env Level_in Polarity Ty -> (Env_out Ty_out)
  #:post (level-at-or-below (level-of-ty Env_out Ty_out) Level_in)

  [(extrude Env Level Polarity Ty)
   (extrude-ty Env () Level Polarity Ty)]
  )

(define-metafunction simple-sub
  ;; Extrude a given type so that it is at-or-above the given level.
  ;;
  ;; `IdPairs` represents the stack of "in progress" extrusions.
  extrude-ty : Env IdPairs Level_in Polarity Ty -> (Env_out Ty_out)
  #:post (level-at-or-below (level-of-ty Env_out Ty_out) Level_in)

  ;; uninteresting cases: just perform structural recursion

  [(extrude-ty Env IdPairs Level Polarity Int) (Env Int)]

  [(extrude-ty Env IdPairs Level Polarity (Ty_arg -> Ty_ret))
   (Env_ret (Ty_arg_extruded -> Ty_ret_extruded))
   (where/error (Env_arg Ty_arg_extruded) (extrude-ty Env IdPairs Level (invert-polarity Polarity) Ty_arg))
   (where/error (Env_ret Ty_ret_extruded) (extrude-ty Env_arg IdPairs Level Polarity Ty_ret))
   ]

  [(extrude-ty Env IdPairs Level Polarity (Cons Ty_car Ty_cdr))
   (Env_cdr (Cons Ty_car_extruded Ty_cdr_extruded))
   (where/error (Env_car Ty_car_extruded) (extrude-ty Env IdPairs Level Polarity Ty_car))
   (where/error (Env_cdr Ty_cdr_extruded) (extrude-ty Env_car IdPairs Level Polarity Ty_cdr))
   ]

  ;; variables

  ; detect cyclic types and re-use the new variable
  [(extrude-ty Env (_ ... (Id Id_extruded) _ ...) _ _ Id) (Env Id_extruded)]

  ; variables at-or-below our target level can just be copied over
  [(extrude-ty Env IdPairs Level Polarity Id)
   (Env Id)
   (where/error Level_id (level-of-var-in-env Env Id))
   (where #t (level-at-or-below Level_id Level))
   ]

  ; otherwise, we have a variable Id in level Lj with bounds like this
  ;
  ;                >=                <=
  ;   Ty_lb ... <------ Id (in Lj) ------> Ty_ub ...
  ;
  ; with polarity +, we want some supertype of `Id`, so we wish to produce
  ; an `Id1` in level Li < Lj with bounds like
  ;
  ;                >=                <=
  ;   Ty_lb ... <------ Id (in Lj) ------> Ty_ub ...
  ;     :                 |
  ;     :                 | <=
  ;     v           >=    v
  ;   Ty_lb1 ... <---- Id1 (in Li)
  ;
  ; Here, `Ty_lb1 ...` are extruded from `Ty_lb ...`. Since we are in a polarity +
  ; context, we are (effectively) outputting an `Id`, so we give back to the caller some
  ; **supertype** `Id1` of `Id` which shares `Id`'s lower bounds (i.e., which could have come
  ; from all the same values).
  ;
  ; With polarity `-`, it is reversed: we give back a *subtype* `Id1` of `Id` that shares `Id`'s
  ; upper bounds (i.e., which must meet the same requirements).
  ;
  ; Note: there is a kind of subtle point here. More bounds on `Id` can be added
  ; to the environment after this extrusion is performed. For example, assuming polarity `+`,
  ; one could imagine adding a new lower bound `Id >= Ty_x`. You might think this would
  ; be a problem, since that bound was not extruded for `Id1`. However, this works out ok,
  ; because when we add a new lower bound to `Id`, we also add it to `Id`'s upper bounds,
  ; and `Id1` is now an upper-bound of `Id`. We would therefore propagate the bound (and extrude it
  ; if necessary to make the levels match up).
  ;
  ; What about the inverse? It is possible that we add a constraint on `Id1`, and that will not be
  ; propagated to `Id`, as there is no reverse link. However, if this is a new *lower bound*, it
  ; doesn't matter, because this use of `Id` occurs in a negative polarity. If it's a new *upper bound*
  ; it will be uncovered when we convert to a user-facing type.
  [(extrude-ty Env IdPairs Level Polarity Id)
   (Env_out Id_extruded)

   ; create a new variable `Id_fresh`; the idea will be that the type
   ; `Id` would be assignable to `Id_fresh` with the given polarity
   ; (e.g., if `Polarity` is +, then `Id` would be a subtype of `Id_fresh`)
   (where/error (Id_extruded Env_with_var) (env-with-fresh-var-in-level Env Level))

   ; relate `Id` and the new extruded type -- note that `Id` always has a higher
   ; level than the new variable, so it can refer to the new variable in its bounds
   ; (but not vice versa)
   (where/error Bound_new (extrude-bound Polarity Id Id_extruded))
   (where/error Env_bounded (env-with-fresh-bound Env_with_var Bound_new))

   ; find the bounds to copy over to the new variable
   (where/error Tys_bounds (appropriate-bounds-of-var-in-env Env_bounded Id Polarity))

   ; extrude those bounds and add them to `Id_extruded`
   (where/error Env_out (extrude-bounds Env_bounded IdPairs Level Polarity Id Id_extruded Tys_bounds))
   ]

  )

(define-metafunction simple-sub
  ;; Extrude bounds if `Id` across a list of its bounds, folding the
  ;; environment. For each bound, relate `Id_extruded` to the extruded type.
  extrude-bounds : Env IdPairs Level Polarity Id Id_extruded Tys -> Env

  [(extrude-bounds Env IdPairs Level Polarity _ _ ()) Env]

  [(extrude-bounds Env (IdPair ...) Level Polarity Id Id_extruded (Ty_next Ty_rest ...))
   (extrude-bounds Env_1 (IdPair ...) Level Polarity Id Id_extruded (Ty_rest ...))

   (where/error (Env_0 Ty_extruded) (extrude-ty Env ((Id Id_extruded) IdPair ...) Level Polarity Ty_next))
   (where/error Env_1 (env-with-fresh-bound Env_0 (appropriate-bound Polarity Id_extruded Ty_extruded)))
   ]

  )

(define-metafunction simple-sub
  extrude-bound : Polarity Id Id -> Bound

  [(extrude-bound + Id_old Id_extruded) (Id_old <= Id_extruded)]
  [(extrude-bound - Id_old Id_extruded) (Id_old >= Id_extruded)]
  )

(define-metafunction simple-sub
  instantiate-above : Env Level Ty -> (Env Ty)

  )

(module+ test

  (test-equal (term (level-of-ty EmptyEnv Int)) (term (L 0)))

  (redex-let*
   simple-sub
   [((Id_x Env) (term (env-with-fresh-var EmptyEnv)))]
   (test-equal (term (level-of-ty Env (Cons Int (Cons Id_x (Id_x -> Int)))))
               (term (L 0))))


  (; test scenario
   ;
   ;    L0         L1
   ;
   ;               Id_x <= Id_y <= Int
   ;                            <= Id_x (cycle)
   redex-let*
   simple-sub
   [(Env_0 (term (env-with-adjusted-level EmptyEnv +1)))
    ((Id_x Env_1) (term (env-with-fresh-var Env_0)))
    ((Id_x1 Env_2) (term (env-with-fresh-var Env_1)))
    (Env_3 (term (env-with-fresh-bound Env_2 (Id_x <= Id_x1))))
    (Env_4 (term (env-with-fresh-bound Env_3 (Id_x1 <= Id_x))))
    (Env_5 (term (env-with-fresh-bound Env_4 (Id_x1 <= Int))))]

   (; extrude Id_x with polarity -
    redex-let*
    simple-sub
    [((Env_e Id_e) (term (extrude Env_5 (L 0) - Id_x)))]

    (test-equal (term (var-def-in-env Env_e Id_x))
                (term (X (L 1) (Id_e) (X1))))
    (test-equal (term (var-def-in-env Env_e Id_e))
                (term (X2 (L 0) () (X3))))
    (test-equal (term (var-def-in-env Env_e X3))
                (term (X3 (L 0) () (X2 Int))))
    )

   (; extrude Id_x1 with polarity -
    redex-let*
    simple-sub
    [((Env_e Id_e) (term (extrude Env_5 (L 0) - Id_x1)))]

    (test-equal (term (var-def-in-env Env_e Id_e))
                (term (X2 (L 0) () (X3 Int))))
    (test-equal (term (var-def-in-env Env_e X3))
                (term (X3 (L 0) () (X2))))
    )

   (; extrude Id_x with polarity +
    redex-let*
    simple-sub
    [((Env_e Id_e) (term (extrude Env_5 (L 0) + Id_x)))]

    (test-equal (term (var-def-in-env Env_e Id_e))
                (term (X2 (L 0) () ())))
    )

   (current-traced-metafunctions '(extrude-ty))
   (; extrude (Id_x -> Id_x1) with polarity -
    redex-let*
    simple-sub
    [((Env_e (Id_xe -> Id_ye)) (term (extrude Env_5 (L 0) - (Id_x -> Id_x1))))]


    ; Extruded: X2 -> X3     <:
    ; Original: X  -> X1
    ; Hence X <: X2, X3 <: X1
    ;
    ; Honestly, this is a bit wacky, but I think it's working correctly.
    ; One interesting note is that we create X2 and setup an edge
    ; `X <= X2`. Then later when we extrude X3, we go to extrude X,
    ; and we observe this edge, and so the resulting X4 has a <= edge to X2
    ; as well.
    ;
    ;
    ;                 ┌───────────────┐
    ;                 ▼               │
    ; ┌─────┐       ┌─────────┐     ┌────┐
    ; │ Int │ ◀──── │   X3    │ ──▶ │ X4 │ ─┐
    ; └─────┘       └─────────┘     └────┘  │
    ;   ▲             │               │     │
    ;   │             │ >=            │     │
    ;   │             ▼               │     │
    ;   │           ┌─────────┐       │     │
    ;   └────────── │ X1 (L1) │ ◀┐    │     │
    ;               └─────────┘  │    │     │
    ;                 │          │    │     │
    ;                 │          │    │     │
    ;                 ▼          │    │     │
    ;               ┌─────────┐  │    │     │
    ;          ┌──▶ │ X (L1)  │ ─┘    │     │
    ;          │    └─────────┘       │     │
    ;          │      │               │     │
    ;          │      │               │     │
    ;          │      ▼               │     │
    ;          │ >= ┌─────────┐       │     │
    ;          │    │   X2    │ ◀─────┼─────┘
    ;          │    └─────────┘       │
    ;          │                      │
    ;          └──────────────────────┘
    ;
    ; https://dot-to-ascii.ggerganov.com/?src_hash=d6de0c92
    ;

    (test-equal (term (var-def-in-env Env_e Id_x))
                (term (X (L 1) (X4) (Id_xe X1))))
    (test-equal (term (var-def-in-env Env_e Id_x1))
                (term (X1 (L 1) (Id_ye) (Int X))))
    (test-equal (term (var-def-in-env Env_e Id_xe))
                (term (X2 (L 0) () ())))
    (test-equal (term (var-def-in-env Env_e Id_ye))
                (term (X3 (L 0) () (X4 Int))))
    (test-equal (term (var-def-in-env Env_e X4))
                (term (X4 (L 0) () (X3 X2))))
    )
   )


  )