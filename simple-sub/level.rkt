#lang racket
(require redex/reduction-semantics "grammar.rkt" "env.rkt")
(provide level-of-ty
         extrude
         instantiate-above)

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
  extrude : Env Level Ty Polarity -> (Env Ty)

  )

(define-metafunction simple-sub
  ;; Extrude a given type so that it is at-or-above the given level.
  ;;
  ;; `IdPairs` represents the stack of "in progress" extrusions.
  extrude-ty : Env IdPairs Level Polarity Ty -> (Env Ty)

  ;; uninteresting cases: just perform structural recursion

  [(extrude-ty Env IdPairs Level Polarity Int) (Env Ty)]

  [(extrude-ty Env IdPairs Level Polarity (Ty_arg -> Ty_ret))
   (Env_ret (Ty_arg_extruded -> Ty_ret_extruded))
   (where/error (Env_arg Ty_arg_extruded) (extrude-ty Env IdPairs Level (invert-polarity Polarity) Ty_arg))
   (where/error (Env_ret Ty_ret_extruded) (extrude-ty Env IdPairs Level Polarity Ty_arg))
   ]

  [(extrude-ty Env IdPairs Level Polarity (Cons Ty_car Ty_cdr))
   (Env_cdr (Cons Ty_car_extruded Ty_cdr_extruded))
   (where/error (Env_car Ty_car_extruded) (extrude-ty Env IdPairs Level Polarity Ty_car))
   (where/error (Env_cdr Ty_cdr_extruded) (extrude-ty Env_car IdPairs Level Polarity Ty_cdr))
   ]

  ;; variables

  ; detect cyclic types and re-use the new variable
  [(extrude-ty _ (_ ... (Id Id_extruded) _ ...) _ _ Id) Id_extruded]

  ; variables at-or-below our target level can just be copied over
  [(extrude-ty Env IdPairs Level Polarity Id)
   Id
   (where/error Level_id (level-of-var-in-env Env Id))
   (where #t (level-below Level_id Level))
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
  [(extrude-ty Env (IdPair ...) Level Polarity Id)
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
   (where/error Tys_bounds (appropriate-bounds-of-var-in-env Env_1 Id Polarity))

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
   (extrude-bounds Env_1 IdPairs Level Polarity Id Id_extruded (Ty_rest ...))

   (where/error (Env_0 Ty_extruded) (extrude-ty Env ((Id Id_extruded) IdPair ...) Level Polarity Ty_0))
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

  )