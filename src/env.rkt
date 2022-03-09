#lang racket
(require redex/reduction-semantics racket/set "grammar.rkt")
(provide (all-defined-out))

(define-term EmptyEnv (() ()))

(define-metafunction simple-sub
  env-with-fresh-var : Env -> (Id Env)

  [(env-with-fresh-var Env)
   (Id_fresh (IdTys ((Id_fresh () ()) BoundedId ...)))
   (where/error Id_fresh ,(variable-not-in (term Env) 'X))
   (where/error (IdTys (BoundedId ...)) Env)
   ]
  )

(define-metafunction simple-sub
  env-bounds : Env Id -> (Tys_lower Tys_upper)

  [(env-bounds Env Id)
   (Tys_lower Tys_upper)
   (where/error (_ (_ ... (Id Tys_lower Tys_upper) _ ...)) Env)
   ]
  )

(define-metafunction simple-sub
  env-upper-bounds : Env Id -> Tys

  [(env-upper-bounds Env Id)
   Tys_upper
   (where/error (Tys_lower Tys_upper) (env-bounds Env Id))
   ]
  )

(define-metafunction simple-sub
  env-lower-bounds : Env Id -> Tys

  [(env-lower-bounds Env Id)
   Tys_lower
   (where/error (Tys_lower Tys_upper) (env-bounds Env Id))
   ]
  )

(define-metafunction simple-sub
  env-with-fresh-bound : Env Bound -> Env or ()

  [(env-with-fresh-bound Env (Id <= Ty))
   (IdTys (BoundedId_0 ... (Id Tys_lower Tys_upper1) BoundedId_1 ...))

   (where/error (IdTys (BoundedId_0 ... (Id Tys_lower Tys_upper0) BoundedId_1 ...)) Env)
   (side-condition (term (pretty-print (term ("env-with-fresh-bound" (Id <= Ty) Tys_upper0)))))
   (where/error Tys_upper1 ,(set-add (term Tys_upper0) (term Ty)))
   (where #f (equal-terms Tys_upper0 Tys_upper1))
   ]

  [(env-with-fresh-bound Env (Ty <= Id))
   (IdTys (BoundedId_0 ... (Id Tys_lower1 Tys_upper) BoundedId_1 ...))

   (where/error (IdTys (BoundedId_0 ... (Id Tys_lower0 Tys_upper) BoundedId_1 ...)) Env)
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