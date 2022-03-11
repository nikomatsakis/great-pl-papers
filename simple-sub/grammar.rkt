#lang racket
(require redex/reduction-semantics)
(provide (all-defined-out))

(define-language simple-sub
  (Exprs := (Expr ...))
  (Expr :=
        Id
        number
        (Lambda Id -> Expr)
        (Expr Expr)
        (Tuple Exprs)
        (Get Expr number)
        (Let Id = Expr in Expr)
        )

  (UserTys := (UserTy ...))
  (UserTy :=
          ⊤
          ⊥
          (⨆ UserTy UserTy)
          (⊓ UserTy UserTy)
          (UserTy -> UserTy)
          (Tuple UserTys)
          (μ Id Type)
          Id
          Int
          )

  (Polarity := + -)

  ; PolarId: a type variable and a polarity
  (PolarIds := (PolarId ...))
  (PolarId := (Polarity Id))

  ; PolarIdVar: map a `PolarId` to a recursive type variable.
  ; Used during user-type reconstruction.
  (PolarIdVars := (PolarIdVar ...))
  (PolarIdVar := (PolarId Id))

  (Env := (IdTys BoundedIds))

  (IdTys := (IdTy ...))
  (IdTy := (Id Ty))

  (BoundedIds := (BoundedId ...))
  (BoundedId := (Id Tys Tys))

  (Bound :=
         (Id <= Ty)
         (Id >= Ty))

  (Tys := (Ty ...))
  (Ty :=
      Id
      Int
      (Ty -> Ty)
      (Tuple Tys)
      )

  ((Id FieldId) := variable-not-otherwise-mentioned)
  )

(define-metafunction simple-sub
  invert-polarity : Polarity -> Polarity

  [(invert-polarity +) -]
  [(invert-polarity -) +]
  )