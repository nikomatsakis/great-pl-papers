#lang racket
(require redex/reduction-semantics)
(provide (all-defined-out))

(define-language simple-sub
  (Expr :=
        Id
        number
        (Lambda Id -> Expr)
        (Expr Expr)
        (Tuple Exprs)
        (Get Expr number)
        (Let Id = Expr in Expr)
        )


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