#lang racket
(require redex/reduction-semantics)
(provide (all-defined-out))

(define-language simple-sub
  (Expr :=
        Id
        number
        (Lambda Id -> Expr)
        (Expr Expr)
        (struct FieldExprs)
        (Expr -> Field)
        (Let Id = Expr in Expr)
        )

  (FieldExprs := (FieldExpr ...))
  (FieldExpr := (FieldId Expr))

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
      (struct FieldTys)
      )

  (FieldTys := (FieldTy ...))
  (FieldTy := (FieldId Ty))

  ((Id FieldId) := variable-not-otherwise-mentioned)
  )