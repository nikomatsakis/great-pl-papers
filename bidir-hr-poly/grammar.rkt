#lang racket
(require redex/reduction-semantics)
(provide (all-defined-out))

(define-language bidir
  ; Figure 1: Source expressions
  (Expr := X () (λ X Expr) (Expr Expr) (Expr : Type))

  ; Figure 6: Algorithmic types and constants
  (Type := Unit T (∀ T Type) (Type -> Type))
  (Monotype := Unit alpha (Monotype -> Monotype))
  (Context := (ContextItem ...))
  (ContextItem :=
               (! T) ; T is universally bound
               (? T) ; T is existentially bound
               (? T = Type) ; T is existentially bound with value Type
               (▶ T) ; Marker for existential variable T
               (X : Type) ; program variable X has type Type
               )

  ; Variables
  ((X T) := variable-not-otherwise-mentioned)
  )


