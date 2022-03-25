#lang racket
(require redex/reduction-semantics)
(provide ml)

(; Figure 10-1, page 392
 define-language ml
  (t ::=               ; expressions
     x                 ; identifier
     (λ z t)           ; function
     (t t)             ; application
     (let z = t in t)  ; local definition
     )

  (vs ws ::= (v ...))
  (v w ::=
     z             ; variable
     m             ; memory location
     (λ z t)       ; function
     number        ; special constant: zero arity :)
     )

  (σ ::=                ; type scheme, from Figure 10-4
     (∀ Xs [ C ] . T))

  (C D ::=              ; constraint, from Figure 10-4
     true
     false
     P
     (C ∧ C)
     (∃ Xs . C)
     (def x : σ in C)
     (x ⪯ T)
     )

  (P ::=              ; predicates
     (T == T)         ; type equality
     (T <= T)         ; subtyping
     )

  (T ::=              ; types
     (T -> T)
     int
     X
     )

  (κ ::=              ; kinds
     *
     (κ -> κ)
     )

  (X ::=              ; type variables
     variable-not-otherwise-mentioned
     )

  (𝐸 ::=              ; Eval context
     hole
     (𝐸 t)            ; Left side of an application
     (v 𝐸)            ; Right side of an application
     (let z = 𝐸 in t)
     )
  (Term ::= any)

  ; x, y: represent any kind of identifier
  (xs ys ::= (x ...))
  (x y ::= z m c)

  ; variable identifiers
  (z ::= variable-not-otherwise-mentioned)

  ; memory locations
  (ms ::= (m ...))
  (m ::= variable-not-otherwise-mentioned)

  ; constants
  (c ::=
     number
     +)

  (Γ ::=   ; type environment
     ((x σ) ...)
     )

  (θ ::=                       ; type substitution
     ((X T) ...)
     )

  ; stores
  (μ ::= (HeapCell ...))
  (HeapCell ::= (m v))

  ; configuration
  (Configuration ::= (t / μ))

  #:binding-forms
  (λ z t #:refers-to z)
  (let z = t in t #:refers-to z)
  )
