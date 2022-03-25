#lang racket
(require redex/reduction-semantics)
(provide ml)

(; Figure 10-1, page 392
 define-language ml
  (t ::=               ; expressions
     X                 ; identifier
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

  #;(σ ::=                ; type scheme, from Figure 10-4
       (∀ Xs [ C ] . T))

  #;(C D ::=              ; constraint, from Figure 10-4
       true
       false
       P Ts
       (C ∧ C)
       (∃ Xs . C)
       (def x : σ in C)
       (x ⪯ T)
       )

  (EC ::=               ; Eval context (𝐸 in the text, but vscode gets grumpy)
      hole
      (EC t)            ; Left side of an application
      (v EC)            ; Right side of an application
      (let z = EC in t)
      )
  (Term ::= any)

  ; x, y: represent any kind of identifier
  (Xs Ys ::= (X ...))
  (X Y ::= z m c)

  ; variable identifiers
  (z ::= variable-not-otherwise-mentioned)

  ; memory locations
  (Ms ::= (m ...))
  (m ::= variable-not-otherwise-mentioned)

  ; constants
  (c ::=
     number
     +)

  ; stores
  (μ ::= (HeapCell ...))
  (HeapCell ::= (m v))

  ; configuration
  (Configuration ::= (t / μ))

  #:binding-forms
  (λ z t #:refers-to z)
  (let z = t in t #:refers-to z)
  )


