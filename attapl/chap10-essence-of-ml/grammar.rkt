#lang racket
(require redex/reduction-semantics)
(provide ml)

(; Figure 10-1, page 392
 define-language ml
  (t ::=               ; expressions
     X                 ; identifier
     (λ Z t)           ; function
     (t t)             ; application
     (let Z = t in t)  ; local definition
     )

  (Vs Ws ::= (V ...))
  (V W ::=
     Z             ; variable
     M             ; memory location
     (λ Z t)       ; function
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
      (V EC)            ; Right side of an application
      (let Z = EC in t)
      )
  (Term ::= any)

  ; x, y: represent any kind of identifier
  (Xs Ys ::= (X ...))
  (X Y ::= Z M C)

  ; variable identifiers
  (Z ::= variable-not-otherwise-mentioned)

  ; memory locations
  (Ms ::= (M ...))
  (M ::= variable-not-otherwise-mentioned)

  ; constants
  (C ::=
     number
     +)

  ; stores
  (μ ::= (HeapCell ...))
  (HeapCell ::= (M V))

  ; configuration
  (Configuration ::= (t / μ))

  #:binding-forms
  (λ Z t #:refers-to Z)
  (let Z = t in t #:refers-to Z)
  )


