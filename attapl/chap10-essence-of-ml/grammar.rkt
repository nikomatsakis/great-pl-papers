#lang racket
(require redex/reduction-semantics)
(provide ml)

(; Figure 10-1, page 392
 define-language ml
  ; defining the set of constants 𝑄
  (Qs ::= (Q ...))
  (Q ::= (C number))

  (T ::=               ; expressions
     X                 ; identifier
     (λ Z T)           ; function
     (T T)             ; application
     (let Z = T in T)  ; local definition
     )

  ((Vs Ws) (V ...))
  (V W ::=
     Z             ; variable
     M             ; memory location
     (λ Z T)       ; function
     number        ; special constant: zero arity :)
     )

  (EC ::=               ; Eval context (𝐸 in the text, but vscode gets grumpy)
      hole
      (EC T)            ; Left side of an application
      (V EC)            ; Right side of an application
      (let Z = EC in T)
      )
  (Term ::= any)

  ; x, y: represent any kind of identifier
  (X Y ::= Z M C)

  ; variable identifiers
  (Z ::= variable-not-otherwise-mentioned)

  ; memory locations
  (M ::= variable-not-otherwise-mentioned)

  ; constants
  (C ::=
     number
     +)

  ; stores
  (μ ::= (HeapCell ...))
  (HeapCell ::= (M V))

  ; configuration
  (Configuration ::= (T μ))

  #:binding-forms
  (λ Z T #:refers-to Z)
  (let Z = T in T #:refers-to Z)
  )


