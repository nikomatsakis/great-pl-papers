#lang racket
(require redex/reduction-semantics "grammar.rkt" "substitution.rkt")

(define-metafunction ml
  ; Expand the sugary constraint forms found in 10.2.3 etc.
  sugar : Term ... -> C

  (;; The syntactic sugar `σ ⪯ T_1`: `T_1` is an instance of `σ`
   (sugar σ ⪯ T_1)
   (∃ Xs_f . (D_f ∧ (T_f <= T_1)))

   (where/error (∀ Xs [ D ] . T) σ)
   (where/error (Xs_f (D_f T_f)) (instantiate-with-fresh Xs (D_i T_i) T_1))
   )

  (;; The syntactic sugar `∃ σ` (`T_1` is an instance of `σ`)
   ;; is defined in 10.2.3. This function implements that.
   (sugar ∃ σ)
   (∃ Xs . D)
   (where/error (∀ Xs [ D ] . T) σ)
   )

  ((sugar let x : σ in C)
   ((sugar ∃ σ) ∧ (def x : σ in C))
   )
  )