#lang racket
(require redex/reduction-semantics "grammar.rkt")
(provide reduce-ml)

(define reduce-ml
  (reduction-relation
   ml

   #:domain Configuration

   ; R-Beta
   (--> ((in-hole 𝐸 ((λ z t) v)) / μ)
        ((in-hole 𝐸 (substitute t z v)) / μ)
        "R-Beta")

   ; R-Let
   (--> ((in-hole 𝐸 (let z = v in t)) / μ)
        ((in-hole 𝐸 (substitute t z v)) / μ)
        "R-Let")

   ; R-Delta: just hardcode some rules
   (--> ((in-hole 𝐸 ((+ number_1) number_2)) / μ)
        (,(+ (term number_1) (term number_2)) / μ)
        "R-Delta")

   ; R-Extend this seems too non-determinstic, not sure what I'm supposed to do with that

   )
  )

(module+ test
  (test-->> reduce-ml
            (term ((let x = 11 in ((+ x) x)) / ()))
            (term (22 / ())))

  (test-->> reduce-ml
            (term ((let inc = (λ y ((+ 1) y)) in (inc 21)) / ()))
            (term (22 / ())))
  )