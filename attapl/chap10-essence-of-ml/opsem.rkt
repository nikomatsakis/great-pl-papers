#lang racket
(require redex/reduction-semantics "grammar.rkt")
(provide reduce-ml)

(define reduce-ml
  (reduction-relation
   ml

   ; R-Beta
   (--> ((in-hole EC ((λ Z T) V)) μ)
        ((in-hole EC (substitute T Z V)) μ)
        "R-Beta")

   ; R-Let
   (--> ((in-hole EC (let Z = V in T)) μ)
        ((in-hole EC (substitute T Z V)) μ)
        "R-Let")

   ; R-Delta: just hardcode some rules
   (--> ((in-hole EC ((+ number_1) number_2)) μ)
        (,(+ (term number_1) (term number_2)) μ)
        "R-Delta")

   ; R-Extend this seems too non-determinstic, not sure what I'm supposed to do with that

   )
  )

(module+ test
  (test-->> reduce-ml
            (term ((let x = 11 in ((+ x) x)) ()))
            (term (22 ())))

  (test-->> reduce-ml
            (term ((let inc = (λ y ((+ 1) y)) in (inc 21)) ()))
            (term (22 ())))
  )