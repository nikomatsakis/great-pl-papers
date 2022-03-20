#lang racket
(require redex/reduction-semantics "grammar.rkt")
(provide (all-defined-out))

(; Figure 7
 define-judgment-form bidir
  #:mode (wf-context I)
  #:contract (wf-context Context)

  [--------------- "EmptyCtx"
   (wf-context ())
   ]

  [; No shadowing of universal variables
   (wf-context (ContextItem_0 ...))
   (where () (declared-in (ContextItem_0 ...) T))
   --------------- "UvarCtx"
   (wf-context (ContextItem_0 ... (! T)))
   ]

  [(wf-context (ContextItem_0 ...))
   (wf-type (ContextItem_0 ...) Type)
   (where () (declared-in (ContextItem_0 ...) X))
   --------------- "VarCtx"
   (wf-context (ContextItem_0 ... (X : Type)))
   ]

  [(wf-context (ContextItem_0 ...))
   (where () (declared-in (ContextItem_0 ...) X))
   --------------- "EvarCtx"
   (wf-context (ContextItem_0 ... (? X)))
   ]

  [(wf-context (ContextItem_0 ...))
   (wf-type (ContextItem_0 ...) Type)
   (where () (declared-in (ContextItem_0 ...) X))
   --------------- "SolvedEvarCtx"
   (wf-context (ContextItem_0 ... (? X = Type)))
   ]

  [(wf-context (ContextItem_0 ...))
   (where () (declared-in (ContextItem_0 ...) T))
   (where #f (in-context (ContextItem_0 ...) (▶ T)))
   --------------- "MarkerCtxt"
   (wf-context (ContextItem_0 ... (▶ T)))
   ]
  )


(; Figure 7
 define-judgment-form bidir
  #:mode (wf-type I I)
  #:contract (wf-type Context Type)
  )

(define-metafunction bidir
  ;; Finds the way in which a variable T is declared in the context
  ;; (if it is).
  declared-in : Context T -> ! or ? or : or ()

  [(declared-in () T) ()]

  [(declared-in (ContextItem_0 ... (! T)) T) !]

  [(declared-in (ContextItem_0 ... (? T)) T) ?]

  [(declared-in (ContextItem_0 ... (? T = _)) T) ?]

  [(declared-in (ContextItem_0 ... (X : _)) X) :]

  [(declared-in (ContextItem_0 ... ContextItem) T)
   (declared-in (ContextItem_0 ...) T)]

  )

(define-metafunction bidir
  in-context : Context ContextItem -> boolean

  [(in-context (_ ... ContextItem _ ...) ContextItem) #t]

  [(in-context Context ContextItem) #f]

  )

(; Figure 3
 define-judgment-form bidir
  #:mode (wf I I)
  #:contract (wf Context Type)

  [(where (_ ... T _ ...) Context)
   --------------- "DeclUvarWF"
   (wf Context T)
   ]

  [(wf Context Type_A)
   (wf Context Type_B)
   --------------- "DeclArrowWF"
   (wf Context (Type_A -> Type_B))
   ]

  [--------------- "DeclUnitWF"
   (wf Context Unit)
   ]

  [(wf (ContextItem ... T) Type)
   --------------- "DeclForallWF"
   (wf (ContextItem ...) (∀ T Type))
   ]
  )

(; Figure 3
 define-judgment-form bidir
  #:mode (subtype I I I)
  #:contract (subtype Context Type Type)

  [(where (_ ... T _ ...) Context)
   --------------- "<=Var"
   (subtype Context T T)
   ]

  [--------------- "<=Unit"
   (subtype Context Unit Unit)
   ]

  [(subtype Context Type_B1 Type_A1)
   (subtype Context Type_A2 Type_B2)
   --------------- "<=Arrow"
   (subtype Context (Type_A1 -> Type_A2) (Type_B1 -> Type_B2))
   ]

  [

   --------------- "<=∀L"
   (subtype Context (∀ T Type_A) Type_B)
   ]
  )