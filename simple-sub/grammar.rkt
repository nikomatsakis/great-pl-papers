#lang racket
(require redex/reduction-semantics)
(provide (all-defined-out))

(define-language simple-sub
  ; Expr: defines the ML-sub language
  (Exprs := (Expr ...))
  (Expr :=
        Id                       ; refrence a variable
        number                   ; constant
        (λ Id -> Expr)           ; define a lambda
        (Expr_f Expr_a)          ; apply a function `f` to its argument `a`
        (Cons Expr Expr)         ; create a tuple
        (Car Expr)               ; get the first element from a tuple
        (Cdr Expr)               ; get the second element from a tuple
        (let Id = Expr in Expr)  ; create a binding; performs let generalization
        )

  ; UserTy: user-facing representation of a type.
  ; contains all information needed and is not
  ; relative to a typing environment, though it may include
  ; free variables.
  (UserTys := (UserTy ...))
  (UserTy :=
          ⊤
          ⊥
          (⨆ UserTy UserTy)
          (⊓ UserTy UserTy)
          (UserTy -> UserTy)
          (Cons UserTy UserTy)
          (μ Id Type)
          Id
          Int
          )

  ; Polarity: aka variance, negative polarity indicates we
  ; are within the argument of a fn
  (Polarity := + -)

  ; PolarId: a type variable and a polarity
  (PolarIds := (PolarId ...))
  (PolarId := (Polarity Id))

  ; PolarIdVar: map a `PolarId` to a recursive type variable.
  ; Used during user-type reconstruction.
  (PolarIdVars := (PolarIdVar ...))
  (PolarIdVar := (PolarId Id))

  ; Env: information about type variables
  (Env := (Level TyVarDefs))

  ; IdTy: an id -> ty mapping, used to store the types of bound variables
  ; in expressions
  (IdTys := (IdTy ...))
  (IdTy := (Id TyScheme))

  ; IdPair: an id -> id mapping, used internally within extrude
  (IdPairs := (IdPair ...))
  (IdPair := (Id Id))

  ; TyVarDef: maps a variable to its definition
  ; (level, current bounds) within the environment
  (TyVarDefs := (TyVarDef ...))
  (TyVarDef := (Id Level Tys_lower Tys_upper))

  ; Bound: a bound on a variable; i.e., relation to a type
  (Bound :=
         (Id <= Ty)
         (Id >= Ty))

  ; Ty: the representation of types we use during type-checking.
  ; For type variables (`Id`), all the relevant state is
  ; stored in the environment `Env`.
  (Tys := (Ty ...))
  (Ty :=
      Id
      Int
      (Ty -> Ty)
      (Cons Ty Ty)
      )

  ; TyScheme: a type that may contain forall binders.
  ; We represent binders as a level -- any type variable
  ; referenced within the TypeScheme is universally bound
  ; and should be instantiated (note that it may have subtyping
  ; relationships still to other types).
  (TyScheme := Ty (ForAll Level Ty))

  ; Level: tracks how many `let` binders we are within, used for
  ; let generalization.
  (Level := (L number))

  (Id := variable-not-otherwise-mentioned)
  )

(define-metafunction simple-sub
  invert-polarity : Polarity -> Polarity

  [(invert-polarity +) -]
  [(invert-polarity -) +]
  )