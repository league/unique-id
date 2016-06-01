module Unique exposing
  ( Id
  , Unique
  , andThen
  , map
  , map2
  , map3
  , replicate
  , return
  , run
  , sequence
  , unique
  )

{-|
Pure generation of unique identifiers in Elm.

# Types
@docs Id, Unique

# Run
@docs run

# Introduction
@docs return, unique

# Sequencing
@docs andThen, map, map2, map3, replicate, sequence

-}

{-| The type of an identifier. Identifiers are integers, but this is an opaque
type so that we cannot introduce them by accident. Equality and toString are
supported on `Id` values.
-}
type Id = Id Int

{-| A calculation that consumes unique identifiers and produces a value of type
`a`.
-}
type Unique a = U (Id -> (a, Id))

{-| Private: treat a Unique as a function. -}
open : Unique a -> Id -> (a, Id)
open (U f) = f

{-| Private: generate next ID. -}
next : Id -> Id
next (Id n) = Id (n+1)

{-| Return a constant value without consuming any IDs. -}
return : a -> Unique a
return a = U(\k -> (a, k))

{-| Generate a unique ID. -}
unique : Unique Id
unique = U(\k -> (k, next k))

{-| TODO -}
andThen : Unique a -> (a -> Unique b) -> Unique b
andThen (U fa) fb =
  U(\k0 -> let (a, k1) = fa k0 in open (fb a) k1)

{-| Run a computation that consumes unique IDs. Within one invocation of `run`,
the generated `Id` values are guaranteed to be unique. However, multiple
invocations of `run` will generate conflicting `Id`s. -}
run : Unique a -> a
run (U f) = fst (f (Id 0))

{-| Apply a function to the result of a `Unique` computation. -}
map : (a -> b) -> Unique a -> Unique b
map f ua =
  ua `andThen` (return << f)

{-| Apply a function to the result of two `Unique` computations. -}
map2 : (a -> b -> c) -> Unique a -> Unique b -> Unique c
map2 f ua ub =
  ua `andThen` \a -> ub `andThen` \b -> return (f a b)

{-| Apply a function to the result of three `Unique` computations. -}
map3 : (a -> b -> c -> d) -> Unique a -> Unique b -> Unique c -> Unique d
map3 f ua ub uc =
  ua `andThen` \a ->
    ub `andThen` \b ->
      uc `andThen` \c ->
        return (f a b c)

{-| Repeatedly invoke a `Unique` computation, generating a list of results. -}
replicate : Int -> Unique a -> Unique (List a)
replicate n f =
  if n <= 0 then return []
  else f `andThen` \head ->
    replicate (n-1) f `andThen` \tail ->
      return (head::tail)

{-| Execute a list of `Unique` computations in order. -}
sequence : List(Unique a) -> Unique(List a)
sequence list =
  case list of
    [] -> return []
    (head::tail) -> map2 (::) head (sequence tail)
