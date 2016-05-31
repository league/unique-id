module Unique exposing
  ( Id
  , Unique
  , return
  , run
  )

{-|
Pure generation of unique identifiers in Elm.

@docs Id, Unique, return, run

-}

{-| TODO -}
type Id = Id Int

{-| TODO -}
type Unique a = U (Id -> (a, Id))

open : Unique a -> Id -> (a, Id)
open (U f) = f

next : Id -> Id
next (Id n) = Id (n+1)

{-| TODO -}
return : a -> Unique a
return a = U(\k -> (a, k))

unique : Unique Id
unique = U(\k -> (k, next k))

andThen : Unique a -> (a -> Unique b) -> Unique b
andThen (U fa) fb =
  U(\k0 -> let (a, k1) = fa k0 in open (fb a) k1)

{-| TODO -}
run : Unique a -> a
run (U f) = fst (f (Id 0))
