module Unique exposing
  ( Id
  , Unique
  , andThen
  , map
  , map2
  , return
  , run
  , unique
  )

{-|
Pure generation of unique identifiers in Elm.

@docs Id, Unique, andThen, return, run, map, map2

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

{-| TODO -}
unique : Unique Id
unique = U(\k -> (k, next k))

{-| TODO -}
andThen : Unique a -> (a -> Unique b) -> Unique b
andThen (U fa) fb =
  U(\k0 -> let (a, k1) = fa k0 in open (fb a) k1)

{-| TODO -}
run : Unique a -> a
run (U f) = fst (f (Id 0))

{-| TODO -}
map : (a -> b) -> Unique a -> Unique b
map f ua =
  ua `andThen` (return << f)

{-| TODO -}
map2 : (a -> b -> c) -> Unique a -> Unique b -> Unique c
map2 f ua ub =
  ua `andThen` \a -> ub `andThen` \b -> return (f a b)
