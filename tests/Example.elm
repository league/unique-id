module Example exposing (..)

{-

This example uses `Unique` to generate unique IDs for each leaf in a binary
tree. It presents a representation of trees using nested boxes: brown for the
branches and green for the leaves.

When you hover over one of the leaves in these two trees, it will splice the
other tree onto that point and show the result. Thus, the unique identifier is
used to designate which leaf to use, independently of the data stored at the
leaf. It can similarly be used to give identity to components of different data
structures.

-}

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Unique as U

type Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a U.Id

type alias TreeGen a = U.Unique (Tree a)

leaf : a -> TreeGen a
leaf a = U.map (Leaf a) U.unique

branch : TreeGen a -> TreeGen a -> TreeGen a
branch left right = U.map2 Branch left right

contains : U.Id -> Tree a -> Bool
contains id tree =
  case tree of
    Leaf x k -> k == id
    Branch l r -> contains id l || contains id r

splice : U.Id -> TreeGen a -> Tree a -> TreeGen a
splice id subst tree =
  case tree of
    Leaf x k ->
      if k == id then subst
      else leaf x
    Branch left right ->
      branch (splice id subst left) (splice id subst right)

nodeStyle color =
  [ ("border", "2px solid " ++ color)
  , ("float", "left")
  , ("padding", "2px")
  , ("margin", "2px")
  ]

branchStyle =
  nodeStyle "brown"

leafStyle =
  ("background", "lightgreen") :: nodeStyle "green"

activeStyle =
  ("background", "orange") :: nodeStyle "green"

here : Maybe U.Id -> U.Id -> Bool
here id1Opt id2 =
  case id1Opt of
    Nothing -> False
    Just id1 -> id1 == id2

type alias Model
  = { t1 : TreeGen String
    , t2 : TreeGen String
    , spliceAt : Maybe U.Id
    }

type Msg
  = SpliceAt U.Id
  | NoSplice

activeTree : Maybe U.Id -> Tree String -> Html Msg
activeTree idOpt tree =
  case tree of
    Branch left right ->
      div [style branchStyle] [activeTree idOpt left, activeTree idOpt right]
    Leaf x k ->
      div [ style <| if here idOpt k then activeStyle else leafStyle
          , onMouseEnter <| SpliceAt k
          , onMouseLeave <| NoSplice
          ] [text x]

showTree : Tree String -> Html Msg
showTree tree =
  case tree of
    Branch left right ->
      div [style branchStyle] [showTree left, showTree right]
    Leaf x _ ->
      div [style leafStyle] [text x]

update msg model =
  case msg of
    SpliceAt s -> {model | spliceAt = Just s}
    NoSplice   -> {model | spliceAt = Nothing}

clear =
  style [ ("clear", "both")
        , ("float", "left")
        , ("min-width", "8em")
        , ("text-align", "right")
        , ("margin-right", "1em")
        ]

view model =
  let (t1, t2) = U.run <| U.map2 (,) model.t1 model.t2
      t3 =
        case model.spliceAt of
          Nothing -> t1
          Just s -> U.run <|
                    if contains s t1 then splice s model.t2 t1
                    else splice s model.t1 t2
  in
    div [] [ h3  [clear] [text "Tree 1"]
           , div []      [activeTree model.spliceAt t1]
           , h3  [clear] [text "Tree 2"]
           , div []      [activeTree model.spliceAt t2]
           , h3  [clear] [text <| toString model.spliceAt]
           , h3  [clear] [text "Spliced"]
           , div []      [showTree t3]
           ]

model : Model
model =
  { t1 =
    branch (branch (branch (leaf "can") (leaf "do"))
                     (leaf "cab"))
             (branch (leaf "zip")
                       (branch (leaf "arc") (leaf "do")))
  , t2 =
    branch (leaf "xen") (branch (leaf "yow") (leaf "zip"))

  , spliceAt =
    Nothing
  }

main =
  Html.App.beginnerProgram
      { model = model
      , view = view
      , update = update
      }
