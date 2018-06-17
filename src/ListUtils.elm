module ListUtils exposing (..)


flatten : List (List a) -> List a
flatten list =
    List.foldr (++) [] list
