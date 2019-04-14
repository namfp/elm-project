module Tools exposing (..)

multipleResult : List (Result a b) -> Result a (List b)
multipleResult results =
    f (Ok _) (Ok value2) =
    List.foldl