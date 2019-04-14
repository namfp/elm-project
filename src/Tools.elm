module Tools exposing (..)

multipleResult : List (Result a b) -> Result a (List b)
multipleResult results =
    let
        apply currentElement currentResult =
            case (currentResult, currentElement) of
                ((Ok r), (Ok current)) -> Ok (current :: r)
                ((Err err), _) -> Err err
    in
        List.foldl apply (Ok []) results
