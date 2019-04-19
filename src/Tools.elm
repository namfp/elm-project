module Tools exposing (flatten, maxBy, minBy, multipleResult, sum)


multipleResult : List (Result a b) -> Result a (List b)
multipleResult results =
    let
        apply currentElement currentResult =
            case ( currentResult, currentElement ) of
                ( Ok r, Ok current ) ->
                    Ok (current :: r)

                ( Ok r, Err current ) ->
                    Err current

                ( Err err, _ ) ->
                    Err err
    in
    List.foldl apply (Ok []) results


minBy : (a -> comparable) -> List a -> Maybe a
minBy f list =
    extremBy (<) f list


maxBy : (a -> comparable) -> List a -> Maybe a
maxBy f list =
    extremBy (>) f list


extremBy : (comparable -> comparable -> Bool) -> (a -> comparable) -> List a -> Maybe a
extremBy comparator f list =
    case list of
        [] ->
            Nothing

        head :: _ ->
            let
                r =
                    List.foldl
                        (\currentResult current ->
                            if comparator (f current) (f currentResult) then
                                current

                            else
                                currentResult
                        )
                        head
                        list
            in
            Just r


flatten : List (Maybe a) -> List a
flatten list =
    List.foldr
        (\maybeA currentResult ->
            case maybeA of
                Nothing ->
                    currentResult

                Just v ->
                    v :: currentResult
        )
        []
        list


average : List Float -> Float
average floats =
    let
        ( totalSize, totalSum ) =
            List.foldl
                (\current currentResult ->
                    case currentResult of
                        ( currentSize, currentSum ) ->
                            ( currentSize + 1, currentSum + current )
                )
                ( 0, 0.0 )
                floats
    in
    totalSum / totalSize


sum : List Float -> Float
sum floats =
    List.foldl (+) 0.0 floats
