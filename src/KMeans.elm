module KMeans exposing (Point, chooseCentroid, computeCentroid, createGroup, distance, run, step)

import Browser
import Color as Color
import Dict
import Set
import Tools

type alias Point =
    { x : Float, y : Float }


run : List Point -> List Point -> List Point
run data centroids =
    let
        updatedCenroids =
            step data centroids

        sets =
            updatedCenroids |> List.map (\x -> ( x.x, x.y )) |> Set.fromList

        currentSets =
            centroids |> List.map (\x -> ( x.x, x.y )) |> Set.fromList
    in
    if sets == currentSets then
        centroids

    else
        run data updatedCenroids


step : List Point -> List Point -> List Point
step data centroids =
    let
        groups =
            createGroup data centroids |> List.map Tuple.second

        updatedCentroids =
            List.map computeCentroid groups
    in
    updatedCentroids


distance : Point -> Point -> Float
distance point1 point2 =
    (point1.x - point2.x)
        * (point1.x - point2.x)
        + (point1.y - point2.y)
        * (point1.y - point2.y)
        |> sqrt


chooseCentroid : Point -> List Point -> Maybe Point
chooseCentroid point centroids =
    let
        minDistance =
            Tools.minBy
                (List.map (\centroid -> ( centroid, distance point centroid )) centroids)
                Tuple.second
    in
    minDistance |> Maybe.map Tuple.first


createGroup : List Point -> List Point -> List ( Point, List Point )
createGroup data centroids =
    data
        |> List.map (\point -> chooseCentroid point centroids |> Maybe.map (\centroid -> ( centroid, point )))
        |> Tools.flatten
        |> List.map
            (\x ->
                case x of
                    ( centroid, point ) ->
                        ( ( centroid.x, centroid.y ), point )
            )
        |> List.foldl
            (\current currentResult ->
                case current of
                    ( ( x, y ), point ) ->
                        Dict.update ( x, y )
                            (\d ->
                                case d of
                                    Just l ->
                                        Just (point :: l)

                                    Nothing ->
                                        Just (point :: [])
                            )
                            currentResult
            )
            Dict.empty
        |> Dict.toList
        |> List.map
            (\a ->
                case a of
                    ( ( x, y ), points ) ->
                        ( { x = x, y = y }, points )
            )


computeCentroid : List Point -> Point
computeCentroid points =
    let
        sumX =
            points |> List.map .x |> Tools.sum

        sumY =
            points |> List.map .y |> Tools.sum

        length =
            List.length points |> toFloat

        averageX =
            sumX / length

        averageY =
            sumY / length
    in
    { x = averageX, y = averageY }

