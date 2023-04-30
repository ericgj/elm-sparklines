module Timeseries exposing
    ( Observation
    , Series
    , groupByIntervals
    , groupByIntervalsInExtent
    , groupByIntervalsMultiple
    , intervalExtent
    , intervalExtentMultiple
    )

import Statistics
import Time
import Time.Extra as Time


type alias Series =
    List Observation


type alias Observation =
    ( Time.Posix, Float )


intervalExtent : Time.Interval -> Time.Zone -> Series -> ( Time.Posix, Time.Posix )
intervalExtent tint tz seq =
    seq
        |> List.map Tuple.first
        |> Statistics.extentBy Time.posixToMillis
        |> Maybe.withDefault ( Time.millisToPosix 0, Time.millisToPosix 0 )
        |> Tuple.mapBoth
            (Time.floor tint tz)
            (Time.ceiling tint tz)


intervalExtentMultiple : Time.Interval -> Time.Zone -> List Series -> ( Time.Posix, Time.Posix )
intervalExtentMultiple tint tz seqs =
    seqs
        |> List.map (List.map Tuple.first)
        |> List.filterMap (Statistics.extentBy Time.posixToMillis)
        |> combineListTuple
        |> Tuple.mapBoth
            (Statistics.extentBy Time.posixToMillis
                >> Maybe.withDefault ( Time.millisToPosix 0, Time.millisToPosix 0 )
                >> Tuple.first
                >> Time.floor tint tz
            )
            (Statistics.extentBy Time.posixToMillis
                >> Maybe.withDefault ( Time.millisToPosix 0, Time.millisToPosix 0 )
                >> Tuple.second
                >> Time.ceiling tint tz
            )


groupByIntervals :
    (List Float -> Float)
    -> Time.Interval
    -> Time.Zone
    -> Series
    -> Series
groupByIntervals fn tint tz seq =
    let
        ext =
            intervalExtent tint tz seq
    in
    groupByIntervalsInExtent fn tint tz ext seq


groupByIntervalsMultiple :
    (List Float -> Float)
    -> Time.Interval
    -> Time.Zone
    -> List Series
    -> List Series
groupByIntervalsMultiple fn tint tz seqs =
    let
        ext =
            intervalExtentMultiple tint tz seqs
    in
    seqs
        |> List.map (groupByIntervalsInExtent fn tint tz ext)


groupByIntervalsInExtent :
    (List Float -> Float)
    -> Time.Interval
    -> Time.Zone
    -> ( Time.Posix, Time.Posix )
    -> Series
    -> Series
groupByIntervalsInExtent fn tint tz ( tmin, tmax ) seq =
    Time.range tint 1 tz tmin (Time.add tint 1 tz tmax)
        |> List.foldr
            (\t acc ->
                seq
                    |> List.filter (isInInterval tint tz t)
                    |> List.foldr (\( _, y ) ys -> y :: ys) []
                    |> (\ys -> ( t, fn ys ) :: acc)
            )
            []


isInInterval : Time.Interval -> Time.Zone -> Time.Posix -> Observation -> Bool
isInInterval tint tz xfloor ( x, _ ) =
    (x |> Time.floor tint tz |> Time.posixToMillis) == (xfloor |> Time.posixToMillis)


combineListTuple : List ( x, y ) -> ( List x, List y )
combineListTuple =
    List.foldr (\( x, y ) ( xs, ys ) -> ( x :: xs, y :: ys )) ( [], [] )
