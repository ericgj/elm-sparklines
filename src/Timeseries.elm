module Timeseries exposing
    ( Observation
    , Series
    , byIntervals
    , byIntervalsInExtent
    , byIntervalsMultiple
    , extent
    , extentMultiple
    )

import Statistics
import Time
import Time.Extra as Time


type alias Series =
    List Observation


type alias Observation =
    ( Time.Posix, Float )


extent : Time.Interval -> Time.Zone -> Series -> ( Time.Posix, Time.Posix )
extent tint tz seq =
    seq
        |> List.map Tuple.first
        |> Statistics.extentBy Time.posixToMillis
        |> Maybe.withDefault ( Time.millisToPosix 0, Time.millisToPosix 0 )
        |> Tuple.mapBoth
            (Time.floor tint tz)
            (Time.ceiling tint tz)


extentMultiple : Time.Interval -> Time.Zone -> List Series -> ( Time.Posix, Time.Posix )
extentMultiple tint tz seqs =
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


byIntervals :
    (List Float -> Float)
    -> Time.Interval
    -> Time.Zone
    -> Series
    -> Series
byIntervals fn tint tz seq =
    let
        ext =
            extent tint tz seq
    in
    byIntervalsInExtent fn tint tz ext seq


byIntervalsMultiple :
    (List Float -> Float)
    -> Time.Interval
    -> Time.Zone
    -> List Series
    -> List Series
byIntervalsMultiple fn tint tz seqs =
    let
        ext =
            extentMultiple tint tz seqs
    in
    seqs
        |> List.map (byIntervalsInExtent fn tint tz ext)


byIntervalsInExtent :
    (List Float -> Float)
    -> Time.Interval
    -> Time.Zone
    -> ( Time.Posix, Time.Posix )
    -> Series
    -> Series
byIntervalsInExtent fn tint tz ( tmin, tmax ) seq =
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
