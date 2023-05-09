module Timeseries exposing
    ( Series, Observation
    , intervalExtent, intervalExtentMultiple
    , groupByIntervals, groupByIntervalsMultiple, groupByIntervalsInExtent
    )

{-| Utility functions for timeseries data, i.e. `List (Time.Posix, Float)`.
In particular, functions for normalizing (removing gaps) and aggregating data
over time intervals.

Time interval types and operations provided by
[justinmimbs/time-extra](https://package.elm-lang.org/packages/justinmimbs/time-extra/latest)

The [groupByIntervals](#groupByIntervals) function is used internally to prepare
data series for a [columns](Sparklines#columns) chart, and
[groupByIntervalsMultiple](#groupByIntervalsMultiple) is used internally for
[columnsFacets](Sparklines#columnsFacets) charts with fixed X scales.

These may be generally useful, for example if you want to pre-aggregate your
data by time interval for [line](Sparklines#line) and
[lineFacets](Sparklines#lineFacets) charts (which do not automatically use
these functions).


# Types

@docs Series, Observation


# Time interval extents

@docs intervalExtent, intervalExtentMultiple


# Normalizing and aggregating data over time intervals

@docs groupByIntervals, groupByIntervalsMultiple, groupByIntervalsInExtent

-}

import Statistics
import Time
import Time.Extra as Time


{-| Representation of a time series
-}
type alias Series =
    List Observation


{-| Representation of a single observation in a time series
-}
type alias Observation =
    ( Time.Posix, Float )


{-| Calculate the extent of a time series when grouped by the given interval.
Note the lower bound is "rounded down" and the upper bound is "rounded up".
-}
intervalExtent : Time.Interval -> Time.Zone -> Series -> Maybe ( Time.Posix, Time.Posix )
intervalExtent tint tz seq =
    seq
        |> List.map Tuple.first
        |> Statistics.extentBy Time.posixToMillis
        |> Maybe.map
            (Tuple.mapBoth
                (Time.floor tint tz)
                (Time.ceiling tint tz)
            )


{-| Calculate the extent of multiple time series when grouped by the given
interval, where the lower bound is the earliest time interval across all
series and the higher bound is the latest time interval across all series.
Note the lower bound is "rounded down" and the upper bound is "rounded up".
-}
intervalExtentMultiple : Time.Interval -> Time.Zone -> List Series -> Maybe ( Time.Posix, Time.Posix )
intervalExtentMultiple tint tz seqs =
    seqs
        |> List.map (List.map Tuple.first)
        |> List.filterMap (Statistics.extentBy Time.posixToMillis)
        |> combineListTuple
        |> Tuple.mapBoth
            (Statistics.extentBy Time.posixToMillis
                >> Maybe.map (Tuple.first >> Time.floor tint tz)
            )
            (Statistics.extentBy Time.posixToMillis
                >> Maybe.map (Tuple.second >> Time.ceiling tint tz)
            )
        |> (\( mmin, mmax ) -> Maybe.map2 Tuple.pair mmin mmax)


{-| Group series by given time interval, aggregating y values by given function,
and normalizing the series such that missing observations for time intervals
are set to y = 0.
-}
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
                |> Maybe.withDefault ( Time.millisToPosix 0, Time.millisToPosix 0 )
    in
    groupByIntervalsInExtent fn tint tz ext seq


{-| Group multiple series by given time interval, aggregating y values by
given function, and normalizing all series such that they all have the same
extent and missing observations for time intervals in each series are set to
y = 0.
-}
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
                |> Maybe.withDefault ( Time.millisToPosix 0, Time.millisToPosix 0 )
    in
    seqs
        |> List.map (groupByIntervalsInExtent fn tint tz ext)


{-| Group series by given time interval and extent, aggregating y values by
given function, and normalizing the series such that missing observations for
time intervals are set to y = 0.
-}
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
