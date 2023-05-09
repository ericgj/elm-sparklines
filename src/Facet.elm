module Facet exposing
    ( Scaling(..), Scaling2d, Scale(..), Scale2d
    , linearScale, fixedLinearScale, freeLinearScales, timeScale, fixedTimeScale, freeTimeScales
    , timeBandScale, fixedTimeBandScale, freeTimeBandScales
    )

{-| Utilities for creating scales for multiple data series together (fixed) or
separately (free). Used internally by [Sparklines](#Sparklines) facet views,
but may be useful generally.


# Basic types

@docs Scaling, Scaling2d, Scale, Scale2d


# Continuous scales

@docs linearScale, fixedLinearScale, freeLinearScales, timeScale, fixedTimeScale, freeTimeScales


# Band scales

@docs timeBandScale, fixedTimeBandScale, freeTimeBandScales

-}

import Dict
import Scale exposing (BandScale, ContinuousScale)
import Statistics
import Time


{-| A declarative type for how to scale data series
-}
type Scaling
    = Fixed
    | Free


{-| A declarative type for how to scale x and y dimensions of data series
-}
type alias Scaling2d =
    { x : Scaling
    , y : Scaling
    }


{-| Either a single fixed scale or a list of free scales. A result type.
-}
type Scale a
    = FixedScale a
    | FreeScales (List a)


{-| Either a single fixed scale or a list of free scales, for x and y dimensions.
A result type.
-}
type alias Scale2d x y =
    { x : Scale x
    , y : Scale y
    }



-- CONTINUOUS SCALES


type alias ContinuousConfig a comparable =
    { scale : ( Float, Float ) -> ( a, a ) -> ContinuousScale a
    , sortBy : a -> comparable
    , defaultExtent : ( a, a )
    }


{-| Create a fixed linear scale from a list of arbitrary data series, given the
accessor and the range. (The domain of fixed scales is from the minimum
value of all the series to the maximum value of all the series.)
-}
fixedLinearScale :
    (a -> Float)
    -> ( Float, Float )
    -> List (List a)
    -> ContinuousScale Float
fixedLinearScale toFloat range seqs =
    let
        config =
            { scale = Scale.linear
            , sortBy = identity
            , defaultExtent = ( 0.0, 0.0 )
            }
    in
    fixedContinuousScale config toFloat range seqs


{-| Create free linear scales from a list of arbitrary data series, given the
accessor and the range. (The domain of free scales is the domain of each
data series separately.)
-}
freeLinearScales :
    (a -> Float)
    -> ( Float, Float )
    -> List (List a)
    -> List (ContinuousScale Float)
freeLinearScales toFloat range seqs =
    let
        config =
            { scale = Scale.linear
            , sortBy = identity
            , defaultExtent = ( 0.0, 0.0 )
            }
    in
    freeContinuousScales config toFloat range seqs


{-| Create a linear scale from a single arbitrary data series, given the
accessor and the range.
-}
linearScale :
    (a -> Float)
    -> ( Float, Float )
    -> List a
    -> ContinuousScale Float
linearScale toFloat range seq =
    let
        config =
            { scale = Scale.linear
            , sortBy = identity
            , defaultExtent = ( 0.0, 0.0 )
            }
    in
    continuousScale config toFloat range seq


{-| Create a fixed continuous time scale from a list of arbitrary data series,
given the accessor and the range. (The domain of fixed scales is from the
minimum value (time) of all the series to the maximum value of all the series.)
-}
fixedTimeScale :
    Time.Zone
    -> (a -> Time.Posix)
    -> ( Float, Float )
    -> List (List a)
    -> ContinuousScale Time.Posix
fixedTimeScale zone toTime range seqs =
    let
        def =
            ( Time.millisToPosix 0, Time.millisToPosix 0 )

        config =
            { scale = Scale.time zone
            , sortBy = Time.posixToMillis
            , defaultExtent = def
            }
    in
    fixedContinuousScale config toTime range seqs


{-| Create free continuous time scales from a list of arbitrary data series,
given the accessor and the range. (The domain of free scales is the domain of
each data series separately.)
-}
freeTimeScales :
    Time.Zone
    -> (a -> Time.Posix)
    -> ( Float, Float )
    -> List (List a)
    -> List (ContinuousScale Time.Posix)
freeTimeScales zone toTime range seqs =
    let
        def =
            ( Time.millisToPosix 0, Time.millisToPosix 0 )

        config =
            { scale = Scale.time zone
            , sortBy = Time.posixToMillis
            , defaultExtent = def
            }
    in
    freeContinuousScales config toTime range seqs


{-| Create a continuous time scale from a single arbitrary data series, given
the accessor and the range.
-}
timeScale :
    Time.Zone
    -> (a -> Time.Posix)
    -> ( Float, Float )
    -> List a
    -> ContinuousScale Time.Posix
timeScale zone toTime range seq =
    let
        def =
            ( Time.millisToPosix 0, Time.millisToPosix 0 )

        config =
            { scale = Scale.time zone
            , sortBy = Time.posixToMillis
            , defaultExtent = def
            }
    in
    continuousScale config toTime range seq


fixedContinuousScale :
    ContinuousConfig b comparable
    -> (a -> b)
    -> ( Float, Float )
    -> List (List a)
    -> ContinuousScale b
fixedContinuousScale c fn range seqs =
    seqs
        |> List.map (List.map fn)
        |> extentMultipleBy c.sortBy
        |> Maybe.withDefault c.defaultExtent
        |> c.scale range


freeContinuousScales :
    ContinuousConfig b comparable
    -> (a -> b)
    -> ( Float, Float )
    -> List (List a)
    -> List (ContinuousScale b)
freeContinuousScales c fn range seqs =
    seqs
        |> List.map (continuousScale c fn range)


continuousScale :
    ContinuousConfig b comparable
    -> (a -> b)
    -> ( Float, Float )
    -> List a
    -> ContinuousScale b
continuousScale c fn range seq =
    seq
        |> List.map fn
        |> Statistics.extentBy c.sortBy
        |> Maybe.withDefault c.defaultExtent
        |> c.scale range



-- BAND SCALES


type alias BandConfig a comparable =
    { sortBy : a -> comparable
    , bandScaleConfig : Scale.BandConfig
    }


{-| Create a fixed time band scale from a list of arbitrary data series,
given the
[BandScale configuration](https://package.elm-lang.org/packages/gampleman/elm-visualization/latest/Scale#BandScale)
, accessor and the range. (The domain of fixed time band scales is a list of
times from the earliest in all the series to the latest in all the series.)

Note that this function does _not_ change the data in any way, so you should
pre-aggregate data by time interval and deal with possible gaps in intervals.
The [Timeseries](#Timeseries) module has tools for doing this.

-}
fixedTimeBandScale :
    Scale.BandConfig
    -> (a -> Time.Posix)
    -> ( Float, Float )
    -> List (List a)
    -> BandScale Time.Posix
fixedTimeBandScale bc fn range seqs =
    let
        c =
            { sortBy = Time.posixToMillis
            , bandScaleConfig = bc
            }
    in
    fixedBandScale c fn range seqs


{-| Create free time band scales from a list of arbitrary data series,
given the
[BandScale configuration](https://package.elm-lang.org/packages/gampleman/elm-visualization/latest/Scale#BandScale)
, accessor and the range. (The domain of free time band scales is a list of
times from the earliest to the latest, within each series separately.)

Note that this function does _not_ change the data in any way, so you should
pre-aggregate data by time interval and deal with possible gaps in intervals.
The [Timeseries](#Timeseries) module has tools for doing this.

-}
freeTimeBandScales :
    Scale.BandConfig
    -> (a -> Time.Posix)
    -> ( Float, Float )
    -> List (List a)
    -> List (BandScale Time.Posix)
freeTimeBandScales bc fn range seqs =
    let
        c =
            { sortBy = Time.posixToMillis
            , bandScaleConfig = bc
            }
    in
    freeBandScales c fn range seqs


{-| Create a single time band scale from an arbitrary data series,
given the
[BandScale configuration](https://package.elm-lang.org/packages/gampleman/elm-visualization/latest/Scale#BandScale)
, accessor and the range.

Note that this function does _not_ change the data in any way, so you should
pre-aggregate data by time interval and deal with possible gaps in intervals.
The [Timeseries](#Timeseries) module has tools for doing this.

-}
timeBandScale :
    Scale.BandConfig
    -> (a -> Time.Posix)
    -> ( Float, Float )
    -> List a
    -> BandScale Time.Posix
timeBandScale bc fn range seq =
    let
        c =
            { sortBy = Time.posixToMillis
            , bandScaleConfig = bc
            }
    in
    bandScale c fn range seq


fixedBandScale :
    BandConfig b comparable
    -> (a -> b)
    -> ( Float, Float )
    -> List (List a)
    -> BandScale b
fixedBandScale c fn range seqs =
    let
        vals =
            seqs
                |> List.foldr
                    (\seq acc ->
                        List.foldr
                            (\a ->
                                let
                                    b =
                                        fn a
                                in
                                Dict.insert (c.sortBy b) b
                            )
                            acc
                            seq
                    )
                    Dict.empty
                |> Dict.toList
                |> List.sortBy Tuple.first
                |> List.map Tuple.second
    in
    Scale.band c.bandScaleConfig range vals


freeBandScales :
    BandConfig b comparable
    -> (a -> b)
    -> ( Float, Float )
    -> List (List a)
    -> List (BandScale b)
freeBandScales c fn range seqs =
    seqs
        |> List.map (bandScale c fn range)


bandScale :
    BandConfig b comparable
    -> (a -> b)
    -> ( Float, Float )
    -> List a
    -> BandScale b
bandScale c fn range seq =
    let
        vals =
            seq
                |> List.foldr
                    (\a ->
                        let
                            b =
                                fn a
                        in
                        Dict.insert (c.sortBy b) b
                    )
                    Dict.empty
                |> Dict.toList
                |> List.sortBy Tuple.first
                |> List.map Tuple.second
    in
    Scale.band c.bandScaleConfig range vals



-- UTILS


extentMultipleBy :
    (a -> comparable)
    -> List (List a)
    -> Maybe ( a, a )
extentMultipleBy fn seqs =
    seqs
        |> List.filterMap (Statistics.extentBy fn)
        |> combineListTuple
        |> Tuple.mapBoth
            (Statistics.extentBy fn >> Maybe.map Tuple.first)
            (Statistics.extentBy fn >> Maybe.map Tuple.second)
        |> combineTupleMaybe


combineListTuple : List ( x, y ) -> ( List x, List y )
combineListTuple =
    List.foldr (\( x, y ) ( xs, ys ) -> ( x :: xs, y :: ys )) ( [], [] )


combineTupleMaybe : ( Maybe x, Maybe y ) -> Maybe ( x, y )
combineTupleMaybe ( mx, my ) =
    Maybe.map2 (\x y -> Just ( x, y )) mx my
        |> Maybe.andThen identity
