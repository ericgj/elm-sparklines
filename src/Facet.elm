module Facet exposing
    ( BandConfig
    , ContinuousConfig
    , Scale(..)
    , Scale2d
    , Scaling(..)
    , Scaling2d
    , bandScale
    , continuousScale
    , fixedBandScale
    , fixedContinuousScale
    , fixedLinearScale
    , fixedTimeBandScale
    , fixedTimeScale
    , freeBandScales
    , freeContinuousScales
    , freeLinearScales
    , freeTimeBandScales
    , freeTimeScales
    , linearScale
    , timeBandScale
    , timeScale
    )

import Dict
import Scale exposing (BandScale, ContinuousScale)
import Statistics
import Time


type Scaling
    = Fixed
    | Free


type alias Scaling2d =
    { x : Scaling
    , y : Scaling
    }


type Scale a
    = FixedScale a
    | FreeScales (List a)


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
