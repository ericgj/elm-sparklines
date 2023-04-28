module Scale.Facet exposing
    ( ContinuousConfig
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
    )

import List.Extra as List
import Scale exposing (BandConfig, BandScale, ContinuousScale)
import Set
import Statistics
import Time
import Time.Extra as Time



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
        |> List.map
            (List.map fn
                >> Statistics.extentBy c.sortBy
                >> Maybe.withDefault c.defaultExtent
                >> c.scale range
            )



-- BAND SCALES


fixedBandScale :
    (a -> comparable)
    -> BandConfig
    -> ( Float, Float )
    -> List (List a)
    -> BandScale comparable
fixedBandScale sortBy c range seqs =
    let
        vals =
            seqs
                |> List.foldr
                    (\seq acc -> List.foldr (sortBy >> Set.insert) acc seq)
                    Set.empty
                |> Set.toList
                |> List.sort
    in
    Scale.band c range vals


freeBandScales :
    (a -> comparable)
    -> BandConfig
    -> ( Float, Float )
    -> List (List a)
    -> List (BandScale comparable)
freeBandScales sortBy c range seqs =
    seqs
        |> List.map (\seq -> fixedBandScale sortBy c range [ seq ])


fixedTimeBandScale :
    Time.Interval
    -> Time.Zone
    -> (a -> Time.Posix)
    -> BandConfig
    -> ( Float, Float )
    -> List (List a)
    -> BandScale Time.Posix
fixedTimeBandScale tint tz toPosix c range seqs =
    let
        ( tmin, tmax ) =
            seqs
                |> List.map (List.map toPosix)
                |> extentMultipleBy Time.posixToMillis
                |> Maybe.withDefault ( Time.millisToPosix 0, Time.millisToPosix 0 )
                |> Tuple.mapBoth (Time.floor tint tz) (Time.ceiling tint tz)
    in
    Scale.band c range <|
        List.iterate
            (\t ->
                if Time.posixToMillis t < Time.posixToMillis tmax then
                    Just <| Time.add tint 1 tz t

                else
                    Nothing
            )
            tmin


freeTimeBandScales :
    Time.Interval
    -> Time.Zone
    -> (a -> Time.Posix)
    -> BandConfig
    -> ( Float, Float )
    -> List (List a)
    -> List (BandScale Time.Posix)
freeTimeBandScales tint tz toPosix c range seqs =
    seqs
        |> List.map (\seq -> fixedTimeBandScale tint tz toPosix c range [ seq ])



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
