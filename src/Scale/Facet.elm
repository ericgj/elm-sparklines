module Scale.Facet exposing
    ( ChartDimensions
    , Config
    , ConfigData
    , Scales
    , Scaling(..)
    , ScalingConfig
    , chartDimensions
    , customConfig
    , fixedScaling
    , linearConfig
    , scales
    , scalesFixed
    , timeseriesConfig
    )

import Scale exposing (ContinuousScale)
import Statistics
import Time


type Config x y
    = Config (ConfigData x y)


type alias ConfigData x y =
    { xScale : ( Float, Float ) -> ( x, x ) -> ContinuousScale x
    , yScale : ( Float, Float ) -> ( y, y ) -> ContinuousScale y
    , xDefault : ( x, x )
    , yDefault : ( y, y )
    , xToFloat : x -> Float
    , yToFloat : y -> Float
    , xDomain : Scaling
    , yDomain : Scaling
    , xRange : Scaling
    , yRange : Scaling
    }


type Scaling
    = Fixed
    | Free


type alias ScalingConfig =
    { xDomain : Scaling
    , yDomain : Scaling
    , xRange : Scaling
    , yRange : Scaling
    }


type alias Scales x y =
    { width : Float
    , height : Float
    , padding : Float
    , xScale : ContinuousScale x
    , yScale : ContinuousScale y
    }


type alias ChartDimensions =
    { width : Float
    , height : Float
    , padding : Float
    }



-- CONFIG


linearConfig : ScalingConfig -> Config Float Float
linearConfig c =
    Config
        { xScale = Scale.linear
        , yScale = Scale.linear
        , xDefault = emptyFloatExtent
        , yDefault = emptyFloatExtent
        , xToFloat = identity
        , yToFloat = identity
        , xDomain = c.xDomain
        , yDomain = c.yDomain
        , xRange = c.xRange
        , yRange = c.yRange
        }


timeseriesConfig : Time.Zone -> ScalingConfig -> Config Time.Posix Float
timeseriesConfig zone c =
    Config
        { xScale = Scale.time zone
        , yScale = Scale.linear
        , xDefault = emptyTimeExtent
        , yDefault = emptyFloatExtent
        , xToFloat = Time.posixToMillis >> toFloat
        , yToFloat = identity
        , xDomain = c.xDomain
        , yDomain = c.yDomain
        , xRange = c.xRange
        , yRange = c.yRange
        }


customConfig : ConfigData x y -> Config x y
customConfig =
    Config


fixedScaling : ScalingConfig
fixedScaling =
    { xDomain = Fixed
    , yDomain = Fixed
    , xRange = Fixed
    , yRange = Fixed
    }


emptyFloatExtent : ( Float, Float )
emptyFloatExtent =
    ( 0.0, 0.0 )


emptyTimeExtent : ( Time.Posix, Time.Posix )
emptyTimeExtent =
    ( Time.millisToPosix 0, Time.millisToPosix 0 )



{-
   Note that the Scaling options are ignored here, as the aim is to generate a
   single fixed Scales for all sequences, with the range dimensions unchanged.
-}


scalesFixed :
    Config x y
    -> ChartDimensions
    -> List (List ( x, y ))
    -> Scales x y
scalesFixed (Config c) dim seqs =
    let
        ( xseqs, yseqs ) =
            combineListsTuple seqs

        xScale =
            scaleFixed c.xToFloat c.xDefault (c.xScale <| xRange dim) xseqs

        yScale =
            scaleFixed c.yToFloat c.yDefault (c.yScale <| yRange dim) yseqs
    in
    { width = dim.width
    , height = dim.height
    , padding = dim.padding
    , xScale = xScale
    , yScale = yScale
    }


scales :
    Config x y
    -> ChartDimensions
    -> List (List ( x, y ))
    -> List (Scales x y)
scales c dims seqs =
    let
        newdims =
            chartDimensions c dims seqs
    in
    scalesHelp c newdims seqs


chartDimensions :
    Config x y
    -> ChartDimensions
    -> List (List ( x, y ))
    -> List ChartDimensions
chartDimensions (Config c) dims seqs =
    case ( c.xRange, c.yRange ) of
        ( Fixed, Fixed ) ->
            dims |> List.repeat (List.length seqs)

        ( Free, Free ) ->
            let
                xFactors =
                    proportionsOfExtent (Tuple.first >> c.xToFloat) seqs

                yFactors =
                    proportionsOfExtent (Tuple.second >> c.yToFloat) seqs
            in
            List.map2
                (\xf yf ->
                    { dims
                        | width = dims.width * xf
                        , height = dims.height * yf
                    }
                )
                xFactors
                yFactors

        ( Fixed, Free ) ->
            let
                yFactors =
                    proportionsOfExtent (Tuple.second >> c.yToFloat) seqs
            in
            List.map
                (\yf ->
                    { dims
                        | height = dims.height * yf
                    }
                )
                yFactors

        ( Free, Fixed ) ->
            let
                xFactors =
                    proportionsOfExtent (Tuple.first >> c.xToFloat) seqs
            in
            List.map
                (\xf ->
                    { dims
                        | width = dims.width * xf
                    }
                )
                xFactors


scalesHelp :
    Config x y
    -> List ChartDimensions
    -> List (List ( x, y ))
    -> List (Scales x y)
scalesHelp (Config c) dims seqs =
    let
        ( xseqs, yseqs ) =
            combineListsTuple seqs
    in
    case ( c.xDomain, c.yDomain ) of
        ( Fixed, Fixed ) ->
            let
                xScale dim =
                    scaleFixed c.xToFloat c.xDefault (c.xScale <| xRange dim) xseqs

                yScale dim =
                    scaleFixed c.yToFloat c.yDefault (c.yScale <| yRange dim) yseqs
            in
            List.map
                (\dim ->
                    { width = dim.width
                    , height = dim.height
                    , padding = dim.padding
                    , xScale = xScale dim
                    , yScale = yScale dim
                    }
                )
                dims

        ( Free, Free ) ->
            let
                xScale dim seq =
                    scaleFree c.xToFloat c.xDefault (c.xScale <| xRange dim) seq

                yScale dim seq =
                    scaleFree c.yToFloat c.yDefault (c.yScale <| yRange dim) seq
            in
            List.map3
                (\dim xseq yseq ->
                    { width = dim.width
                    , height = dim.height
                    , padding = dim.padding
                    , xScale = xScale dim xseq
                    , yScale = yScale dim yseq
                    }
                )
                dims
                xseqs
                yseqs

        ( Fixed, Free ) ->
            let
                xScale dim =
                    scaleFixed c.xToFloat c.xDefault (c.xScale <| yRange dim) xseqs

                yScale dim seq =
                    scaleFree c.yToFloat c.yDefault (c.yScale <| yRange dim) seq
            in
            List.map2
                (\dim yseq ->
                    { width = dim.width
                    , height = dim.height
                    , padding = dim.padding
                    , xScale = xScale dim
                    , yScale = yScale dim yseq
                    }
                )
                dims
                yseqs

        ( Free, Fixed ) ->
            let
                xScale dim seq =
                    scaleFree c.xToFloat c.xDefault (c.xScale <| xRange dim) seq

                yScale dim =
                    scaleFixed c.yToFloat c.yDefault (c.yScale <| yRange dim) yseqs
            in
            List.map2
                (\dim xseq ->
                    { width = dim.width
                    , height = dim.height
                    , padding = dim.padding
                    , xScale = xScale dim xseq
                    , yScale = yScale dim
                    }
                )
                dims
                xseqs


xRange : ChartDimensions -> ( Float, Float )
xRange dims =
    ( 0, dims.width - 2 * dims.padding )


yRange : ChartDimensions -> ( Float, Float )
yRange dims =
    ( dims.height - 2 * dims.padding, 0 )


scaleFixed :
    (a -> comparable)
    -> ( a, a )
    -> (( a, a ) -> ContinuousScale a)
    -> List (List a)
    -> ContinuousScale a
scaleFixed fn defext toScale seqs =
    seqs
        |> extentMultipleBy fn
        |> Maybe.withDefault defext
        |> toScale


scaleFree :
    (a -> comparable)
    -> ( a, a )
    -> (( a, a ) -> ContinuousScale a)
    -> List a
    -> ContinuousScale a
scaleFree fn defext toScale seq =
    seq
        |> Statistics.extentBy fn
        |> Maybe.withDefault defext
        |> toScale


proportionsOfExtent :
    (datum -> Float)
    -> List (List datum)
    -> List Float
proportionsOfExtent fn seqs =
    let
        exts =
            seqs
                |> List.map (List.map fn >> Statistics.extent)

        mmax =
            exts
                |> List.filterMap (Maybe.map (\( n, m ) -> m - n))
                |> List.maximum
    in
    exts
        |> List.map
            (\mext ->
                Maybe.map2
                    (\max ( n, m ) ->
                        if max == 0.0 then
                            0.0

                        else
                            (m - n) / max
                    )
                    mmax
                    mext
                    |> Maybe.withDefault 0.0
            )


extentMultiple : List (List comparable) -> Maybe ( comparable, comparable )
extentMultiple =
    extentMultipleBy identity


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


combineListsTuple : List (List ( x, y )) -> ( List (List x), List (List y) )
combineListsTuple =
    List.foldr
        (\list ( lxs, lys ) ->
            list |> combineListTuple |> (\( xs, ys ) -> ( xs :: lxs, ys :: lys ))
        )
        ( [], [] )


combineListTuple : List ( x, y ) -> ( List x, List y )
combineListTuple =
    List.foldr (\( x, y ) ( xs, ys ) -> ( x :: xs, y :: ys )) ( [], [] )


combineTupleMaybe : ( Maybe x, Maybe y ) -> Maybe ( x, y )
combineTupleMaybe ( mx, my ) =
    Maybe.map2 (\x y -> Just ( x, y )) mx my
        |> Maybe.andThen identity
