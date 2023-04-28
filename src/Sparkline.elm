module Sparkline exposing
    ( Config
    , Highlight(..)
    , columnFacets
    , columns
    , defaultColorPairs
    , defaultColumnsConfig
    , defaultLineConfig
    , line
    , lineFacets
    , lines
    )

import Color exposing (Color)
import Html.Bem as Bem exposing (element, elementList)
import List.Extra as List
import Path exposing (Path)
import Scale exposing (BandScale, ContinuousScale)
import Scale.Color
import Scale.Facet as Facet
import Shape
import Statistics
import Time
import Time.Extra as Time
import Timeseries exposing (Observation, Series)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Opacity(..), Paint(..), Transform(..))



--------------------------------------------------------------------------------
-- CONFIG
--------------------------------------------------------------------------------


type Config
    = Config ConfigData


type alias ConfigData =
    { width : Float
    , height : Float
    , padding : Float
    , timeZone : Time.Zone
    , interval : Time.Interval
    , aggregate : List Float -> Float
    , highlight : Highlight
    , label : Maybe (( Time.Posix, Float ) -> String)
    , appearance : AppearanceConfig
    , css : CssConfig
    }


type Highlight
    = NoHighlight
    | HighlightLast
    | HighlightNegative
    | HighlightMin
    | HighlightMax
    | HighlightMinMax
    | HighlightPeaks
        { lookaround : Int
        , sensitivity : Float
        , coallesce : Int
        }


type alias AppearanceConfig =
    { width : Float
    , color : ( Color, Color )
    , labelPadding : TypedSvg.Types.Length
    , bandPaddingInner : Float
    , bandPaddingOuter : Float
    , bandAlign : Float
    }


type alias CssConfig =
    { block : String
    }



-- GETTERS


height : Config -> Float
height (Config c) =
    c.height


width : Config -> Float
width (Config c) =
    c.width


padding : Config -> Float
padding (Config c) =
    c.padding


aggregate : Config -> (List Float -> Float)
aggregate (Config c) =
    c.aggregate


timeInterval : Config -> Time.Interval
timeInterval (Config c) =
    c.interval


timeZone : Config -> Time.Zone
timeZone (Config c) =
    c.timeZone


ranges : Config -> ( ( Float, Float ), ( Float, Float ) )
ranges (Config c) =
    ( ( 0, c.width - 2 * c.padding )
    , ( c.height - 2 * c.padding, 0 )
    )


bandConfig : Config -> Scale.BandConfig
bandConfig (Config c) =
    { paddingInner = c.appearance.bandPaddingInner
    , paddingOuter = c.appearance.bandPaddingOuter
    , align = c.appearance.bandAlign
    }



-- CONSTRUCTORS


defaultLineConfig : Time.Zone -> Float -> Float -> Config
defaultLineConfig z w h =
    Config
        { width = w
        , height = h
        , padding = 10.0
        , timeZone = z
        , interval = Time.Day
        , aggregate = List.sum
        , highlight = NoHighlight
        , label = Nothing
        , appearance = defaultLineAppearanceConfig
        , css = defaultCssConfig
        }


defaultColumnsConfig : (List Float -> Float) -> Time.Interval -> Time.Zone -> Float -> Float -> Config
defaultColumnsConfig fn tint z w h =
    Config
        { width = w
        , height = h
        , padding = 10.0
        , timeZone = z
        , interval = tint
        , aggregate = fn
        , highlight = NoHighlight
        , label = Nothing
        , appearance = defaultColumnsAppearanceConfig
        , css = defaultCssConfig
        }


defaultLineAppearanceConfig : AppearanceConfig
defaultLineAppearanceConfig =
    { width = 1.0
    , color = ( Color.rgb 0 0 0, Color.rgb 1 0 0 )
    , labelPadding = Em 0.35
    , bandPaddingInner = 0.0
    , bandPaddingOuter = 0.0
    , bandAlign = 0.5
    }


defaultColumnsAppearanceConfig : AppearanceConfig
defaultColumnsAppearanceConfig =
    { width = 1.0
    , color = ( Color.rgba 0 0 0 0.75, Color.rgb 1 0 0 )
    , labelPadding = Em 0.35
    , bandPaddingInner = 0.0
    , bandPaddingOuter = 0.0
    , bandAlign = 0.5
    }


defaultCssConfig : CssConfig
defaultCssConfig =
    { block = "sparkline"
    }


defaultColorPairs : List ( Color, Color )
defaultColorPairs =
    Scale.Color.paired
        |> List.foldr
            (\c ( mlast, pairs ) ->
                case mlast of
                    Nothing ->
                        ( Just c, pairs )

                    Just last ->
                        ( Nothing, ( last, c ) :: pairs )
            )
            ( Nothing, [] )
        |> Tuple.second



--------------------------------------------------------------------------------
-- FACET SCALING
--------------------------------------------------------------------------------


type Scaling
    = Fixed
    | Free


type FacetScale a
    = FixedScale a
    | FreeScales (List a)


type alias ScalingConfig =
    { x : Scaling
    , y : Scaling
    }


type alias FacetScales x y =
    { x : FacetScale x
    , y : FacetScale y
    }


lineFacetScales :
    ScalingConfig
    -> Config
    -> List Series
    -> FacetScales (ContinuousScale Time.Posix) (ContinuousScale Float)
lineFacetScales s c seqs =
    let
        ( xr, yr ) =
            ranges c

        tz =
            timeZone c

        xsc =
            case s.x of
                Fixed ->
                    FixedScale <| Facet.fixedTimeScale tz Tuple.first xr seqs

                Free ->
                    FreeScales <| Facet.freeTimeScales tz Tuple.first xr seqs

        ysc =
            case s.y of
                Fixed ->
                    FixedScale <| Facet.fixedLinearScale Tuple.second yr seqs

                Free ->
                    FreeScales <| Facet.freeLinearScales Tuple.second yr seqs
    in
    { x = xsc, y = ysc }


columnFacetScales :
    ScalingConfig
    -> Config
    -> List Series
    -> FacetScales (BandScale Time.Posix) (ContinuousScale Float)
columnFacetScales s c seqs =
    let
        ( xr, yr ) =
            ranges c

        tz =
            timeZone c

        tint =
            timeInterval c

        bc =
            bandConfig c

        xsc =
            case s.x of
                Fixed ->
                    FixedScale <| Facet.fixedTimeBandScale tint tz Tuple.first bc xr seqs

                Free ->
                    FreeScales <| Facet.freeTimeBandScales tint tz Tuple.first bc xr seqs

        ysc =
            case s.y of
                Fixed ->
                    FixedScale <| Facet.fixedLinearScale Tuple.second yr seqs

                Free ->
                    FreeScales <| Facet.freeLinearScales Tuple.second yr seqs
    in
    { x = xsc, y = ysc }



--------------------------------------------------------------------------------
-- LINE VIEWS
--------------------------------------------------------------------------------


lineFacets : ScalingConfig -> Config -> List Series -> List (Svg msg)
lineFacets s c seqs =
    let
        w =
            width c

        h =
            height c

        sc =
            lineFacetScales s c seqs
    in
    case ( sc.x, sc.y ) of
        ( FixedScale xsc, FixedScale ysc ) ->
            List.map
                (\seq ->
                    S.svg
                        [ SA.viewBox 0 0 w h ]
                        [ lineInner Nothing xsc ysc c seq ]
                )
                seqs

        ( FixedScale xsc, FreeScales yscs ) ->
            List.map2
                (\ysc seq ->
                    S.svg
                        [ SA.viewBox 0 0 w h ]
                        [ lineInner Nothing xsc ysc c seq ]
                )
                yscs
                seqs

        ( FreeScales xscs, FixedScale ysc ) ->
            List.map2
                (\xsc seq ->
                    S.svg
                        [ SA.viewBox 0 0 w h ]
                        [ lineInner Nothing xsc ysc c seq ]
                )
                xscs
                seqs

        ( FreeScales xscs, FreeScales yscs ) ->
            List.map3
                (\xsc ysc seq ->
                    S.svg
                        [ SA.viewBox 0 0 w h ]
                        [ lineInner Nothing xsc ysc c seq ]
                )
                xscs
                yscs
                seqs


lines : List ( Color, Color ) -> Config -> List Series -> Svg msg
lines cpairs c seqs =
    let
        w =
            width c

        h =
            height c

        ( xr, yr ) =
            ranges c

        tz =
            timeZone c

        xsc =
            Facet.fixedTimeScale tz Tuple.first xr seqs

        ysc =
            Facet.fixedLinearScale Tuple.second yr seqs

        mcpairs =
            seqs
                |> List.length
                |> List.range 0
                |> List.map (\i -> List.cycle i cpairs |> List.head)

        inners =
            List.map2
                (\mcpair seq ->
                    lineInner mcpair xsc ysc c seq
                )
                mcpairs
                seqs
    in
    S.svg
        [ SA.viewBox 0 0 w h ]
        inners


line : Config -> Series -> Svg msg
line c seq =
    let
        w =
            width c

        h =
            height c

        ( xr, yr ) =
            ranges c

        tz =
            timeZone c

        xsc =
            Facet.fixedTimeScale tz Tuple.first xr [ seq ]

        ysc =
            Facet.fixedLinearScale Tuple.second yr [ seq ]

        inner =
            lineInner Nothing xsc ysc c seq
    in
    S.svg
        [ SA.viewBox 0 0 w h ]
        [ inner ]


lineInner :
    Maybe ( Color, Color )
    -> ContinuousScale Time.Posix
    -> ContinuousScale Float
    -> Config
    -> Series
    -> Svg msg
lineInner mcpair xsc ysc (Config c) data =
    let
        css =
            c.css

        b =
            Bem.init css.block

        e =
            b.element "chart"

        el =
            b.element "line"

        pad =
            c.padding

        ( linecolor, highlightcolor ) =
            mcpair |> Maybe.withDefault c.appearance.color

        linewidth =
            c.appearance.width

        points =
            scaledPoints xsc ysc data

        linepath =
            points |> Shape.line Shape.linearCurve

        highlights =
            data |> selectHighlights c.highlight

        circlesWithLabels =
            case c.label of
                Nothing ->
                    []

                Just render ->
                    labelledCircles
                        b
                        highlightcolor
                        (linewidth * 2.0)
                        render
                        xsc
                        ysc
                        highlights
    in
    S.g
        [ e |> element
        , SA.transform [ Translate pad pad ]
        ]
        (Path.element linepath
            [ el |> element
            , SA.stroke <| Paint <| linecolor
            , SA.strokeWidth <| Px linewidth
            , SA.fill PaintNone
            ]
            :: circlesWithLabels
        )


scaledPoints :
    ContinuousScale Time.Posix
    -> ContinuousScale Float
    -> Series
    -> List (Maybe ( Float, Float ))
scaledPoints xsc ysc =
    List.map (scaledPoint xsc ysc)


scaledPoint :
    ContinuousScale Time.Posix
    -> ContinuousScale Float
    -> Observation
    -> Maybe ( Float, Float )
scaledPoint xsc ysc ( x, y ) =
    Just ( Scale.convert xsc x, Scale.convert ysc y )


selectHighlights : Highlight -> Series -> Series
selectHighlights h data =
    case h of
        NoHighlight ->
            []

        HighlightLast ->
            data |> List.last |> Maybe.map List.singleton |> Maybe.withDefault []

        HighlightNegative ->
            data |> List.filter (\( _, y ) -> y < 0.0)

        HighlightMin ->
            data
                |> Statistics.extentBy Tuple.second
                |> Maybe.map (Tuple.first >> List.singleton)
                |> Maybe.withDefault []

        HighlightMax ->
            data
                |> Statistics.extentBy Tuple.second
                |> Maybe.map (Tuple.second >> List.singleton)
                |> Maybe.withDefault []

        HighlightMinMax ->
            data
                |> Statistics.extentBy Tuple.second
                |> Maybe.map (\( a, b ) -> [ a, b ])
                |> Maybe.withDefault []

        HighlightPeaks spec ->
            data |> Statistics.peaks Tuple.second spec


labelledCircles :
    Bem.Block
    -> Color
    -> Float
    -> (( Time.Posix, Float ) -> String)
    -> ContinuousScale Time.Posix
    -> ContinuousScale Float
    -> Series
    -> List (Svg msg)
labelledCircles b fillcolor radius tolabel xsc ysc data =
    let
        points =
            data |> scaledPoints xsc ysc

        lbls =
            data |> List.map tolabel
    in
    List.map2 (labelledCircle b fillcolor radius)
        lbls
        points
        |> List.filterMap identity


labelledCircle : Bem.Block -> Color -> Float -> String -> Maybe ( Float, Float ) -> Maybe (Svg msg)
labelledCircle b fillcolor radius lbl mpoint =
    case mpoint of
        Nothing ->
            Nothing

        Just ( x, y ) ->
            let
                e =
                    b.element "highlight"

                ep =
                    b.element "point"

                el =
                    b.element "label"
            in
            Just <|
                S.g [ e |> element ]
                    [ pointToCircle ep fillcolor radius ( x, y )
                    , labelAtPoint el lbl ( x, y )
                    ]


pointToCircle : Bem.Element -> Color -> Float -> ( Float, Float ) -> Svg msg
pointToCircle e fillcolor radius ( x, y ) =
    S.g [ e |> element ]
        [ S.circle
            [ SA.cx <| Px <| x
            , SA.cy <| Px <| y
            , SA.r <| Px radius
            , SA.fill <| Paint <| fillcolor
            , SA.strokeWidth <| Px 0
            , SA.stroke <| PaintNone
            ]
            []
        ]


labelAtPoint : Bem.Element -> String -> ( Float, Float ) -> Svg msg
labelAtPoint e lbl ( x, y ) =
    S.text_
        [ e |> element
        , SA.transform [ Translate x y ]
        , SA.dy (Em (y - 0.35))
        , SA.textAnchor AnchorMiddle
        ]
        [ text lbl ]



--------------------------------------------------------------------------------
-- COLUMN VIEWS
--------------------------------------------------------------------------------


columnFacets : ScalingConfig -> Config -> List Series -> List (Svg msg)
columnFacets s c seqs =
    let
        w =
            width c

        h =
            height c

        tint =
            timeInterval c

        tz =
            timeZone c

        agg =
            aggregate c

        seqs_ =
            case s.x of
                Free ->
                    seqs
                        |> List.map (Timeseries.byIntervals agg tint tz)

                Fixed ->
                    seqs |> Timeseries.byIntervalsMultiple agg tint tz

        sc =
            columnFacetScales s c seqs_
    in
    case ( sc.x, sc.y ) of
        ( FixedScale xsc, FixedScale ysc ) ->
            List.map
                (\seq ->
                    S.svg
                        [ SA.viewBox 0 0 w h ]
                        [ columnsInner h xsc ysc c seq ]
                )
                seqs_

        ( FixedScale xsc, FreeScales yscs ) ->
            List.map2
                (\ysc seq ->
                    S.svg
                        [ SA.viewBox 0 0 w h ]
                        [ columnsInner h xsc ysc c seq ]
                )
                yscs
                seqs_

        ( FreeScales xscs, FixedScale ysc ) ->
            List.map2
                (\xsc seq ->
                    S.svg
                        [ SA.viewBox 0 0 w h ]
                        [ columnsInner h xsc ysc c seq ]
                )
                xscs
                seqs_

        ( FreeScales xscs, FreeScales yscs ) ->
            List.map3
                (\xsc ysc seq ->
                    S.svg
                        [ SA.viewBox 0 0 w h ]
                        [ columnsInner h xsc ysc c seq ]
                )
                xscs
                yscs
                seqs_


columns : (List Float -> Float) -> Time.Interval -> Config -> Series -> Svg msg
columns fn tint c data =
    let
        tz =
            timeZone c

        bc =
            bandConfig c

        w =
            width c

        h =
            height c

        ( xr, yr ) =
            ranges c

        data_ =
            Timeseries.byIntervals fn tint tz data

        xsc =
            Facet.fixedTimeBandScale tint tz Tuple.first bc yr [ data_ ]

        ysc =
            Facet.fixedLinearScale Tuple.second yr [ data_ ]

        inner =
            columnsInner h xsc ysc c data_
    in
    S.svg
        [ SA.viewBox 0 0 w h ]
        [ inner ]


columnsInner : Float -> BandScale Time.Posix -> ContinuousScale Float -> Config -> Series -> Svg msg
columnsInner h xsc ysc (Config c) data =
    let
        pad =
            c.padding

        css =
            c.css

        b =
            Bem.init css.block

        e =
            b.element "columns"

        ec =
            b.element "column"

        cpair =
            c.appearance.color

        highlights =
            data |> selectHighlights c.highlight
    in
    S.g
        [ SA.transform [ Translate pad pad ], e |> element ]
    <|
        List.map (columnInner ec cpair h pad xsc ysc highlights) data


columnInner :
    Bem.Element
    -> ( Color, Color )
    -> Float
    -> Float
    -> BandScale Time.Posix
    -> ContinuousScale Float
    -> Series
    -> Observation
    -> Svg msg
columnInner e ( cbar, chigh ) h pad xsc ysc highlights ( x_, y_ ) =
    let
        ishigh =
            highlights
                |> List.any
                    (\( xh, _ ) -> Time.posixToMillis xh == Time.posixToMillis x_)
    in
    S.g
        [ e |> elementList [ ( "highlighted", ishigh ) ] ]
        [ S.rect
            [ SA.x <| Px <| Scale.convert xsc x_
            , SA.y <| Px <| Scale.convert ysc y_
            , SA.width <| Px <| Scale.bandwidth xsc
            , SA.height <| Px <| h - Scale.convert ysc y_ - 2 * pad
            , SA.fill <|
                Paint <|
                    if ishigh then
                        chigh

                    else
                        cbar
            ]
            []
        ]
