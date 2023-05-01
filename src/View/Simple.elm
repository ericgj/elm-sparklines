module View.Simple exposing
    ( Config
    , Highlight(..)
    , columnFacets
    , columns
    , columnsConfig
    , defaultColorPairs
    , line
    , lineConfig
    , lineFacets
    , lines
    , withBandConfig
    , withHighlight
    , withPadding
    )

import Color exposing (Color)
import Facet exposing (Scale(..), Scaling(..))
import List.Extra as List
import Path exposing (Path)
import Scale exposing (BandScale, ContinuousScale)
import Scale.Color
import Shape
import Statistics
import Svg.Bem as Bem exposing (element, elementList)
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


lineConfig : Time.Zone -> Float -> Float -> Config
lineConfig z w h =
    Config
        { width = w
        , height = h
        , padding = 0.0
        , timeZone = z
        , interval = Time.Day
        , aggregate = List.sum
        , highlight = NoHighlight
        , appearance = defaultLineAppearanceConfig
        , css = defaultCssConfig
        }


columnsConfig : (List Float -> Float) -> Time.Interval -> Time.Zone -> Float -> Float -> Config
columnsConfig fn tint z w h =
    Config
        { width = w
        , height = h
        , padding = 0.0
        , timeZone = z
        , interval = tint
        , aggregate = fn
        , highlight = NoHighlight
        , appearance = defaultColumnsAppearanceConfig
        , css = defaultCssConfig
        }


defaultLineAppearanceConfig : AppearanceConfig
defaultLineAppearanceConfig =
    { width = 1.0
    , color = ( Color.rgb 0 0 0, Color.rgb 1 0 0 )
    , bandPaddingInner = 0.0
    , bandPaddingOuter = 0.0
    , bandAlign = 0.5
    }


defaultColumnsAppearanceConfig : AppearanceConfig
defaultColumnsAppearanceConfig =
    { width = 1.0
    , color = ( Color.rgba 0 0 0 0.75, Color.rgb 1 0 0 )
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
    Scale.Color.category10
        |> List.map
            (\c ->
                ( adjustColorAlpha 0.7 c, c )
            )


adjustColorAlpha : Float -> Color -> Color
adjustColorAlpha a =
    Color.toRgba
        >> (\rec -> { rec | alpha = rec.alpha * a })
        >> Color.fromRgba


withPadding : Float -> Config -> Config
withPadding p (Config c) =
    Config { c | padding = p }


withHighlight : Highlight -> Config -> Config
withHighlight h (Config c) =
    Config { c | highlight = h }


withBandConfig : Scale.BandConfig -> Config -> Config
withBandConfig bc (Config c) =
    Config { c | appearance = updateAppearanceBandConfig bc c.appearance }


updateAppearanceBandConfig : Scale.BandConfig -> AppearanceConfig -> AppearanceConfig
updateAppearanceBandConfig bc ac =
    { ac
        | bandPaddingInner = bc.paddingInner
        , bandPaddingOuter = bc.paddingOuter
        , bandAlign = bc.align
    }



--------------------------------------------------------------------------------
-- FACET SCALING
--------------------------------------------------------------------------------


lineScales :
    Facet.Scaling2d
    -> Config
    -> List Series
    -> Facet.Scale2d (ContinuousScale Time.Posix) (ContinuousScale Float)
lineScales s c seqs =
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


columnScales :
    Facet.Scaling2d
    -> Config
    -> List Series
    -> Facet.Scale2d (BandScale Time.Posix) (ContinuousScale Float)
columnScales s c seqs =
    let
        ( xr, yr ) =
            ranges c

        bc =
            bandConfig c

        xsc =
            case s.x of
                Fixed ->
                    FixedScale <| Facet.fixedTimeBandScale bc Tuple.first xr seqs

                Free ->
                    FreeScales <| Facet.freeTimeBandScales bc Tuple.first xr seqs

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


lineFacets : Facet.Scaling2d -> Config -> List Series -> List (Svg msg)
lineFacets s c seqs =
    let
        w =
            width c

        h =
            height c

        sc =
            lineScales s c seqs
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
                |> List.map
                    (\i ->
                        List.cycle (i + 1) cpairs |> List.drop i |> List.head
                    )

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
            Facet.timeScale tz Tuple.first xr seq

        ysc =
            Facet.linearScale Tuple.second yr seq

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

        circles =
            highlightCircles
                b
                highlightcolor
                (linewidth * 2.0)
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
            :: circles
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


highlightCircles :
    Bem.Block
    -> Color
    -> Float
    -> ContinuousScale Time.Posix
    -> ContinuousScale Float
    -> Series
    -> List (Svg msg)
highlightCircles b fillcolor radius xsc ysc data =
    let
        points =
            data |> scaledPoints xsc ysc
    in
    points
        |> List.map (Maybe.map (highlightCircle b fillcolor radius))
        |> List.filterMap identity


highlightCircle : Bem.Block -> Color -> Float -> ( Float, Float ) -> Svg msg
highlightCircle b fillcolor radius ( x, y ) =
    let
        e =
            b.element "highlight"

        ep =
            b.element "point"
    in
    S.g [ e |> element ]
        [ pointToCircle ep fillcolor radius ( x, y )
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



--------------------------------------------------------------------------------
-- COLUMN VIEWS
--------------------------------------------------------------------------------


columnFacets : Facet.Scaling2d -> Config -> List Series -> List (Svg msg)
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
                        |> List.map (Timeseries.groupByIntervals agg tint tz)

                Fixed ->
                    seqs |> Timeseries.groupByIntervalsMultiple agg tint tz

        sc =
            columnScales s c seqs_
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


columns : Config -> Series -> Svg msg
columns c data =
    let
        tint =
            timeInterval c

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

        fn =
            aggregate c

        data_ =
            Timeseries.groupByIntervals fn tint tz data

        xsc =
            Facet.timeBandScale bc Tuple.first xr data_

        ysc =
            Facet.linearScale Tuple.second yr data_

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
columnInner e ( cbar, chigh ) h pad xsc ysc highlights ( x, y ) =
    let
        ishigh =
            highlights
                |> List.any
                    (\( xh, _ ) -> Time.posixToMillis xh == Time.posixToMillis x)
    in
    S.g
        [ e |> elementList [ ( "highlighted", ishigh ) ] ]
        [ S.rect
            [ SA.x <| Px <| Scale.convert xsc x
            , SA.y <| Px <| Scale.convert ysc y
            , SA.width <| Px <| Scale.bandwidth xsc
            , SA.height <| Px <| (h - Scale.convert ysc y) - (2 * pad)
            , SA.fill <|
                Paint <|
                    if ishigh then
                        chigh

                    else
                        cbar
            ]
            []
        ]
