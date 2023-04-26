module Sparkline exposing
    ( Config
    , Highlight(..)
    , defaultColorPairs
    , defaultLineConfig
    , line
    , lineFacets
    , lines
    )

import Color exposing (Color)
import Html.Bem as Bem exposing (element)
import List.Extra as List
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Scale.Color
import Scale.Facet exposing (Scales, ScalingConfig)
import Shape
import Statistics
import Time
import TypedSvg exposing (circle, g, svg, text_)
import TypedSvg.Attributes exposing (class, cx, cy, dy, fill, opacity, r, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (strokeWidth)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Opacity(..), Paint(..), Transform(..))



-- CONFIG


type Config
    = Config ConfigData


type alias ConfigData =
    { width : Float
    , height : Float
    , padding : Float
    , timeZone : Time.Zone
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
    }


type alias CssConfig =
    { block : String
    }


height : Config -> Float
height (Config c) =
    c.height


width : Config -> Float
width (Config c) =
    c.width


timeZone : Config -> Time.Zone
timeZone (Config c) =
    c.timeZone


chartDimensions : Config -> Scale.Facet.ChartDimensions
chartDimensions (Config c) =
    { width = c.width
    , height = c.height
    , padding = c.padding
    }


defaultLineConfig : Time.Zone -> Float -> Float -> Config
defaultLineConfig z w h =
    Config
        { width = w
        , height = h
        , padding = 10.0
        , timeZone = z
        , highlight = NoHighlight
        , label = Nothing
        , appearance = defaultLineAppearanceConfig
        , css = defaultCssConfig
        }


defaultLineAppearanceConfig : AppearanceConfig
defaultLineAppearanceConfig =
    { width = 1.0
    , color = ( Color.rgb 0 0 0, Color.rgb 1 0 0 )
    , labelPadding = Em 0.35
    , bandPaddingInner = 0.0
    , bandPaddingOuter = 0.0
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



-- DATA TYPE


type alias Data =
    List Datum


type alias Datum =
    ( Time.Posix, Float )



-- LINE VIEWS


lineFacets : ScalingConfig -> Config -> List Data -> List (Svg msg)
lineFacets s c seqs =
    let
        scs =
            scaleTimeseriesFacets s c seqs
    in
    List.map2
        (\sc seq ->
            svg
                [ viewBox 0 0 sc.width sc.height ]
                [ lineInner Nothing sc c seq ]
        )
        scs
        seqs


lines : List ( Color, Color ) -> Config -> List Data -> Svg msg
lines cpairs c seqs =
    let
        w =
            width c

        h =
            height c

        mcpairs =
            seqs
                |> List.length
                |> List.range 0
                |> List.map (\i -> List.cycle i cpairs |> List.head)

        sc =
            scaleTimeseries c seqs

        inners =
            List.map2
                (\mcpair seq ->
                    lineInner mcpair sc c seq
                )
                mcpairs
                seqs
    in
    svg
        [ viewBox 0 0 w h ]
        inners


line : Config -> Data -> Svg msg
line c seq =
    let
        w =
            width c

        h =
            height c

        sc =
            scaleTimeseries c [ seq ]

        inner =
            lineInner Nothing sc c seq
    in
    svg
        [ viewBox 0 0 w h ]
        [ inner ]


lineInner : Maybe ( Color, Color ) -> Scales Time.Posix Float -> Config -> Data -> Svg msg
lineInner mcpair sc (Config c) data =
    let
        css =
            c.css

        b =
            Bem.init css.block

        e =
            b.element "chart"

        el =
            b.element "line"

        w =
            sc.width

        h =
            sc.height

        pad =
            sc.padding

        ( linecolor, highlightcolor ) =
            mcpair |> Maybe.withDefault c.appearance.color

        linewidth =
            c.appearance.width

        points =
            scaledPoints sc data

        linepath =
            points |> Shape.line Shape.linearCurve

        highlights =
            data |> selectHighlights c.highlight

        circlesWithLabels =
            case c.label of
                Nothing ->
                    []

                Just render ->
                    labelledCircles b highlightcolor (linewidth * 2.0) render sc highlights
    in
    g
        [ e |> element
        , transform [ Translate pad pad ]
        ]
        (Path.element linepath
            [ el |> element
            , stroke <| Paint <| linecolor
            , strokeWidth linewidth
            , fill PaintNone
            ]
            :: circlesWithLabels
        )


selectHighlights : Highlight -> Data -> Data
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
    -> Scales Time.Posix Float
    -> Data
    -> List (Svg msg)
labelledCircles b fillcolor radius tolabel sc data =
    let
        points =
            data |> scaledPoints sc

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
                g [ e |> element ]
                    [ pointToCircle ep fillcolor radius ( x, y )
                    , labelAtPoint el lbl ( x, y )
                    ]


pointToCircle : Bem.Element -> Color -> Float -> ( Float, Float ) -> Svg msg
pointToCircle e fillcolor radius ( x, y ) =
    g [ e |> element ]
        [ circle
            [ cx <| Px <| x
            , cy <| Px <| y
            , r <| Px radius
            , fill <| Paint <| fillcolor
            , strokeWidth 0
            , stroke <| PaintNone
            ]
            []
        ]


labelAtPoint : Bem.Element -> String -> ( Float, Float ) -> Svg msg
labelAtPoint e lbl ( x, y ) =
    text_
        [ e |> element
        , transform [ Translate x y ]
        , dy (Em (y - 0.35))
        , textAnchor AnchorMiddle
        ]
        [ text lbl ]



-- SCALING HELPERS


scaleTimeseriesFacets : ScalingConfig -> Config -> List Data -> List (Scales Time.Posix Float)
scaleTimeseriesFacets s c seqs =
    Scale.Facet.scales
        (Scale.Facet.timeseriesConfig
            (timeZone c)
            s
        )
        (chartDimensions c)
        seqs


scaleTimeseries : Config -> List Data -> Scales Time.Posix Float
scaleTimeseries c seqs =
    Scale.Facet.scalesFixed
        (Scale.Facet.timeseriesConfig
            (timeZone c)
            Scale.Facet.fixedScaling
        )
        (chartDimensions c)
        seqs



-- GENERIC HELPERS


extentFloatOrEmpty : List Float -> ( Float, Float )
extentFloatOrEmpty =
    Statistics.extent >> Maybe.withDefault emptyFloatRange


extentTimeOrEmpty : List Time.Posix -> ( Time.Posix, Time.Posix )
extentTimeOrEmpty =
    Statistics.extentBy Time.posixToMillis >> Maybe.withDefault emptyTimeRange


emptyFloatRange : ( Float, Float )
emptyFloatRange =
    ( 0.0, 0.0 )


emptyTimeRange : ( Time.Posix, Time.Posix )
emptyTimeRange =
    ( Time.millisToPosix 0, Time.millisToPosix 0 )


scaledPoints : Scales Time.Posix Float -> Data -> List (Maybe ( Float, Float ))
scaledPoints sc =
    List.map (scaledPoint sc)


scaledPoint : Scales Time.Posix Float -> Datum -> Maybe ( Float, Float )
scaledPoint sc ( x, y ) =
    Just ( Scale.convert sc.xScale x, Scale.convert sc.yScale y )
