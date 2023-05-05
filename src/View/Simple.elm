module View.Simple exposing
    ( BrushingAppearanceConfig
    , BrushingLabelsConfig
    , Config
    , Highlight(..)
    , columnFacets
    , columns
    , columnsConfig
    , defaultBrushingAppearance
    , defaultBrushingLabels
    , defaultColorPairs
    , line
    , lineConfig
    , lineFacets
    , lines
    , selectBrushed
    , withBandConfig
    , withBrush
    , withBrushLabels
    , withBrushLabelsX
    , withBrushLabelsY
    , withHighlight
    , withPadding
    )

import Brush exposing (Brush, OnBrush, OneDimensional)
import Color exposing (Color)
import Facet exposing (Scale(..), Scaling(..))
import List.Extra as List
import Path exposing (Path)
import Scale exposing (BandScale, ContinuousScale)
import Scale.Color
import Shape
import Statistics
import Svg.Bem as Bem exposing (element, elementIf, elementList, elementOf, elementOfList)
import Time
import Time.Extra as Time
import Timeseries exposing (Observation, Series)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core exposing (Attribute, Svg, text)
import TypedSvg.Types
    exposing
        ( AlignmentBaseline(..)
        , AnchorAlignment(..)
        , Length(..)
        , Opacity(..)
        , Paint(..)
        , ShapeRendering(..)
        , Transform(..)
        )



--------------------------------------------------------------------------------
-- CONFIG
--------------------------------------------------------------------------------


type Config msg
    = Config (ConfigData msg)


type alias ConfigData msg =
    { width : Float
    , height : Float
    , padding : Float
    , timeZone : Time.Zone
    , interval : Time.Interval
    , aggregate : List Float -> Float
    , highlight : Highlight
    , brushing : Brushing msg
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


type Brushing msg
    = NoBrush
    | BrushNoLabels (BrushingNoLabelsData msg)
    | BrushLabels (BrushingLabelsData msg)
    | BrushLabelsX (BrushingLabelsData msg)
    | BrushLabelsY (BrushingLabelsData msg)


type alias BrushingNoLabelsData msg =
    { onBrush : OnBrush -> msg
    , appearance : BrushingAppearanceConfig
    }


type alias BrushingLabelsData msg =
    { onBrush : OnBrush -> msg
    , appearance : BrushingAppearanceConfig
    , labels : BrushingLabelsConfig
    }


type alias AppearanceConfig =
    { width : Float
    , color : Paint
    , highlightColor : Paint
    , bandPaddingInner : Float
    , bandPaddingOuter : Float
    , bandAlign : Float
    }


type alias BrushingAppearanceConfig =
    { area : Paint
    , bounds : Paint
    , boundsDashed : Bool
    , highlight : Paint
    }


type alias BrushingLabelsConfig =
    { color : Paint
    , size : Length
    , toString : Float -> String
    }


type alias CssConfig =
    { block : String
    }



-- GETTERS


height : Config msg -> Float
height (Config c) =
    c.height


width : Config msg -> Float
width (Config c) =
    c.width


padding : Config msg -> Float
padding (Config c) =
    c.padding


aggregate : Config msg -> (List Float -> Float)
aggregate (Config c) =
    c.aggregate


timeInterval : Config msg -> Time.Interval
timeInterval (Config c) =
    c.interval


timeZone : Config msg -> Time.Zone
timeZone (Config c) =
    c.timeZone


highlight : Config msg -> Highlight
highlight (Config c) =
    c.highlight


brushing : Config msg -> Brushing msg
brushing (Config c) =
    c.brushing


onBrush : Config msg -> Maybe (OnBrush -> msg)
onBrush (Config c) =
    case c.brushing of
        NoBrush ->
            Nothing

        BrushNoLabels d ->
            Just d.onBrush

        BrushLabels d ->
            Just d.onBrush

        BrushLabelsX d ->
            Just d.onBrush

        BrushLabelsY d ->
            Just d.onBrush


color : Config msg -> Paint
color (Config c) =
    c.appearance.color


highlightColor : Config msg -> Paint
highlightColor (Config c) =
    c.appearance.highlightColor


observationWidth : Config msg -> Float
observationWidth (Config c) =
    c.appearance.width


ranges : Config msg -> ( ( Float, Float ), ( Float, Float ) )
ranges (Config c) =
    ( ( 0, c.width - 2 * c.padding )
    , ( c.height - 2 * c.padding, 0 )
    )


bandConfig : Config msg -> Scale.BandConfig
bandConfig (Config c) =
    { paddingInner = c.appearance.bandPaddingInner
    , paddingOuter = c.appearance.bandPaddingOuter
    , align = c.appearance.bandAlign
    }


cssBlock : Config msg -> Bem.Block
cssBlock (Config c) =
    Bem.init c.css.block



-- CONSTRUCTORS


lineConfig : Time.Interval -> Time.Zone -> Float -> Float -> Config msg
lineConfig tint tz w h =
    Config
        { width = w
        , height = h
        , padding = 0.0
        , timeZone = tz
        , interval = tint
        , aggregate = List.sum
        , highlight = NoHighlight
        , brushing = NoBrush
        , appearance = defaultLineAppearanceConfig
        , css = defaultCssConfig
        }


columnsConfig : (List Float -> Float) -> Time.Interval -> Time.Zone -> Float -> Float -> Config msg
columnsConfig fn tint tz w h =
    Config
        { width = w
        , height = h
        , padding = 0.0
        , timeZone = tz
        , interval = tint
        , aggregate = fn
        , highlight = NoHighlight
        , brushing = NoBrush
        , appearance = defaultColumnsAppearanceConfig
        , css = defaultCssConfig
        }


defaultLineAppearanceConfig : AppearanceConfig
defaultLineAppearanceConfig =
    { width = 1.0
    , color = Paint <| Color.rgb 0 0 0
    , highlightColor = Paint <| Color.rgb 1 0 0
    , bandPaddingInner = 0.0
    , bandPaddingOuter = 0.0
    , bandAlign = 0.5
    }


defaultColumnsAppearanceConfig : AppearanceConfig
defaultColumnsAppearanceConfig =
    { width = 1.0
    , color = Paint <| Color.rgb 0 0 0
    , highlightColor = Paint <| Color.rgb 1 0 0
    , bandPaddingInner = 0.0
    , bandPaddingOuter = 0.0
    , bandAlign = 0.5
    }


defaultCssConfig : CssConfig
defaultCssConfig =
    { block = "sparkline"
    }


defaultBrushingAppearance : BrushingAppearanceConfig
defaultBrushingAppearance =
    { area = Paint <| Color.rgb 1 1 0
    , bounds = Paint <| Color.rgba 0 0 0 0.5
    , boundsDashed = False
    , highlight = Paint <| Color.rgb 1 0 0
    }


defaultBrushingLabels : BrushingLabelsConfig
defaultBrushingLabels =
    { color = Paint <| Color.rgb 0 0 0
    , size = Rem 0.7
    , toString = round >> String.fromInt
    }


defaultColorPairs : List ( Paint, Paint )
defaultColorPairs =
    Scale.Color.category10
        |> List.map
            (\c ->
                ( Paint <| adjustColorAlpha 0.7 c, Paint c )
            )


adjustColorAlpha : Float -> Color -> Color
adjustColorAlpha a =
    Color.toRgba
        >> (\rec -> { rec | alpha = rec.alpha * a })
        >> Color.fromRgba


withPadding : Float -> Config msg -> Config msg
withPadding p (Config c) =
    Config { c | padding = p }


withHighlight : Highlight -> Config msg -> Config msg
withHighlight h (Config c) =
    Config { c | highlight = h }


withBrush : (OnBrush -> msg) -> BrushingAppearanceConfig -> Config msg -> Config msg
withBrush onbrush bac (Config c) =
    let
        br =
            BrushNoLabels { onBrush = onbrush, appearance = bac }
    in
    Config { c | brushing = br }


withBrushLabels :
    (OnBrush -> msg)
    -> BrushingAppearanceConfig
    -> BrushingLabelsConfig
    -> Config msg
    -> Config msg
withBrushLabels =
    withBrushLabelsHelp BrushLabels


withBrushLabelsX :
    (OnBrush -> msg)
    -> BrushingAppearanceConfig
    -> BrushingLabelsConfig
    -> Config msg
    -> Config msg
withBrushLabelsX =
    withBrushLabelsHelp BrushLabelsX


withBrushLabelsY :
    (OnBrush -> msg)
    -> BrushingAppearanceConfig
    -> BrushingLabelsConfig
    -> Config msg
    -> Config msg
withBrushLabelsY =
    withBrushLabelsHelp BrushLabelsY


withBrushLabelsHelp :
    (BrushingLabelsData msg -> Brushing msg)
    -> (OnBrush -> msg)
    -> BrushingAppearanceConfig
    -> BrushingLabelsConfig
    -> Config msg
    -> Config msg
withBrushLabelsHelp constr onbrush bac blc (Config c) =
    let
        br =
            constr { onBrush = onbrush, appearance = bac, labels = blc }
    in
    Config { c | brushing = br }


withBandConfig : Scale.BandConfig -> Config msg -> Config msg
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
    -> Config msg
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
    -> Config msg
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



-- -----------------------------------------------------------------------------
-- HIGHLIGHTS
-- -----------------------------------------------------------------------------


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


isHighlightedTimeInterval :
    (Time.Interval -> Time.Zone -> Time.Posix -> Time.Posix)
    -> Time.Interval
    -> Time.Zone
    -> Time.Posix
    -> Series
    -> Bool
isHighlightedTimeInterval fn tint tz x =
    List.any
        (\( x_, _ ) ->
            fn tint tz x_ == fn tint tz x
        )



-- -----------------------------------------------------------------------------
-- BRUSHING
-- -----------------------------------------------------------------------------


extentMaybeBrushed :
    Maybe (Brush OneDimensional)
    -> ContinuousScale Time.Posix
    -> Maybe ( Time.Posix, Time.Posix )
extentMaybeBrushed mbrush xsc =
    mbrush |> Maybe.andThen (\br -> extentBrushed br xsc)


selectMaybeBrushed :
    Maybe (Brush OneDimensional)
    -> ContinuousScale Time.Posix
    -> Series
    -> Maybe Series
selectMaybeBrushed mbrush xsc data =
    mbrush |> Maybe.andThen (\br -> selectBrushed br xsc data)


selectBrushed :
    Brush OneDimensional
    -> ContinuousScale Time.Posix
    -> Series
    -> Maybe Series
selectBrushed br xsc data =
    extentBrushed br xsc
        |> Maybe.map (\ext -> selectInTimeExtent ext data)


extentBrushed :
    Brush OneDimensional
    -> ContinuousScale Time.Posix
    -> Maybe ( Time.Posix, Time.Posix )
extentBrushed br xsc =
    let
        invert =
            Scale.invert xsc
    in
    br
        |> Brush.selection1d
        |> Maybe.map (Tuple.mapBoth invert invert)


selectInTimeExtent : ( Time.Posix, Time.Posix ) -> Series -> Series
selectInTimeExtent ( tmin, tmax ) =
    List.filter
        (\( x, _ ) ->
            let
                nx =
                    Time.posixToMillis x
            in
            Time.posixToMillis tmin <= nx && nx <= Time.posixToMillis tmax
        )


viewBrush : Bem.Block -> Float -> Maybe (OnBrush -> msg) -> Maybe (Brush OneDimensional) -> List (Svg msg)
viewBrush b pad mmsg mbrush =
    let
        e =
            b.element "brush"

        brushrect ext attrs =
            S.rect
                ((SA.x <| Px <| ext.left)
                    :: (SA.y <| Px <| ext.top)
                    :: (SA.width <| Px <| ext.right - ext.left)
                    :: (SA.height <| Px <| ext.bottom - ext.top)
                    :: (SA.fillOpacity <| Opacity <| 0.0)
                    :: SA.shapeRendering RenderCrispEdges
                    :: attrs
                )
                []

        inner msg br =
            S.g
                [ e |> element ]
                [ Brush.view
                    [ Brush.selectedArea brushrect ]
                    msg
                    br
                ]
    in
    Maybe.map2 inner mmsg mbrush
        |> Maybe.map List.singleton
        |> Maybe.withDefault []



-- -----------------------------------------------------------------------------
-- SCALING
-- -----------------------------------------------------------------------------


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


scaledAreaBounds :
    Float
    -> ContinuousScale Time.Posix
    -> ContinuousScale Float
    -> Series
    -> List (Maybe ( ( Float, Float ), ( Float, Float ) ))
scaledAreaBounds yadj xsc ysc =
    List.map (scaledArea yadj xsc ysc)


scaledArea :
    Float
    -> ContinuousScale Time.Posix
    -> ContinuousScale Float
    -> Observation
    -> Maybe ( ( Float, Float ), ( Float, Float ) )
scaledArea yadj xsc ysc ( x, y ) =
    let
        ymin =
            Scale.rangeExtent ysc |> Tuple.first
    in
    Just
        ( ( Scale.convert xsc x, ymin )
        , ( Scale.convert xsc x, Scale.convert ysc y - yadj )
        )


scaledVerticalLinePoints :
    ContinuousScale Time.Posix
    -> ContinuousScale Float
    -> Observation
    -> List (Maybe ( Float, Float ))
scaledVerticalLinePoints xsc ysc ( x, y ) =
    let
        cx =
            Scale.convert xsc x
    in
    [ Just ( cx, Scale.rangeExtent ysc |> Tuple.first )
    , Just ( cx, Scale.convert ysc y )
    ]


scaledHorizontalLinePoints :
    ContinuousScale Time.Posix
    -> ContinuousScale Float
    -> Observation
    -> List (Maybe ( Float, Float ))
scaledHorizontalLinePoints xsc ysc ( x, y ) =
    let
        xmin =
            Scale.rangeExtent xsc |> Tuple.first

        cy =
            Scale.convert ysc y
    in
    [ Just ( xmin, cy )
    , Just ( Scale.convert xsc x, cy )
    ]



-- -----------------------------------------------------------------------------
-- LINE CHARTS
-- -----------------------------------------------------------------------------


lineFacets :
    Facet.Scaling2d
    -> Config msg
    -> Maybe (Brush OneDimensional)
    -> List Series
    -> List (Svg msg)
lineFacets s c mbrush seqs =
    let
        b =
            cssBlock c

        w =
            width c

        h =
            height c

        pad =
            padding c

        brush =
            viewBrush b pad (onBrush c) mbrush

        sc =
            lineScales s c seqs
    in
    case ( sc.x, sc.y ) of
        ( FixedScale xsc, FixedScale ysc ) ->
            List.map
                (\seq ->
                    S.svg
                        [ SA.viewBox 0 0 w h ]
                        ([ lineInner Nothing xsc ysc c mbrush seq ]
                            ++ brush
                        )
                )
                seqs

        ( FixedScale xsc, FreeScales yscs ) ->
            List.map2
                (\ysc seq ->
                    S.svg
                        [ SA.viewBox 0 0 w h ]
                        ([ lineInner Nothing xsc ysc c mbrush seq ]
                            ++ brush
                        )
                )
                yscs
                seqs

        ( FreeScales xscs, FixedScale ysc ) ->
            List.map2
                (\xsc seq ->
                    S.svg
                        [ SA.viewBox 0 0 w h ]
                        ([ lineInner Nothing xsc ysc c mbrush seq ]
                            ++ brush
                        )
                )
                xscs
                seqs

        ( FreeScales xscs, FreeScales yscs ) ->
            List.map3
                (\xsc ysc seq ->
                    S.svg
                        [ SA.viewBox 0 0 w h ]
                        ([ lineInner Nothing xsc ysc c mbrush seq ]
                            ++ brush
                        )
                )
                xscs
                yscs
                seqs


lines :
    List ( Paint, Paint )
    -> Config msg
    -> Maybe (Brush OneDimensional)
    -> List Series
    -> Svg msg
lines cpairs c mbrush seqs =
    let
        b =
            cssBlock c

        w =
            width c

        h =
            height c

        pad =
            padding c

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
                    lineInner mcpair xsc ysc c mbrush seq
                )
                mcpairs
                seqs

        brush =
            viewBrush b pad (onBrush c) mbrush
    in
    S.svg
        [ SA.viewBox 0 0 w h ]
        (inners ++ brush)


line : Config msg -> Maybe (Brush OneDimensional) -> Series -> Svg msg
line c mbrush seq =
    let
        b =
            cssBlock c

        w =
            width c

        h =
            height c

        pad =
            padding c

        ( xr, yr ) =
            ranges c

        tz =
            timeZone c

        xsc =
            Facet.timeScale tz Tuple.first xr seq

        ysc =
            Facet.linearScale Tuple.second yr seq

        inner =
            lineInner Nothing xsc ysc c mbrush seq

        brush =
            viewBrush b pad (onBrush c) mbrush
    in
    S.svg
        [ SA.viewBox 0 0 w h ]
        ([ inner ] ++ brush)


lineInner :
    Maybe ( Paint, Paint )
    -> ContinuousScale Time.Posix
    -> ContinuousScale Float
    -> Config msg
    -> Maybe (Brush OneDimensional)
    -> Series
    -> Svg msg
lineInner mcpair xsc ysc c mbrush data =
    let
        b =
            cssBlock c

        e =
            b.element "chart"

        el =
            b.element "line"

        pad =
            padding c

        ( linecolor, highlightcolor ) =
            mcpair
                |> Maybe.withDefault ( color c, highlightColor c )

        linewidth =
            observationWidth c

        highlights =
            data |> selectHighlights (highlight c)

        linepath =
            data
                |> scaledPoints xsc ysc
                |> Shape.line Shape.linearCurve

        brushoverlay =
            mbrush
                |> Maybe.andThen
                    (\brush -> lineBrushOverlay b c xsc ysc brush highlights data)

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
            , SA.stroke <| linecolor
            , SA.strokeWidth <| Px linewidth
            , SA.fill PaintNone
            ]
            :: (brushoverlay |> Maybe.withDefault (text ""))
            :: circles
        )


highlightCircles :
    Bem.Block
    -> Paint
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


highlightCircle : Bem.Block -> Paint -> Float -> ( Float, Float ) -> Svg msg
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


pointToCircle : Bem.Element -> Paint -> Float -> ( Float, Float ) -> Svg msg
pointToCircle e fillcolor radius ( x, y ) =
    S.g [ e |> element ]
        [ S.circle
            [ SA.cx <| Px <| x
            , SA.cy <| Px <| y
            , SA.r <| Px radius
            , SA.fill <| fillcolor
            , SA.strokeWidth <| Px 0
            , SA.stroke <| PaintNone
            ]
            []
        ]



-- -----------------------------------------------------------------------------
-- BRUSH OVERLAY
-- -----------------------------------------------------------------------------


lineBrushOverlay :
    Bem.Block
    -> Config msg
    -> ContinuousScale Time.Posix
    -> ContinuousScale Float
    -> Brush OneDimensional
    -> Series
    -> Series
    -> Maybe (Svg msg)
lineBrushOverlay b c xsc ysc brush hdata data =
    case brushing c of
        NoBrush ->
            Nothing

        BrushNoLabels { appearance } ->
            Just <|
                lineBrushOverlayHelp appearance ( Nothing, Nothing ) b c xsc ysc brush hdata data

        BrushLabels { appearance, labels } ->
            Just <|
                lineBrushOverlayHelp appearance ( Just labels, Just labels ) b c xsc ysc brush hdata data

        BrushLabelsX { appearance, labels } ->
            Just <|
                lineBrushOverlayHelp appearance ( Just labels, Nothing ) b c xsc ysc brush hdata data

        BrushLabelsY { appearance, labels } ->
            Just <|
                lineBrushOverlayHelp appearance ( Nothing, Just labels ) b c xsc ysc brush hdata data


lineBrushOverlayHelp :
    BrushingAppearanceConfig
    -> ( Maybe BrushingLabelsConfig, Maybe BrushingLabelsConfig )
    -> Bem.Block
    -> Config msg
    -> ContinuousScale Time.Posix
    -> ContinuousScale Float
    -> Brush OneDimensional
    -> Series
    -> Series
    -> Svg msg
lineBrushOverlayHelp bapp ( mxlabels, mylabels ) b c xsc ysc brush hdata data =
    let
        e =
            b.element "brush-overlay"

        ea =
            b.element "brush-overlay-area"

        tint =
            timeInterval c

        tz =
            timeZone c

        mselectext =
            extentBrushed brush xsc

        ( hlower, hupper ) =
            mselectext
                |> Maybe.map
                    (Tuple.mapBoth
                        (\x -> isHighlightedTimeInterval Time.ceiling tint tz x hdata)
                        (\x -> isHighlightedTimeInterval Time.floor tint tz x hdata)
                    )
                |> Maybe.withDefault ( False, False )

        selected =
            mselectext
                |> Maybe.map (\ext -> selectInTimeExtent ext data)
                |> Maybe.withDefault []

        brusharea =
            selected
                |> scaledAreaBounds -1 xsc ysc
                |> Shape.area Shape.linearCurve

        brushlabels =
            lineBrushOverlayBoundsAndLabels
                bapp
                ( mxlabels, mylabels )
                b
                tint
                tz
                c
                xsc
                ysc
                hlower
                hupper
                selected
    in
    S.g
        [ e |> element ]
        [ Path.element brusharea
            [ ea |> element
            , SA.fill bapp.area
            ]
        , brushlabels |> Maybe.withDefault (text "")
        ]


lineBrushOverlayBoundsAndLabels :
    BrushingAppearanceConfig
    -> ( Maybe BrushingLabelsConfig, Maybe BrushingLabelsConfig )
    -> Bem.Block
    -> Time.Interval
    -> Time.Zone
    -> Config msg
    -> ContinuousScale Time.Posix
    -> ContinuousScale Float
    -> Bool
    -> Bool
    -> Series
    -> Maybe (Svg msg)
lineBrushOverlayBoundsAndLabels app ( mxlabels, mylabels ) b tint tz c xsc ysc hlower hupper selected =
    let
        inner ( ( ( vmin, vminl ), ( hmin, hminl ) ), ( ( vmax, vmaxl ), ( hmax, hmaxl ) ) ) =
            case ( mxlabels, mylabels ) of
                ( Nothing, Nothing ) ->
                    S.g
                        []
                        [ brushBoundsLines app b "x" c hlower hupper ( vmin, vmax )
                        ]

                ( Just xlabels, Nothing ) ->
                    S.g
                        []
                        [ brushBoundsXLinesAndLabels
                            app
                            xlabels
                            b
                            c
                            hlower
                            hupper
                            ( ( vmin, vminl ), ( vmax, vmaxl ) )
                        ]

                ( Nothing, Just ylabels ) ->
                    S.g
                        []
                        [ brushBoundsYLinesAndLabels
                            app
                            ylabels
                            b
                            c
                            hlower
                            hupper
                            ( ( hmin, hminl ), ( hmax, hmaxl ) )
                        ]

                ( Just xlabels, Just ylabels ) ->
                    S.g
                        []
                        [ brushBoundsXLinesAndLabels
                            app
                            xlabels
                            b
                            c
                            hlower
                            hupper
                            ( ( vmin, vminl ), ( vmax, vmaxl ) )
                        , brushBoundsYLinesAndLabels
                            app
                            ylabels
                            b
                            c
                            hlower
                            hupper
                            ( ( hmin, hminl ), ( hmax, hmaxl ) )
                        ]
    in
    selectedBoundsAndLabels tint tz xsc ysc selected
        |> Maybe.map inner


selectedBoundsAndLabels :
    Time.Interval
    -> Time.Zone
    -> ContinuousScale Time.Posix
    -> ContinuousScale Float
    -> Series
    ->
        Maybe
            ( ( ( Path, ( String, Float ) ), ( Path, ( Float, Float ) ) )
            , ( ( Path, ( String, Float ) ), ( Path, ( Float, Float ) ) )
            )
selectedBoundsAndLabels tint tz xsc ysc data =
    let
        mext =
            data |> Statistics.extentBy (Tuple.first >> Time.posixToMillis)

        inner ( x, y ) =
            ( ( scaledVerticalLinePoints xsc ysc ( x, y ) |> Shape.line Shape.linearCurve
              , ( timeIntervalString tint tz x
                , Scale.convert xsc x
                )
              )
            , ( scaledHorizontalLinePoints xsc ysc ( x, y ) |> Shape.line Shape.linearCurve
              , ( y
                , Scale.convert ysc y
                )
              )
            )
    in
    mext |> Maybe.map (Tuple.mapBoth inner inner)


brushBoundsLines :
    BrushingAppearanceConfig
    -> Bem.Block
    -> String
    -> Config msg
    -> Bool
    -> Bool
    -> ( Path, Path )
    -> Svg msg
brushBoundsLines bapp b dim c hlower hupper ( p0, p1 ) =
    let
        e =
            b.element "selected-bounds"
    in
    S.g
        [ e |> elementOf "dim" dim ]
        (brushBoundsLinesInner bapp b dim c hlower hupper ( p0, p1 ))


brushBoundsLinesInner :
    BrushingAppearanceConfig
    -> Bem.Block
    -> String
    -> Config msg
    -> Bool
    -> Bool
    -> ( Path, Path )
    -> List (Svg msg)
brushBoundsLinesInner bapp b dim c hlower hupper ( p0, p1 ) =
    let
        ep =
            b.element "selected-bound"

        str =
            bapp.bounds

        hstr =
            bapp.highlight

        strdash =
            if bapp.boundsDashed then
                "4 2"

            else
                ""
    in
    [ Path.element p0
        [ ep |> elementOfList [ ( "dim", dim ), ( "type", "lower" ) ]
        , ep |> elementIf "highlight" hlower
        , SA.stroke <|
            if hlower then
                hstr

            else
                str
        , SA.strokeDasharray strdash
        , SA.fill PaintNone
        ]
    , Path.element p1
        [ ep |> elementOfList [ ( "dim", dim ), ( "type", "upper" ) ]
        , ep |> elementIf "highlight" hupper
        , SA.stroke <|
            if hupper then
                hstr

            else
                str
        , SA.strokeDasharray strdash
        , SA.fill PaintNone
        ]
    ]


brushBoundsXLinesAndLabels :
    BrushingAppearanceConfig
    -> BrushingLabelsConfig
    -> Bem.Block
    -> Config msg
    -> Bool
    -> Bool
    -> ( ( Path, ( String, Float ) ), ( Path, ( String, Float ) ) )
    -> Svg msg
brushBoundsXLinesAndLabels bapp bl b c hlower hupper ( ( p0, ( l0, x0 ) ), ( p1, ( l1, x1 ) ) ) =
    let
        e =
            b.element "selected-bounds"

        el =
            b.element "selected-bound-label"

        pad =
            padding c

        h =
            height c

        w =
            width c

        ybaseline =
            h - pad * 2

        xmid =
            (w - pad * 2) / 2.0

        x0adj =
            if x0 <= xmid then
                1

            else
                -1

        x1adj =
            if x1 <= xmid then
                1

            else
                -1

        regcolor =
            bapp.bounds

        hcolor =
            bapp.highlight

        fsize =
            bl.size
    in
    S.g
        [ e |> elementOf "dim" "x" ]
        (brushBoundsLinesInner bapp b "x" c hlower hupper ( p0, p1 )
            ++ [ S.text_
                    [ el |> elementOfList [ ( "dim", "x" ), ( "type", "lower" ) ]
                    , el |> elementIf "highlight" hlower
                    , SA.transform [ Translate (x0 + x0adj) ybaseline ]
                    , SA.textAnchor <|
                        if x0 <= xmid then
                            AnchorStart

                        else
                            AnchorEnd
                    , SA.fontSize fsize
                    , SA.fill <|
                        if hlower then
                            hcolor

                        else
                            regcolor
                    , SA.style "pointer-events: none; user-select: none"
                    ]
                    [ text l0 ]
               , S.text_
                    [ el |> elementOfList [ ( "dim", "x" ), ( "type", "upper" ) ]
                    , el |> elementIf "highlight" hupper
                    , SA.transform [ Translate (x1 + x1adj) ybaseline ]
                    , SA.textAnchor <|
                        if x1 <= xmid then
                            AnchorStart

                        else
                            AnchorEnd
                    , SA.fontSize fsize
                    , SA.fill <|
                        if hupper then
                            hcolor

                        else
                            regcolor
                    , SA.style "pointer-events: none; user-select: none"
                    ]
                    [ text l1 ]
               ]
        )


brushBoundsYLinesAndLabels :
    BrushingAppearanceConfig
    -> BrushingLabelsConfig
    -> Bem.Block
    -> Config msg
    -> Bool
    -> Bool
    -> ( ( Path, ( Float, Float ) ), ( Path, ( Float, Float ) ) )
    -> Svg msg
brushBoundsYLinesAndLabels bapp bl b c hlower hupper ( ( p0, ( v0, y0 ) ), ( p1, ( v1, y1 ) ) ) =
    let
        e =
            b.element "selected-bounds"

        el =
            b.element "selected-bound-label"

        pad =
            padding c

        h =
            height c

        w =
            width c

        xbaseline =
            0.0

        l0 =
            v0 |> bl.toString

        l1 =
            v1 |> bl.toString

        ymid =
            (h - pad * 2) / 2.0

        y0adj =
            if y0 <= ymid then
                1

            else
                -1

        y1adj =
            if y1 <= ymid then
                1

            else
                -1

        y0align =
            if y0 <= ymid then
                AlignmentHanging

            else
                AlignmentBaseline

        y1align =
            if y1 <= ymid then
                AlignmentHanging

            else
                AlignmentBaseline

        regcolor =
            bapp.bounds

        hcolor =
            bapp.highlight

        fsize =
            bl.size
    in
    S.g
        [ e |> elementOf "dim" "y" ]
        (brushBoundsLinesInner bapp b "y" c hlower hupper ( p0, p1 )
            ++ [ S.text_
                    [ el |> elementOfList [ ( "dim", "y" ), ( "type", "lower" ) ]
                    , el |> elementIf "highlight" hlower
                    , SA.transform [ Translate xbaseline (y0 + y0adj) ]
                    , SA.textAnchor AnchorStart
                    , SA.alignmentBaseline y0align
                    , SA.fill <|
                        if hlower then
                            hcolor

                        else
                            regcolor
                    , SA.fontSize fsize
                    , SA.style "pointer-events: none; user-select: none"
                    ]
                    [ text l0 ]
               , S.text_
                    [ el |> elementOfList [ ( "dim", "y" ), ( "type", "upper" ) ]
                    , el |> elementIf "highlight" hupper
                    , SA.transform [ Translate xbaseline (y1 + y1adj) ]
                    , SA.textAnchor AnchorStart
                    , SA.alignmentBaseline y1align
                    , SA.fill <|
                        if hupper then
                            hcolor

                        else
                            regcolor
                    , SA.fontSize fsize
                    , SA.style "pointer-events: none; user-select: none"
                    ]
                    (text l1
                        :: percentDiffLabel
                            [ SA.alignmentBaseline y1align
                            , SA.fontSize fsize
                            ]
                            b
                            v0
                            v1
                    )
               ]
        )


percentDiffLabel :
    List (Attribute msg)
    -> Bem.Block
    -> Float
    -> Float
    -> List (Svg msg)
percentDiffLabel attrs b y0 y1 =
    let
        es =
            b.element "percent-label-symbol"

        ev =
            b.element "percent-label-value"

        pct =
            (y1 - y0) / y0

        ( tag, sym ) =
            case compare pct 0.0 of
                EQ ->
                    ( "eq", " " )

                GT ->
                    ( "gt", "⯅" )

                LT ->
                    ( "lt", "⯆" )

        disp =
            (pct * 100 |> round |> abs |> String.fromInt) ++ "%"
    in
    [ S.tspan
        ((es |> elementOf "diff" tag) :: attrs)
        [ text sym ]
    , S.tspan
        ((ev |> elementOf "diff" tag) :: attrs)
        [ text disp ]
    ]



-- TODO: Finish implementing and move somewhere generic


timeIntervalString : Time.Interval -> Time.Zone -> Time.Posix -> String
timeIntervalString tint tz t =
    let
        tparts =
            Time.posixToParts tz t
    in
    case tint of
        Time.Year ->
            tparts.year |> String.fromInt

        _ ->
            "TODO"



-- -----------------------------------------------------------------------------
-- COLUMN VIEWS
-- -----------------------------------------------------------------------------


columnFacets : Facet.Scaling2d -> Config msg -> List Series -> List (Svg msg)
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


columns : Config msg -> Series -> Svg msg
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


columnsInner : Float -> BandScale Time.Posix -> ContinuousScale Float -> Config msg -> Series -> Svg msg
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

        cbar =
            c.appearance.color

        chigh =
            c.appearance.highlightColor

        highlights =
            data |> selectHighlights c.highlight
    in
    S.g
        [ SA.transform [ Translate pad pad ], e |> element ]
    <|
        List.map (columnInner ec cbar chigh h pad xsc ysc highlights) data


columnInner :
    Bem.Element
    -> Paint
    -> Paint
    -> Float
    -> Float
    -> BandScale Time.Posix
    -> ContinuousScale Float
    -> Series
    -> Observation
    -> Svg msg
columnInner e cbar chigh h pad xsc ysc highlights ( x, y ) =
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
                if ishigh then
                    chigh

                else
                    cbar
            ]
            []
        ]
