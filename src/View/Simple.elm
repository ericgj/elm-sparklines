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
    , selectBrushed
    , withBandConfig
    , withBrush
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
import TypedSvg.Core exposing (Svg, text)
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
    , onBrush : Maybe (OnBrush -> msg)
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
    , color : Color
    , highlightColor : Color
    , selectFill : Paint
    , selectStroke : Paint
    , selectStrokeDashed : Bool
    , selectLabelSize : Length
    , bandPaddingInner : Float
    , bandPaddingOuter : Float
    , bandAlign : Float
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


onBrush : Config msg -> Maybe (OnBrush -> msg)
onBrush (Config c) =
    c.onBrush


color : Config msg -> Color
color (Config c) =
    c.appearance.color


highlightColor : Config msg -> Color
highlightColor (Config c) =
    c.appearance.highlightColor


selectFill : Config msg -> Paint
selectFill (Config c) =
    c.appearance.selectFill


selectStroke : Config msg -> Paint
selectStroke (Config c) =
    c.appearance.selectStroke


selectStrokeDashArray : Config msg -> String
selectStrokeDashArray (Config c) =
    if c.appearance.selectStrokeDashed then
        "4 2"

    else
        ""


selectLabelSize : Config msg -> Length
selectLabelSize (Config c) =
    c.appearance.selectLabelSize


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
        , onBrush = Nothing
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
        , onBrush = Nothing
        , appearance = defaultColumnsAppearanceConfig
        , css = defaultCssConfig
        }


defaultLineAppearanceConfig : AppearanceConfig
defaultLineAppearanceConfig =
    { width = 1.0
    , color = Color.rgb 0 0 0
    , highlightColor = Color.rgb 1 0 0
    , selectFill = Paint <| Color.rgb 1 1 0
    , selectStroke = Paint <| Color.rgba 0 0 0 0.5
    , selectStrokeDashed = False
    , selectLabelSize = Rem 0.7
    , bandPaddingInner = 0.0
    , bandPaddingOuter = 0.0
    , bandAlign = 0.5
    }


defaultColumnsAppearanceConfig : AppearanceConfig
defaultColumnsAppearanceConfig =
    { width = 1.0
    , color = Color.rgb 0 0 0
    , highlightColor = Color.rgb 1 0 0
    , selectFill = Paint <| Color.rgb 1 1 0
    , selectStroke = PaintNone
    , selectStrokeDashed = False
    , selectLabelSize = Rem 0.7
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


withPadding : Float -> Config msg -> Config msg
withPadding p (Config c) =
    Config { c | padding = p }


withHighlight : Highlight -> Config msg -> Config msg
withHighlight h (Config c) =
    Config { c | highlight = h }


withBrush : (OnBrush -> msg) -> Config msg -> Config msg
withBrush tomsg (Config c) =
    Config { c | onBrush = Just tomsg }


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


isHighlightedTime : Time.Posix -> Series -> Bool
isHighlightedTime x =
    List.any (\( x_, _ ) -> Time.posixToMillis x_ == Time.posixToMillis x)



-- -----------------------------------------------------------------------------
-- BRUSHING
-- -----------------------------------------------------------------------------


extentMaybeBrushed :
    Maybe (Brush OneDimensional)
    -> Float
    -> ContinuousScale Time.Posix
    -> Maybe ( Time.Posix, Time.Posix )
extentMaybeBrushed mbrush pad xsc =
    mbrush |> Maybe.andThen (\br -> extentBrushed br pad xsc)


selectMaybeBrushed :
    Maybe (Brush OneDimensional)
    -> Float
    -> ContinuousScale Time.Posix
    -> Series
    -> Maybe Series
selectMaybeBrushed mbrush pad xsc data =
    mbrush |> Maybe.andThen (\br -> selectBrushed br pad xsc data)


selectBrushed :
    Brush OneDimensional
    -> Float
    -> ContinuousScale Time.Posix
    -> Series
    -> Maybe Series
selectBrushed br pad xsc data =
    extentBrushed br pad xsc
        |> Maybe.map (\ext -> selectInTimeExtent ext data)


extentBrushed :
    Brush OneDimensional
    -> Float
    -> ContinuousScale Time.Posix
    -> Maybe ( Time.Posix, Time.Posix )
extentBrushed br pad xsc =
    let
        invert x =
            (x - pad) |> Scale.invert xsc
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


viewBrush : Bem.Block -> Maybe (OnBrush -> msg) -> Maybe (Brush OneDimensional) -> List (Svg msg)
viewBrush b mmsg mbrush =
    let
        e =
            b.element "brush"

        brushrect _ attrs =
            S.rect
                ((SA.fillOpacity <| Opacity <| 0.0)
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
-- LINE VIEWS
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

        brush =
            viewBrush b (onBrush c) mbrush

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
    List ( Color, Color )
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
            viewBrush b (onBrush c) mbrush
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
            viewBrush b (onBrush c) mbrush
    in
    S.svg
        [ SA.viewBox 0 0 w h ]
        ([ inner ] ++ brush)


lineInner :
    Maybe ( Color, Color )
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

        ea =
            b.element "selected-area"

        tint =
            timeInterval c

        tz =
            timeZone c

        pad =
            padding c

        ( linecolor, highlightcolor ) =
            mcpair
                |> Maybe.withDefault ( color c, highlightColor c )

        sfill =
            selectFill c

        linewidth =
            observationWidth c

        highlights =
            data |> selectHighlights (highlight c)

        mselectext =
            extentMaybeBrushed mbrush pad xsc

        selected =
            mselectext
                |> Maybe.map (\ext -> selectInTimeExtent ext data)
                |> Maybe.withDefault []

        ( hlower, hupper ) =
            mselectext
                |> Maybe.map
                    (Tuple.mapBoth
                        (\x -> isHighlightedTime x highlights)
                        (\x -> isHighlightedTime x highlights)
                    )
                |> Maybe.withDefault ( False, False )

        linepath =
            data
                |> scaledPoints xsc ysc
                |> Shape.line Shape.linearCurve

        areapath =
            selected
                |> scaledAreaBounds -1 xsc ysc
                |> Shape.area Shape.linearCurve

        mbounds =
            selected
                |> selectedBoundsLinesAndLabels tint tz xsc ysc
                |> Maybe.map (viewBoundsLinesAndLabels b c hlower hupper)

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
        [ e |> elementIf "selected" (List.length selected > 0)
        , SA.transform [ Translate pad pad ]
        ]
        (Path.element linepath
            [ el |> element
            , SA.stroke <| Paint <| linecolor
            , SA.strokeWidth <| Px linewidth
            , SA.fill PaintNone
            ]
            :: Path.element areapath
                [ ea |> element
                , SA.fill <| sfill
                ]
            :: (mbounds |> Maybe.withDefault (text ""))
            :: circles
        )


viewBoundsLinesAndLabels :
    Bem.Block
    -> Config msg
    -> Bool
    -> Bool
    ->
        ( ( ( Path, ( String, Float ) ), ( Path, ( String, Float ) ) )
        , ( ( Path, ( String, Float ) ), ( Path, ( String, Float ) ) )
        )
    -> Svg msg
viewBoundsLinesAndLabels b c islowerhl isupperhl ( ( vlower, hlower ), ( vupper, hupper ) ) =
    S.g
        []
        [ viewXBoundsLinesAndLabels
            b
            c
            islowerhl
            isupperhl
            ( vlower, vupper )
        , viewYBoundsLinesAndLabels
            b
            c
            islowerhl
            isupperhl
            ( hlower, hupper )
        ]


viewXBoundsLinesAndLabels :
    Bem.Block
    -> Config msg
    -> Bool
    -> Bool
    -> ( ( Path, ( String, Float ) ), ( Path, ( String, Float ) ) )
    -> Svg msg
viewXBoundsLinesAndLabels b c islowerhl isupperhl ( ( p0, ( l0, x0 ) ), ( p1, ( l1, x1 ) ) ) =
    let
        e =
            b.element "selected-bounds"

        ep =
            b.element "selected-bound"

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

        str =
            selectStroke c

        strdash =
            selectStrokeDashArray c

        fsize =
            selectLabelSize c
    in
    S.g
        [ e |> elementOf "dim" "x" ]
        [ Path.element p0
            [ ep |> elementOfList [ ( "dim", "x" ), ( "type", "lower" ) ]
            , ep |> elementIf "highlight" islowerhl
            , SA.stroke str
            , SA.strokeDasharray strdash
            , SA.fill PaintNone
            ]
        , Path.element p1
            [ ep |> elementOfList [ ( "dim", "x" ), ( "type", "upper" ) ]
            , ep |> elementIf "highlight" isupperhl
            , SA.stroke str
            , SA.strokeDasharray strdash
            , SA.fill PaintNone
            ]
        , S.text_
            [ el |> elementOfList [ ( "dim", "x" ), ( "type", "lower" ) ]
            , el |> elementIf "highlight" islowerhl
            , SA.transform [ Translate x0 ybaseline ]
            , SA.textAnchor <|
                if x0 <= xmid then
                    AnchorStart

                else
                    AnchorEnd
            , SA.fontSize fsize
            , SA.style "pointer-events: none; user-select: none"
            ]
            [ text l0 ]
        , S.text_
            [ el |> elementOfList [ ( "dim", "x" ), ( "type", "upper" ) ]
            , el |> elementIf "highlight" isupperhl
            , SA.transform [ Translate x1 ybaseline ]
            , SA.textAnchor <|
                if x1 <= xmid then
                    AnchorStart

                else
                    AnchorEnd
            , SA.fontSize fsize
            , SA.style "pointer-events: none; user-select: none"
            ]
            [ text l1 ]
        ]


viewYBoundsLinesAndLabels :
    Bem.Block
    -> Config msg
    -> Bool
    -> Bool
    -> ( ( Path, ( String, Float ) ), ( Path, ( String, Float ) ) )
    -> Svg msg
viewYBoundsLinesAndLabels b c islowerhl isupperhl ( ( p0, ( l0, y0 ) ), ( p1, ( l1, y1 ) ) ) =
    let
        e =
            b.element "selected-bounds"

        ep =
            b.element "selected-bound"

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

        ymid =
            (h - pad * 2) / 2.0

        str =
            selectStroke c

        strdash =
            selectStrokeDashArray c

        fsize =
            selectLabelSize c
    in
    S.g
        [ e |> elementOf "type" "y" ]
        [ Path.element p0
            [ ep |> elementOfList [ ( "dim", "y" ), ( "type", "lower" ) ]
            , ep |> elementIf "highlight" islowerhl
            , SA.stroke str
            , SA.strokeDasharray strdash
            , SA.fill PaintNone
            ]
        , Path.element p1
            [ ep |> elementOfList [ ( "dim", "y" ), ( "type", "upper" ) ]
            , ep |> elementIf "highlight" isupperhl
            , SA.stroke str
            , SA.strokeDasharray strdash
            , SA.fill PaintNone
            ]
        , S.text_
            [ el |> elementOfList [ ( "dim", "y" ), ( "type", "lower" ) ]
            , el |> elementIf "highlight" islowerhl
            , SA.transform [ Translate 0.0 y0 ]
            , SA.textAnchor AnchorStart
            , SA.alignmentBaseline <|
                if y0 <= ymid then
                    AlignmentHanging

                else
                    AlignmentBaseline
            , SA.fontSize fsize
            , SA.style "pointer-events: none; user-select: none"
            ]
            [ text l0 ]
        , S.text_
            [ el |> elementOfList [ ( "dim", "y" ), ( "type", "upper" ) ]
            , el |> elementIf "highlight" isupperhl
            , SA.transform [ Translate 0.0 y1 ]
            , SA.textAnchor AnchorStart
            , SA.alignmentBaseline <|
                if y1 <= ymid then
                    AlignmentHanging

                else
                    AlignmentBaseline
            , SA.fontSize fsize
            , SA.style "pointer-events: none; user-select: none"
            ]
            [ text l1 ]
        ]


selectedBoundsLinesAndLabels :
    Time.Interval
    -> Time.Zone
    -> ContinuousScale Time.Posix
    -> ContinuousScale Float
    -> Series
    ->
        Maybe
            ( ( ( Path, ( String, Float ) ), ( Path, ( String, Float ) ) )
            , ( ( Path, ( String, Float ) ), ( Path, ( String, Float ) ) )
            )
selectedBoundsLinesAndLabels tint tz xsc ysc data =
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
              , ( y |> round |> String.fromInt
                , Scale.convert ysc y
                )
              )
            )
    in
    mext |> Maybe.map (Tuple.mapBoth inner inner)


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
            ""



-- not implemented yet


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
    -> Color
    -> Color
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
                Paint <|
                    if ishigh then
                        chigh

                    else
                        cbar
            ]
            []
        ]
