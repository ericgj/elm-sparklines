module Sparklines exposing
    ( line, lines, columns
    , lineFacets, columnsFacets
    , ChartData, lineData, lineFacetsData, columnsData, columnsFacetsData
    , Highlight(..)
    , lineConfig, columnsConfig, withPadding, withCssBlock, withHighlight, withBandConfig, defaultColorPairs, Config
    , withBrush, withBrushLabels, withBrushLabelsX, withBrushLabelsY, BrushingAppearanceConfig, BrushingLabelsConfig, defaultBrushingAppearance, formattingLabelsX, formattingLabelsY
    , coloringLabels, sizingLabels
    )

{-| Sparkline view functions. There are two basic chart types:

  - [Line charts](#line)
  - [Columns charts](#columns)

These are conceptually very similar, except columns charts aggregate
observations under a specified time interval, whereas line charts do not do any
aggregation, but plot the observations according to a continuous time scale,
and draw a line between them.

The data to be charted must be a `List (Time.Posix, Float)`. For line charts,
in most cases the X values should be unique, i.e. there should one Y
for every X, but this is not enforced. For columns charts, this constraint is
enforced by aggregating Y values over each given time interval, using a
provided aggregation function (by default, `List.sum`). (If you want the same
behavior for line charts, i.e. aggregating over time intervals, use the
functions in the [Timeseries](Timeseries) module on your data first.)

There are also functions for scaling multiple data series together, i.e.
for creating 'facet' charts from small multiples:

  - [Faceted line charts](#lineFacets)
  - [Faceted columns charts](#columnsFacets)

Please note that these functions do not themselves group your data into small
multiples, nor do they position or label the series each chart represents.
That is all up to you. The purpose of these chart functions is to render the
charts scaling the X and Y dimensions of the series according to the parameters
you specify.

Finally, a function to create a single line chart with multiple series (with
both X and Y dimensions scaled together) is provided:

  - [Multiple-lines charts](#lines)


# Highlighting

For any chart type you can define which observations are _highlighted_.
Often this is the last observation, or the min and/or the max, but there are
some more options as well (see [Highlight](#Highlight)). Highlighted
observations are rendered as circles (points) with a configurable color.


# Brushing

For any chart type except multiple-lines charts, you can provide and configure a
an elm-visualization
[Brush](https://package.elm-lang.org/packages/elm-visualization/latest/Brush)
to allow interactive selection of observations. This can also be
configured to display X and Y values at the start and end of the selection --
interactive axis labels.


# Access to highlighted and selected data

In addition, functions are available that return charts together with
the data that is currently highlighted and selected (brushed), to make it
easy to build a separate view based on the current highlights/selections.


# Examples

See the examples directory for all features in action.


# Reference


## Single charts

@docs line, lines, columns


## Facet charts

@docs lineFacets, columnsFacets


## Charts together with selected/highlighted data

@docs ChartData, lineData, lineFacetsData, columnsData, columnsFacetsData


## Highlighting

@docs Highlight


## Basic configuration

@docs lineConfig, columnsConfig, withPadding, withCssBlock, withHighlight, withBandConfig, defaultColorPairs, Config


## Brushing configuration

@docs withBrush, withBrushLabels, withBrushLabelsX, withBrushLabelsY, BrushingAppearanceConfig, BrushingLabelsConfig, defaultBrushingAppearance, formattingLabelsX, formattingLabelsY

-}

import Brush exposing (Brush, OnBrush, OneDimensional)
import Color exposing (Color)
import DateFormat as DF
import Facet exposing (Scale(..), Scaling(..))
import List.Extra as List
import Path exposing (Path)
import Scale exposing (BandConfig, BandScale, ContinuousScale)
import Scale.Band.Extra
import Scale.Color
import Shape
import Statistics
import Svg.Bem as Bem exposing (element, elementIf, elementName, elementNameMod, elementOf, elementOfList)
import Time
import Time.Extra as Time
import Timeseries exposing (Observation, Series)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core exposing (Attribute, Svg, attribute, text)
import TypedSvg.Filters as SF
import TypedSvg.Filters.Attributes as SFA
import TypedSvg.Types as ST
    exposing
        ( AlignmentBaseline(..)
        , AnchorAlignment(..)
        , Length(..)
        , Opacity(..)
        , Paint(..)
        , ShapeRendering(..)
        )



--------------------------------------------------------------------------------
-- CONFIG
--------------------------------------------------------------------------------


{-| Chart configuration. See above for usage examples.
-}
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


{-| Options for chart highlighting.

  - `NoHighlight`: Don't highlight anything
  - `HighlightLast`: Highlight the last observation
  - `HighlightNegative`: Highlight all values less than 0
  - `HighlightMin`: Highlight the minimum value in the series
  - `HighlightMax`: Highlight the maximum value in the series
  - `HighlightMinMax`: Highlight the minimum and maximum values in the series
  - `HighlightPeaks`: Highlight positive peaks detected in the series, with
    specified parameters.

See [elm-visualization's peaks function](https://package.elm-lang.org/packages/gampleman/elm-visualization/latest/Statistics#peaks) for details on that calculation and the parameters needed.

Example:

    config : Config msg
    config =
        lineConfig Time.Day Time.utc 100 50
            |> withHighlight HighlightMax

-}
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


{-| Configuration for the appearance of the brush overlay. Note that
colors are specified using `TypedSvg.Type.Paint`.

  - `area`: the fill color of the brushed area (or bars) of the chart
  - `bounds`: the stroke color of the bounds lines of the brush
  - `boundsDashed`: True if you want dashed bounds lines
  - `highlight`: the color of bounds lines and labels when the observations are
    highlighted

-}
type alias BrushingAppearanceConfig =
    { area : Paint
    , bounds : Paint
    , boundsDashed : Bool
    , highlight : Paint
    }


{-| Configuration for the appearance of the brush overlay labels. Note that
color is specified using `TypedSvg.Type.Paint`, and size is specified using
`TypedSvg.Type.Length`.

  - `color`: the text color of labels
  - `size`: the size of label text
  - `labelX`: how to display X (time) values
  - `labelY`: how to display Y (float) values

Note `labelX` uses the time zone specified in top-level Config.

-}
type alias BrushingLabelsConfig =
    { color : Paint
    , size : Length
    , labelX : Time.Zone -> Time.Posix -> String
    , labelY : Float -> String
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


{-| Basic line chart configuration, given a Time.Zone, width, and height of chart.
-}
lineConfig : Time.Zone -> Float -> Float -> Config msg
lineConfig tz w h =
    Config
        { width = w
        , height = h
        , padding = 0.0
        , timeZone = tz
        , interval = Time.Day
        , aggregate = List.sum
        , highlight = NoHighlight
        , brushing = NoBrush
        , appearance = defaultLineAppearanceConfig
        , css = defaultCssConfig
        }


{-| Basic columns chart configuration, given an aggregation function,
a Time.Interval, a Time.Zone, the width, and the height of chart.

Example for a columns chart aggregating by the mean of observations over
month intervals:

    config : Config msg
    config =
        columnsConfig
            (\ys -> List.sum ys / (List.length ys |> toFloat))
            Time.Month
            Time.utc
            100
            50

-}
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


{-| Default brushing appearance options.
Yellow selection area, 70% black solid (non-dashed) bounds lines, red when
highlighted.
-}
defaultBrushingAppearance : BrushingAppearanceConfig
defaultBrushingAppearance =
    { area = Paint <| Color.rgb 1 1 0
    , bounds = Paint <| Color.rgba 0 0 0 0.7
    , boundsDashed = False
    , highlight = Paint <| Color.rgb 1 0 0
    }


{-| Default brushing labels options.
White text (note labels appear over a solid-color filter), 80% rem,
values are displayed as rounded numbers.
-}
defaultBrushingLabels : Time.Interval -> BrushingLabelsConfig
defaultBrushingLabels tint =
    { color = Paint <| Color.rgb 1 1 1
    , size = Rem 0.8
    , labelX = defaultTimeFormat tint
    , labelY = round >> String.fromInt
    }


defaultTimeFormat : Time.Interval -> Time.Zone -> Time.Posix -> String
defaultTimeFormat tint =
    case tint of
        Time.Year ->
            DF.format [ DF.yearNumber ]

        Time.Quarter ->
            DF.format [ DF.text "Q", DF.quarterNumber, DF.text " ", DF.yearNumber ]

        Time.Month ->
            DF.format [ DF.monthNameAbbreviated, DF.text " ", DF.yearNumber ]

        Time.Week ->
            DF.format [ DF.text "W", DF.weekOfYearNumber, DF.text " ", DF.yearNumber ]

        Time.Day ->
            DF.format
                [ DF.dayOfMonthFixed
                , DF.text "-"
                , DF.monthNameAbbreviated
                , DF.text "-"
                , DF.yearNumber
                ]

        Time.Hour ->
            DF.format
                [ DF.hourNumber, DF.amPmUppercase ]

        Time.Minute ->
            DF.format
                [ DF.hourNumber, DF.text ":", DF.minuteFixed, DF.amPmUppercase ]

        Time.Second ->
            DF.format
                [ DF.hourNumber
                , DF.text ":"
                , DF.minuteFixed
                , DF.text ":"
                , DF.secondFixed
                , DF.amPmUppercase
                ]

        Time.Millisecond ->
            DF.format
                [ DF.hourNumber
                , DF.text ":"
                , DF.minuteFixed
                , DF.text ":"
                , DF.secondFixed
                , DF.text "."
                , DF.millisecondFixed
                , DF.amPmUppercase
                ]

        _ ->
            DF.format
                [ DF.dayOfMonthFixed
                , DF.text "-"
                , DF.monthNameAbbreviated
                , DF.text "-"
                , DF.yearNumber
                ]


{-| A default set of color-pairs for [multi-lines charts](#lines). Uses
elm-visualization `category10` color scale, with line color at 70% alpha of
the highlight color. Need a better default.
-}
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


{-| Set padding (in pixels) around chart
-}
withPadding : Float -> Config msg -> Config msg
withPadding p (Config c) =
    Config { c | padding = p }


{-| Set CSS block name (default is "sparklines")
-}
withCssBlock : String -> Config msg -> Config msg
withCssBlock s (Config c) =
    let
        upd css =
            { css | block = s }
    in
    Config { c | css = upd c.css }


{-| Set [highlight](#Highlight) to include in chart
-}
withHighlight : Highlight -> Config msg -> Config msg
withHighlight h (Config c) =
    Config { c | highlight = h }


{-| Configure brushing in chart (with no labels)

Example:

    type Msg
        = UpdateBrush Brush.OnBrush
        | ...

    config : Config Msg
    config =
        lineConfig Time.utc 100 50
            |> withBrush UpdateBrush defaultBrushingAppearance

-}
withBrush : (OnBrush -> msg) -> BrushingAppearanceConfig -> Config msg -> Config msg
withBrush onbrush bac (Config c) =
    let
        br =
            BrushNoLabels { onBrush = onbrush, appearance = bac }
    in
    Config { c | brushing = br }


{-| Configure brushing in chart with both X and Y labels

Example:

    type Msg
        = UpdateBrush Brush.OnBrush
        | ...

    config : Config Msg
    config =
        lineConfig Time.utc 100 50
            |> withBrushLabels UpdateBrush defaultBrushingAppearance

-}
withBrushLabels :
    (OnBrush -> msg)
    -> BrushingAppearanceConfig
    -> Config msg
    -> Config msg
withBrushLabels =
    withBrushLabelsHelp BrushLabels


{-| Configure brushing in chart with only X labels
-}
withBrushLabelsX :
    (OnBrush -> msg)
    -> BrushingAppearanceConfig
    -> Config msg
    -> Config msg
withBrushLabelsX =
    withBrushLabelsHelp BrushLabelsX


{-| Configure brushing in chart with only Y labels
-}
withBrushLabelsY :
    (OnBrush -> msg)
    -> BrushingAppearanceConfig
    -> Config msg
    -> Config msg
withBrushLabelsY =
    withBrushLabelsHelp BrushLabelsY


withBrushLabelsHelp :
    (BrushingLabelsData msg -> Brushing msg)
    -> (OnBrush -> msg)
    -> BrushingAppearanceConfig
    -> Config msg
    -> Config msg
withBrushLabelsHelp constr onbrush bac (Config c) =
    let
        br =
            constr
                { onBrush = onbrush
                , appearance = bac
                , labels = defaultBrushingLabels c.interval
                }
    in
    Config { c | brushing = br }


{-| Format time interval labels (x axis) with custom function.

Note: The time zone in top-level Config is used to render labels.

Most date/time formatting libraries should compose with this. Internally,
[ryannhg/date-format](https://package.elm-lang.org/packages/ryannhg/date-format/latest)
is used for default formats based on the given interval.

Example:


    yyyymmdd : Time.Zone -> Time.Posix -> String
    yyyymmdd =
        DateFormat.format
            [ DateFormat.yearNumber
            , DateFormat.monthFixed
            , DateFormat.dayOfMonthFixed
            ]

    config : Config Msg
    config =
        lineConfig Time.utc 100 50
            |> withBrushLabels UpdateBrush defaultBrushingAppearance
            |> formattingLabelsX yyyymmdd

-}
formattingLabelsX : (Time.Zone -> Time.Posix -> String) -> Config msg -> Config msg
formattingLabelsX fn (Config c) =
    let
        updBrushLabels bl =
            { bl | labels = updBrushLabelsC bl.labels }

        updBrushLabelsC blc =
            { blc | labelX = fn }
    in
    case c.brushing of
        BrushLabels bl ->
            Config { c | brushing = BrushLabels <| updBrushLabels bl }

        BrushLabelsX bl ->
            Config { c | brushing = BrushLabelsX <| updBrushLabels bl }

        BrushLabelsY bl ->
            Config { c | brushing = BrushLabelsY <| updBrushLabels bl }

        _ ->
            Config c


{-| Format labels for the y axis with custom function
-}
formattingLabelsY : (Float -> String) -> Config msg -> Config msg
formattingLabelsY fn (Config c) =
    let
        updBrushLabels bl =
            { bl | labels = updBrushLabelsC bl.labels }

        updBrushLabelsC blc =
            { blc | labelY = fn }
    in
    case c.brushing of
        BrushLabels bl ->
            Config { c | brushing = BrushLabels <| updBrushLabels bl }

        BrushLabelsX bl ->
            Config { c | brushing = BrushLabelsX <| updBrushLabels bl }

        BrushLabelsY bl ->
            Config { c | brushing = BrushLabelsY <| updBrushLabels bl }

        _ ->
            Config c


{-| Set label size (in TypedSvg.Types.Length).
-}
sizingLabels : ST.Length -> Config msg -> Config msg
sizingLabels sz (Config c) =
    let
        updBrushLabels bl =
            { bl | labels = updBrushLabelsC bl.labels }

        updBrushLabelsC blc =
            { blc | size = sz }
    in
    case c.brushing of
        BrushLabels bl ->
            Config { c | brushing = BrushLabels <| updBrushLabels bl }

        BrushLabelsX bl ->
            Config { c | brushing = BrushLabelsX <| updBrushLabels bl }

        BrushLabelsY bl ->
            Config { c | brushing = BrushLabelsY <| updBrushLabels bl }

        _ ->
            Config c


{-| Set label (background) color (in TypedSvg.Types.Paint).
-}
coloringLabels : ST.Paint -> Config msg -> Config msg
coloringLabels p (Config c) =
    let
        updBrushLabels bl =
            { bl | labels = updBrushLabelsC bl.labels }

        updBrushLabelsC blc =
            { blc | color = p }
    in
    case c.brushing of
        BrushLabels bl ->
            Config { c | brushing = BrushLabels <| updBrushLabels bl }

        BrushLabelsX bl ->
            Config { c | brushing = BrushLabelsX <| updBrushLabels bl }

        BrushLabelsY bl ->
            Config { c | brushing = BrushLabelsY <| updBrushLabels bl }

        _ ->
            Config c


{-| Configure the band-scale used for columns charts. Refer to
elm-visualization [Scale.BandConfig](https://package.elm-lang.org/packages/elm-visualization/gampleman.elm-visualization/latest/Scale#BandConfig) for details.
-}
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



-- -----------------------------------------------------------------------------
-- OUTPUT DATA
-- -----------------------------------------------------------------------------


{-| Data type for chart and selected and highlighted data returned by
[lineData](#lineData), [lineFacetsData](#lineFacetsData),
[columnsData](#columnsData), and [columnsFacetsData](#columnsFacetsData).
-}
type alias ChartData msg =
    { chart : Svg msg
    , selected : Series
    , highlighted : Series
    }



-- -----------------------------------------------------------------------------
-- LINE CHARTS
-- -----------------------------------------------------------------------------


{-| Produce a list of line charts, scaling the given timeseries data together
as specified by a [Facet.Scaling2d](Facet#Scaling2d), and passing in an
optional brush for synchronized brushing across all charts. (See
Example/Brush for an example of this).

Example (fixed X and free Y scales):

    charts : List (Svg msg)
    charts =
        lineFacets
            { x = Facet.Fixed, y = Facet.Free }
            config
            (Just brush)
            [ series1, series2, series3, ... ]

-}
lineFacets :
    Facet.Scaling2d
    -> Config msg
    -> Maybe (Brush OneDimensional)
    -> List Series
    -> List (Svg msg)
lineFacets s c mbrush seqs =
    lineFacetsData s c mbrush seqs |> List.map .chart


{-| Same as [lineFacets](#lineFacets), but returning [ChartData](#ChartData)
with highlighted and selected (brushed) data, in addition to the chart.
-}
lineFacetsData :
    Facet.Scaling2d
    -> Config msg
    -> Maybe (Brush OneDimensional)
    -> List Series
    -> List (ChartData msg)
lineFacetsData s c mbrush seqs =
    let
        b =
            cssBlock c

        w =
            width c

        h =
            height c

        sc =
            lineScales s c seqs

        inner xsc_ ysc_ seq_ =
            let
                d =
                    lineInnerData Nothing xsc_ ysc_ c mbrush seq_
            in
            { chart = svg w h [ d.chart ]
            , selected = d.selected
            , highlighted = d.highlighted
            }
    in
    case ( sc.x, sc.y ) of
        ( FixedScale xsc, FixedScale ysc ) ->
            seqs |> List.map (inner xsc ysc)

        ( FixedScale xsc, FreeScales yscs ) ->
            List.map2 (\ysc seq -> inner xsc ysc seq) yscs seqs

        ( FreeScales xscs, FixedScale ysc ) ->
            List.map2 (\xsc seq -> inner xsc ysc seq) xscs seqs

        ( FreeScales xscs, FreeScales yscs ) ->
            List.map3 inner xscs yscs seqs


{-| Produce a multi-line chart, with all given series scaled together on both
X and Y dimensions, using the given list of color pairs for the line and
highlight colors respectively of each series. (Note that colors are specified
as `TypedSvg.Types.Paint`.)

Example:

    chart : Svg msg
    chart =
        lines defaultColorPairs config [series1, series2, series3, ... ]

Note that neither brushing nor returning highlighted data is implemented
currently for multi-line charts.

-}
lines :
    List ( Paint, Paint )
    -> Config msg
    -> List Series
    -> Svg msg
lines cpairs c seqs =
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
                    lineInnerData mcpair xsc ysc c Nothing seq |> .chart
                )
                mcpairs
                seqs
    in
    svg w h inners


{-| Produce a line chart, passing in an optional brush for interactive
selection. (See Example/Brush for a full example of how to set up brushes).

Example:

    chart : Svg msg
    chart =
        line config (Just brush) series

-}
line : Config msg -> Maybe (Brush OneDimensional) -> Series -> Svg msg
line c mbrush seq =
    lineData c mbrush seq |> .chart


{-| Same as [line](#line), but returning [ChartData](#ChartData)
with highlighted and selected (brushed) data, in addition to the chart.
-}
lineData : Config msg -> Maybe (Brush OneDimensional) -> Series -> ChartData msg
lineData c mbrush seq =
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
            lineInnerData Nothing xsc ysc c mbrush seq
    in
    { chart = svg w h [ inner.chart ]
    , selected = inner.selected
    , highlighted = inner.highlighted
    }


lineInnerData :
    Maybe ( Paint, Paint )
    -> ContinuousScale Time.Posix
    -> ContinuousScale Float
    -> Config msg
    -> Maybe (Brush OneDimensional)
    -> Series
    -> ChartData msg
lineInnerData mcpair xsc ysc c mbrush data =
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

        highlighted =
            data |> selectHighlights (highlight c)

        linepath =
            data
                |> scaledPoints ( 0, 0 ) (LineScale xsc) ysc
                |> Shape.line Shape.linearCurve

        brush =
            viewBrush b (onBrush c) mbrush

        brushoverlay =
            mbrush
                |> Maybe.andThen
                    (\br -> lineBrushOverlay b c xsc ysc br highlighted data)

        circles =
            highlightCircles
                b
                highlightcolor
                (linewidth * 2.0)
                ( 0, 0 )
                (LineScale xsc)
                ysc
                highlighted

        chart =
            S.g
                [ e |> element
                , SA.transform [ ST.Translate pad pad ]
                ]
                (Path.element linepath
                    [ el |> element
                    , SA.stroke <| linecolor
                    , SA.strokeWidth <| Num linewidth
                    , SA.fill PaintNone
                    ]
                    :: (brushoverlay |> Maybe.map .chart |> Maybe.withDefault (text ""))
                    :: (brush |> Maybe.withDefault (text ""))
                    :: circles
                )
    in
    { chart = chart
    , selected = brushoverlay |> Maybe.map .selected |> Maybe.withDefault []
    , highlighted = highlighted
    }


highlightCircles :
    Bem.Block
    -> Paint
    -> Float
    -> ( Float, Float )
    -> ChartScaleX
    -> ContinuousScale Float
    -> Series
    -> List (Svg msg)
highlightCircles b fillcolor radius ( xadj, yadj ) xsc ysc data =
    let
        points =
            data |> scaledPoints ( xadj, yadj ) xsc ysc
    in
    points
        |> List.filterMap (Maybe.map (highlightCircle b fillcolor radius))


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
            [ SA.cx <| Num <| x
            , SA.cy <| Num <| y
            , SA.r <| Num radius
            , SA.fill <| fillcolor
            , SA.strokeWidth <| Num 0
            , SA.stroke <| PaintNone
            ]
            []
        ]



-- -----------------------------------------------------------------------------
-- COLUMN CHARTS
-- -----------------------------------------------------------------------------


{-| Produce a list of columns charts, scaling the given timeseries data together
as specified by a [Facet.Scaling2d](Facet#Scaling2d), and passing in an
optional brush for synchronized brushing across all charts. (See
Example/Brush for an example of this).

Example (fixed X and free Y scales):

    charts : List (Svg msg)
    charts =
        columnsFacets
            { x = Facet.Fixed, y = Facet.Free }
            config
            (Just brush)
            [ series1, series2, series3, ... ]

-}
columnsFacets :
    Facet.Scaling2d
    -> Config msg
    -> Maybe (Brush OneDimensional)
    -> List Series
    -> List (Svg msg)
columnsFacets s c mbrush seqs =
    columnsFacetsData s c mbrush seqs |> List.map .chart


{-| Same as [columnsFacets](#columnsFacets), but returning [ChartData](#ChartData)
with highlighted and selected (brushed) data, in addition to the chart.
-}
columnsFacetsData :
    Facet.Scaling2d
    -> Config msg
    -> Maybe (Brush OneDimensional)
    -> List Series
    -> List (ChartData msg)
columnsFacetsData s c mbrush seqs =
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

        inner xsc_ ysc_ seq_ =
            let
                d =
                    columnsInnerData h xsc_ ysc_ c mbrush seq_
            in
            { chart = svg w h [ d.chart ]
            , selected = d.selected
            , highlighted = d.highlighted
            }
    in
    case ( sc.x, sc.y ) of
        ( FixedScale xsc, FixedScale ysc ) ->
            List.map (inner xsc ysc) seqs_

        ( FixedScale xsc, FreeScales yscs ) ->
            List.map2 (inner xsc) yscs seqs_

        ( FreeScales xscs, FixedScale ysc ) ->
            List.map2 (\xsc seq -> inner xsc ysc seq) xscs seqs_

        ( FreeScales xscs, FreeScales yscs ) ->
            List.map3 inner xscs yscs seqs_


{-| Produce a columns chart, passing in an optional brush for interactive
selection. (See Example/Brush for a full example of how to set up brushes).

Example:

    chart : Svg msg
    chart =
        columns config (Just brush) series

-}
columns : Config msg -> Maybe (Brush OneDimensional) -> Series -> Svg msg
columns c mbrush data =
    columnsData c mbrush data |> .chart


{-| Same as [columns](#columns), but returning [ChartData](#ChartData)
with highlighted and selected (brushed) data, in addition to the chart.
-}
columnsData : Config msg -> Maybe (Brush OneDimensional) -> Series -> ChartData msg
columnsData c mbrush data =
    let
        b =
            cssBlock c

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
            columnsInnerData h xsc ysc c mbrush data_
    in
    { chart = svg w h [ inner.chart ]
    , selected = inner.selected
    , highlighted = inner.highlighted
    }


columnsInnerData :
    Float
    -> BandScale Time.Posix
    -> ContinuousScale Float
    -> Config msg
    -> Maybe (Brush OneDimensional)
    -> Series
    -> ChartData msg
columnsInnerData h xsc ysc c mbrush data =
    let
        pad =
            padding c

        b =
            cssBlock c

        e =
            b.element "columns"

        ec =
            b.element "column"

        cbar =
            color c

        chigh =
            highlightColor c

        wcol =
            observationWidth c

        bw =
            Scale.bandwidth xsc

        highlighted =
            data |> selectHighlights (highlight c)

        brush =
            viewBrush b (onBrush c) mbrush

        brushoverlay =
            mbrush
                |> Maybe.andThen
                    (\br -> columnsBrushOverlay b c xsc ysc br highlighted data)

        circles =
            highlightCircles
                b
                chigh
                (wcol * 2)
                ( bw / 2, 0.0 )
                (ColumnsScale xsc)
                ysc
                highlighted

        chart =
            S.g
                [ SA.transform [ ST.Translate pad pad ]
                ]
                (S.g
                    [ e |> element ]
                    (data |> List.map (columnInner ec cbar h pad xsc ysc highlighted))
                    :: (brushoverlay |> Maybe.map .chart |> Maybe.withDefault (text ""))
                    :: (brush |> Maybe.withDefault (text ""))
                    :: circles
                )
    in
    { chart = chart
    , selected = brushoverlay |> Maybe.map .selected |> Maybe.withDefault []
    , highlighted = highlighted
    }


columnInner :
    Bem.Element
    -> Paint
    -> Float
    -> Float
    -> BandScale Time.Posix
    -> ContinuousScale Float
    -> Series
    -> Observation
    -> Svg msg
columnInner e cbar h pad xsc ysc hs ( x, y ) =
    let
        ishigh =
            hs
                |> List.any
                    (\( x_, _ ) -> Time.posixToMillis x_ == Time.posixToMillis x)
    in
    S.rect
        [ e |> elementIf "highlight" ishigh
        , SA.x <| Num <| Scale.convert xsc x
        , SA.y <| Num <| Scale.convert ysc y
        , SA.width <| Num <| Scale.bandwidth xsc
        , SA.height <| Num <| (h - Scale.convert ysc y) - (pad * 2)
        , SA.fill cbar
        , SA.stroke cbar
        , SA.strokeWidth <| Num <| 0.25
        ]
        []



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


highlightedLowerTimeBounds : Series -> Series -> List ( Maybe Time.Posix, Time.Posix )
highlightedLowerTimeBounds hdata data =
    data
        |> List.map Tuple.first
        |> listLowerBoundsBy Time.posixToMillis
        |> List.filter (\( _, x0 ) -> List.any (\( x1, _ ) -> x0 == x1) hdata)


highlightedUpperTimeBounds : Series -> Series -> List ( Time.Posix, Maybe Time.Posix )
highlightedUpperTimeBounds hdata data =
    data
        |> List.map Tuple.first
        |> listUpperBoundsBy Time.posixToMillis
        |> List.filter (\( x0, _ ) -> List.any (\( x1, _ ) -> x0 == x1) hdata)


isInLowerTimeBounds : Time.Posix -> List ( Maybe Time.Posix, Time.Posix ) -> Bool
isInLowerTimeBounds x =
    List.any
        (\( mxmin, xmax ) ->
            case mxmin of
                Nothing ->
                    Time.posixToMillis x <= Time.posixToMillis xmax

                Just xmin ->
                    Time.posixToMillis x
                        <= Time.posixToMillis xmax
                        && Time.posixToMillis x
                        > Time.posixToMillis xmin
        )


isInUpperTimeBounds : Time.Posix -> List ( Time.Posix, Maybe Time.Posix ) -> Bool
isInUpperTimeBounds x =
    List.any
        (\( xmin, mxmax ) ->
            case mxmax of
                Nothing ->
                    Time.posixToMillis x >= Time.posixToMillis xmin

                Just xmax ->
                    Time.posixToMillis x
                        >= Time.posixToMillis xmin
                        && Time.posixToMillis x
                        < Time.posixToMillis xmax
        )


listLowerBoundsBy : (a -> comparable) -> List a -> List ( Maybe a, a )
listLowerBoundsBy fn list =
    let
        inner a ( acc, mprev ) =
            ( ( mprev, a ) :: acc, Just a )
    in
    list
        |> List.sortBy fn
        |> List.foldl inner ( [], Nothing )
        |> Tuple.first


listUpperBoundsBy : (a -> comparable) -> List a -> List ( a, Maybe a )
listUpperBoundsBy fn list =
    let
        inner a ( acc, mprev ) =
            ( ( a, mprev ) :: acc, Just a )
    in
    list
        |> List.sortBy fn
        |> List.foldr inner ( [], Nothing )
        |> Tuple.first



-- -----------------------------------------------------------------------------
-- SCALING
-- -----------------------------------------------------------------------------
{- Note: certain functions are used by both line and columns charts,
   this type allows polymorphism. It is only used internally.
-}


type ChartScaleX
    = LineScale (ContinuousScale Time.Posix)
    | ColumnsScale (BandScale Time.Posix)


scaledPoints :
    ( Float, Float )
    -> ChartScaleX
    -> ContinuousScale Float
    -> Series
    -> List (Maybe ( Float, Float ))
scaledPoints ( xadj, yadj ) xsc ysc =
    List.map (scaledPoint ( xadj, yadj ) xsc ysc)


scaledPoint :
    ( Float, Float )
    -> ChartScaleX
    -> ContinuousScale Float
    -> Observation
    -> Maybe ( Float, Float )
scaledPoint ( xadj, yadj ) xsc ysc ( x, y ) =
    case xsc of
        LineScale xsc_ ->
            Just ( Scale.convert xsc_ x + xadj, Scale.convert ysc y + yadj )

        ColumnsScale xsc_ ->
            Just ( Scale.convert xsc_ x + xadj, Scale.convert ysc y + yadj )


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
    ChartScaleX
    -> ContinuousScale Float
    -> Observation
    -> List (Maybe ( Float, Float ))
scaledVerticalLinePoints xsc ysc ( x, y ) =
    let
        cx =
            case xsc of
                LineScale xsc_ ->
                    Scale.convert xsc_ x

                ColumnsScale xsc_ ->
                    Scale.convert xsc_ x + (Scale.bandwidth xsc_ / 2.0)
    in
    [ Just ( cx, Scale.rangeExtent ysc |> Tuple.first )
    , Just ( cx, Scale.convert ysc y )
    ]


scaledHorizontalLinePoints :
    ChartScaleX
    -> ContinuousScale Float
    -> Observation
    -> List (Maybe ( Float, Float ))
scaledHorizontalLinePoints xsc ysc ( x, y ) =
    let
        xmin =
            case xsc of
                LineScale xsc_ ->
                    Scale.rangeExtent xsc_ |> Tuple.first

                ColumnsScale xsc_ ->
                    let
                        ( d0, d1 ) =
                            Scale.range xsc_
                    in
                    if d0 < d1 then
                        d0

                    else
                        d1

        cx =
            case xsc of
                LineScale xsc_ ->
                    Scale.convert xsc_ x

                ColumnsScale xsc_ ->
                    Scale.convert xsc_ x + (Scale.bandwidth xsc_ / 2.0)

        cy =
            Scale.convert ysc y
    in
    [ Just ( xmin, cy )
    , Just ( cx, cy )
    ]



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


selectedBrushedColumns :
    Brush OneDimensional
    -> BandScale Time.Posix
    -> Series
    -> Maybe Series
selectedBrushedColumns br xsc data =
    let
        mext =
            Brush.selection1d br

        invert =
            Scale.Band.Extra.invert (Scale.domain xsc) (Scale.range xsc)
    in
    mext
        |> Maybe.map (Tuple.mapBoth invert invert)
        |> Maybe.andThen
            (\pair ->
                case pair of
                    ( Nothing, _ ) ->
                        Nothing

                    ( _, Nothing ) ->
                        Nothing

                    ( Just smin, Just smax ) ->
                        Just <| selectInTimeExtent ( smin, smax ) data
            )


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


viewBrush : Bem.Block -> Maybe (OnBrush -> msg) -> Maybe (Brush OneDimensional) -> Maybe (Svg msg)
viewBrush b mmsg mbrush =
    let
        e =
            b.element "brush"

        brushrect ext attrs =
            S.rect
                ((SA.x <| Num <| ext.left)
                    :: (SA.y <| Num <| ext.top)
                    :: (SA.width <| Num <| ext.right - ext.left)
                    :: (SA.height <| Num <| ext.bottom - ext.top)
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
    -> Maybe (ChartData msg)
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
    -> ChartData msg
lineBrushOverlayHelp bapp ( mxlabels, mylabels ) b c xsc ysc brush hdata data =
    let
        e =
            b.element "brush-domain"

        ea =
            b.element "brush-domain-area"

        tint =
            timeInterval c

        tz =
            timeZone c

        mselectext =
            extentBrushed brush xsc

        hlbounds =
            highlightedLowerTimeBounds hdata data

        hubounds =
            highlightedUpperTimeBounds hdata data

        ( hlower, hupper ) =
            mselectext
                |> Maybe.map
                    (Tuple.mapBoth
                        (\x -> isInLowerTimeBounds x hlbounds)
                        (\x -> isInUpperTimeBounds x hubounds)
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
            brushOverlayBoundsAndLabels
                bapp
                ( mxlabels, mylabels )
                b
                tint
                tz
                c
                (LineScale xsc)
                ysc
                hlower
                hupper
                selected

        chart =
            S.g
                [ e |> element ]
                (Path.element brusharea
                    [ ea |> element
                    , SA.fill bapp.area
                    ]
                    :: brushlabels
                )
    in
    { chart = chart
    , selected = selected
    , highlighted = hdata
    }


columnsBrushOverlay :
    Bem.Block
    -> Config msg
    -> BandScale Time.Posix
    -> ContinuousScale Float
    -> Brush OneDimensional
    -> Series
    -> Series
    -> Maybe (ChartData msg)
columnsBrushOverlay b c xsc ysc brush hdata data =
    case brushing c of
        NoBrush ->
            Nothing

        BrushNoLabels { appearance } ->
            Just <|
                columnsBrushOverlayHelp appearance ( Nothing, Nothing ) b c xsc ysc brush hdata data

        BrushLabels { appearance, labels } ->
            Just <|
                columnsBrushOverlayHelp appearance ( Just labels, Just labels ) b c xsc ysc brush hdata data

        BrushLabelsX { appearance, labels } ->
            Just <|
                columnsBrushOverlayHelp appearance ( Just labels, Nothing ) b c xsc ysc brush hdata data

        BrushLabelsY { appearance, labels } ->
            Just <|
                columnsBrushOverlayHelp appearance ( Nothing, Just labels ) b c xsc ysc brush hdata data


columnsBrushOverlayHelp :
    BrushingAppearanceConfig
    -> ( Maybe BrushingLabelsConfig, Maybe BrushingLabelsConfig )
    -> Bem.Block
    -> Config msg
    -> BandScale Time.Posix
    -> ContinuousScale Float
    -> Brush OneDimensional
    -> Series
    -> Series
    -> ChartData msg
columnsBrushOverlayHelp bapp ( mxlabels, mylabels ) b c xsc ysc brush hdata data =
    let
        e =
            b.element "brush-domain"

        ec =
            b.element "brush-domain-column"

        tint =
            timeInterval c

        tz =
            timeZone c

        h =
            height c

        pad =
            padding c

        selected =
            selectedBrushedColumns brush xsc data
                |> Maybe.withDefault []

        ( hlower, hupper ) =
            ( List.head selected, List.last selected )
                |> Tuple.mapBoth
                    (Maybe.map
                        (\( x, _ ) -> isHighlightedTimeInterval Time.ceiling tint tz x hdata)
                        >> Maybe.withDefault False
                    )
                    (Maybe.map
                        (\( x, _ ) -> isHighlightedTimeInterval Time.floor tint tz x hdata)
                        >> Maybe.withDefault False
                    )

        brushlabels =
            brushOverlayBoundsAndLabels
                bapp
                ( mxlabels, mylabels )
                b
                tint
                tz
                c
                (ColumnsScale xsc)
                ysc
                hlower
                hupper
                selected

        chart =
            S.g
                [ e |> element ]
                (List.map (columnInner ec bapp.area h pad xsc ysc hdata) selected
                    ++ brushlabels
                )
    in
    { chart = chart
    , selected = selected
    , highlighted = hdata
    }


brushOverlayBoundsAndLabels :
    BrushingAppearanceConfig
    -> ( Maybe BrushingLabelsConfig, Maybe BrushingLabelsConfig )
    -> Bem.Block
    -> Time.Interval
    -> Time.Zone
    -> Config msg
    -> ChartScaleX
    -> ContinuousScale Float
    -> Bool
    -> Bool
    -> Series
    -> List (Svg msg)
brushOverlayBoundsAndLabels bapp ( mxlabels, mylabels ) b tint tz c xsc ysc hlower hupper selected =
    let
        e =
            b.element "brush-label-filter"

        filt =
            ST.Filter <| "url(#" ++ elementName e ++ ")"

        hfilt =
            ST.Filter <| "url(#" ++ elementNameMod e "highlight" ++ ")"

        inner ( ( ( vmin, vminl ), ( hmin, hminl ) ), ( ( vmax, vmaxl ), ( hmax, hmaxl ) ) ) =
            case ( mxlabels, mylabels ) of
                ( Nothing, Nothing ) ->
                    brushBoundsLines bapp b "x" hlower hupper ( vmin, vmax )

                ( Just xlabels, Nothing ) ->
                    [ brushLabelFilter (elementName e) bapp.bounds
                    , brushLabelFilter (elementNameMod e "highlight") bapp.highlight
                    , brushBoundsXLinesAndLabels
                        bapp
                        xlabels
                        filt
                        hfilt
                        b
                        c
                        hlower
                        hupper
                        ( ( vmin, vminl ), ( vmax, vmaxl ) )
                    ]

                ( Nothing, Just ylabels ) ->
                    [ brushLabelFilter (elementName e) bapp.bounds
                    , brushLabelFilter (elementNameMod e "highlight") bapp.highlight
                    , brushBoundsYLinesAndLabels
                        bapp
                        ylabels
                        filt
                        hfilt
                        b
                        c
                        hlower
                        hupper
                        ( ( hmin, hminl ), ( hmax, hmaxl ) )
                    ]

                ( Just xlabels, Just ylabels ) ->
                    [ brushLabelFilter (elementName e) bapp.bounds
                    , brushLabelFilter (elementNameMod e "highlight") bapp.highlight
                    , brushBoundsXLinesAndLabels
                        bapp
                        xlabels
                        filt
                        hfilt
                        b
                        c
                        hlower
                        hupper
                        ( ( vmin, vminl ), ( vmax, vmaxl ) )
                    , brushBoundsYLinesAndLabels
                        bapp
                        ylabels
                        filt
                        hfilt
                        b
                        c
                        hlower
                        hupper
                        ( ( hmin, hminl ), ( hmax, hmaxl ) )
                    ]
    in
    selectedBoundsAndLabels xsc ysc selected
        |> Maybe.map inner
        |> Maybe.withDefault []


selectedBoundsAndLabels :
    ChartScaleX
    -> ContinuousScale Float
    -> Series
    ->
        Maybe
            ( ( ( Path, ( Time.Posix, Float ) ), ( Path, ( Float, Float ) ) )
            , ( ( Path, ( Time.Posix, Float ) ), ( Path, ( Float, Float ) ) )
            )
selectedBoundsAndLabels xsc ysc data =
    let
        mext =
            data |> Statistics.extentBy (Tuple.first >> Time.posixToMillis)

        convert =
            case xsc of
                LineScale xsc_ ->
                    Scale.convert xsc_

                ColumnsScale xsc_ ->
                    Scale.convert xsc_

        inner ( x, y ) =
            ( ( scaledVerticalLinePoints xsc ysc ( x, y ) |> Shape.line Shape.linearCurve
              , ( x
                , convert x
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
    -> Bool
    -> Bool
    -> ( Path, Path )
    -> List (Svg msg)
brushBoundsLines bapp b dim hlower hupper ( p0, p1 ) =
    let
        ep =
            b.element "brush-domain-bound"

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
    -> ST.Filter
    -> ST.Filter
    -> Bem.Block
    -> Config msg
    -> Bool
    -> Bool
    -> ( ( Path, ( Time.Posix, Float ) ), ( Path, ( Time.Posix, Float ) ) )
    -> Svg msg
brushBoundsXLinesAndLabels bapp bl filt hfilt b c hlower hupper ( ( p0, ( t0, x0 ) ), ( p1, ( t1, x1 ) ) ) =
    let
        e =
            b.element "brush-domain-bounds"

        el =
            b.element "brush-label"

        pad =
            padding c

        h =
            height c

        w =
            width c

        ybaseline =
            h - (pad * 2)

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
            bl.color

        hcolor =
            bl.color

        fsize =
            bl.size

        tz =
            timeZone c

        l0 =
            bl.labelX tz t0

        l1 =
            bl.labelX tz t1
    in
    S.g
        [ e |> elementOf "dim" "x" ]
        (brushBoundsLines bapp b "x" hlower hupper ( p0, p1 )
            ++ [ S.text_
                    [ el |> elementOfList [ ( "dim", "x" ), ( "type", "lower" ) ]
                    , el |> elementIf "highlight" hlower
                    , SA.filter <|
                        if hlower then
                            hfilt

                        else
                            filt
                    , SA.transform [ ST.Translate (x0 + x0adj) ybaseline ]
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
                    , SA.filter <|
                        if hupper then
                            hfilt

                        else
                            filt
                    , SA.transform [ ST.Translate (x1 + x1adj) ybaseline ]
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
    -> ST.Filter
    -> ST.Filter
    -> Bem.Block
    -> Config msg
    -> Bool
    -> Bool
    -> ( ( Path, ( Float, Float ) ), ( Path, ( Float, Float ) ) )
    -> Svg msg
brushBoundsYLinesAndLabels bapp bl filt hfilt b c hlower hupper ( ( p0, ( v0, y0 ) ), ( p1, ( v1, y1 ) ) ) =
    let
        e =
            b.element "brush-domain-bounds"

        el =
            b.element "brush-label"

        pad =
            padding c

        h =
            height c

        xbaseline =
            0.0

        l0 =
            v0 |> bl.labelY

        l1 =
            v1 |> bl.labelY

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
            bl.color

        hcolor =
            bl.color

        fsize =
            bl.size
    in
    S.g
        [ e |> elementOf "dim" "y" ]
        (brushBoundsLines bapp b "y" hlower hupper ( p0, p1 )
            ++ [ S.text_
                    [ el |> elementOfList [ ( "dim", "y" ), ( "type", "lower" ) ]
                    , el |> elementIf "highlight" hlower
                    , SA.filter <|
                        if hlower then
                            hfilt

                        else
                            filt
                    , SA.transform [ ST.Translate xbaseline (y0 + y0adj) ]
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
                    , SA.filter <|
                        if hupper then
                            hfilt

                        else
                            filt
                    , SA.transform [ ST.Translate xbaseline (y1 + y1adj) ]
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


brushLabelFilter : String -> Paint -> Svg msg
brushLabelFilter idname p =
    let
        fc =
            floodColorFromPaint p
    in
    S.filter
        [ SA.id idname
        , SA.x <| Num -0.05
        , SA.y <| Num -0.05
        , SA.width <| Num 1.1
        , SA.height <| Num 1.1
        ]
        [ SF.flood
            [ floodColor fc
            ]
            []
        , SF.composite
            [ SFA.in_ ST.InSourceGraphic
            , SFA.compositeOperator ST.CompositeOperatorOver
            ]
            []
        ]


floodColorFromPaint : ST.Paint -> ST.FloodColor
floodColorFromPaint p =
    case p of
        Paint c ->
            ST.Flood c

        CSSVariable s ->
            ST.FloodICC s

        Reference s ->
            ST.FloodICC s

        ContextFill ->
            ST.FloodCurrentColor

        ContextStroke ->
            ST.FloodCurrentColor

        PaintNone ->
            ST.FloodICC "none"



{- Note: hack around TypedSvg bug -}


floodColor : ST.FloodColor -> Attribute x
floodColor fc =
    case fc of
        ST.FloodInherit ->
            attribute "flood-color" "inherit"

        ST.FloodCurrentColor ->
            attribute "flood-color" "currentColor"

        ST.Flood c ->
            attribute "flood-color" <| Color.toCssString c

        ST.FloodICC icc ->
            attribute "flood-color" icc


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
    case tint of
        Time.Year ->
            let
                tparts =
                    Time.posixToParts tz t
            in
            tparts.year |> String.fromInt

        _ ->
            "TODO"



-- -----------------------------------------------------------------------------
-- UTILS
-- -----------------------------------------------------------------------------


svg : Float -> Float -> List (Svg msg) -> Svg msg
svg w h =
    S.svg
        [ SA.viewBox 0 0 w h ]
