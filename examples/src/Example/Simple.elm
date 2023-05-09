module Example.Simple exposing (columns, line, lineFacetsFreeY, lines)

import Facet exposing (Scaling(..))
import Html exposing (..)
import Html.Attributes exposing (style)
import Sparklines exposing (Highlight(..))
import Time
import Time.Extra as Time
import Timeseries exposing (Series)


line : Time.Zone -> Series -> Html msg
line tz data =
    let
        c =
            Sparklines.lineConfig tz 200 80
                |> Sparklines.withPadding 5.0
                |> Sparklines.withHighlight HighlightMinMax
    in
    div [ style "width" "200px" ]
        [ Sparklines.line c Nothing data
        ]


lines : Time.Zone -> List Series -> Html msg
lines tz datas =
    let
        c =
            Sparklines.lineConfig tz 200 80
                |> Sparklines.withPadding 5.0
                |> Sparklines.withHighlight HighlightMinMax
    in
    div [ style "width" "200px" ]
        [ Sparklines.lines Sparklines.defaultColorPairs c datas
        ]


lineFacetsFreeY : Time.Zone -> List Series -> Html msg
lineFacetsFreeY tz datas =
    let
        c =
            Sparklines.lineConfig tz 200 80
                |> Sparklines.withPadding 5.0
                |> Sparklines.withHighlight HighlightMinMax
    in
    div [ style "width" "200px" ]
        (Sparklines.lineFacets
            { x = Fixed, y = Free }
            c
            Nothing
            datas
        )


columns : Time.Interval -> Time.Zone -> Series -> Html msg
columns tint tz data =
    let
        c =
            Sparklines.columnsConfig List.sum tint tz 200 80
                |> Sparklines.withPadding 5.0
                |> Sparklines.withHighlight HighlightMinMax
                |> Sparklines.withBandConfig
                    { paddingInner = 0.3
                    , paddingOuter = 0.0
                    , align = 0.5
                    }
    in
    div [ style "width" "200px" ]
        [ Sparklines.columns c Nothing data
        ]
