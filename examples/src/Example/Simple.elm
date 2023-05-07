module Example.Simple exposing (columns, line, lineFacetsFreeY, lines)

import Facet exposing (Scaling(..))
import Html exposing (..)
import Html.Attributes exposing (style)
import Time
import Time.Extra as Time
import Timeseries exposing (Series)
import View.Simple exposing (Highlight(..))


line : Time.Interval -> Time.Zone -> Series -> Html msg
line tint tz data =
    let
        c =
            View.Simple.lineConfig tint tz 100 40
                |> View.Simple.withPadding 5.0
                |> View.Simple.withHighlight HighlightMinMax
    in
    div [ style "width" "200px" ]
        [ View.Simple.line c Nothing data
        ]


lines : Time.Interval -> Time.Zone -> List Series -> Html msg
lines tint tz datas =
    let
        c =
            View.Simple.lineConfig tint tz 100 40
                |> View.Simple.withPadding 5.0
                |> View.Simple.withHighlight HighlightMinMax
    in
    div [ style "width" "200px" ]
        [ View.Simple.lines View.Simple.defaultColorPairs c Nothing datas
        ]


lineFacetsFreeY : Time.Interval -> Time.Zone -> List Series -> Html msg
lineFacetsFreeY tint tz datas =
    let
        c =
            View.Simple.lineConfig tint tz 100 50
                |> View.Simple.withPadding 5.0
                |> View.Simple.withHighlight HighlightMinMax
    in
    div [ style "width" "200px" ]
        (View.Simple.lineFacets
            { x = Fixed, y = Free }
            c
            Nothing
            datas
        )


columns : Time.Interval -> Time.Zone -> Series -> Html msg
columns tint tz data =
    let
        c =
            View.Simple.columnsConfig List.sum tint tz 100 40
                |> View.Simple.withPadding 5.0
                |> View.Simple.withHighlight HighlightMinMax
                |> View.Simple.withBandConfig
                    { paddingInner = 0.3
                    , paddingOuter = 0.0
                    , align = 0.5
                    }
    in
    div [ style "width" "200px" ]
        [ View.Simple.columns c Nothing data
        ]
