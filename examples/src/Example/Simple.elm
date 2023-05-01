module Example.Simple exposing (columns, line, lineFacetsFreeY, lines)

import Facet exposing (Scaling(..))
import Html exposing (..)
import Html.Attributes exposing (style)
import Time
import Time.Extra as Time
import Timeseries exposing (Series)
import View.Simple exposing (Highlight(..))


line : Time.Zone -> Series -> Html msg
line tz data =
    let
        c =
            View.Simple.lineConfig tz 100 40
                |> View.Simple.withPadding 5.0
                |> View.Simple.withHighlight HighlightMinMax
    in
    div [ style "width" "200px" ]
        [ View.Simple.line c data
        ]


lines : Time.Zone -> List Series -> Html msg
lines tz datas =
    let
        c =
            View.Simple.lineConfig tz 100 40
                |> View.Simple.withPadding 5.0
                |> View.Simple.withHighlight HighlightMinMax
    in
    div [ style "width" "200px" ]
        [ View.Simple.lines View.Simple.defaultColorPairs c datas
        ]


lineFacetsFreeY : Time.Zone -> List Series -> Html msg
lineFacetsFreeY tz datas =
    let
        c =
            View.Simple.lineConfig tz 100 50
                |> View.Simple.withPadding 5.0
                |> View.Simple.withHighlight HighlightMinMax
    in
    div [ style "width" "200px" ]
        (View.Simple.lineFacets
            { x = Fixed, y = Free }
            c
            datas
        )


columns : Time.Zone -> Series -> Html msg
columns tz data =
    let
        c =
            View.Simple.columnsConfig List.sum Time.Year tz 100 40
                |> View.Simple.withPadding 5.0
                |> View.Simple.withHighlight HighlightMinMax
                |> View.Simple.withBandConfig
                    { paddingInner = 0.3
                    , paddingOuter = 0.0
                    , align = 0.5
                    }
    in
    div [ style "width" "200px" ]
        [ View.Simple.columns c data
        ]
