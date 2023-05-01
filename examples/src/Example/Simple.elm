module Example.Simple exposing (line, lineFacetsFreeY, lines)

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
            View.Simple.defaultLineConfig tz 100 40
                |> View.Simple.withHighlight HighlightMinMax
    in
    div [ style "width" "200px" ]
        [ View.Simple.line c data
        ]


lines : Time.Zone -> List Series -> Html msg
lines tz datas =
    let
        c =
            View.Simple.defaultLineConfig tz 100 40
                |> View.Simple.withHighlight HighlightMinMax
    in
    div [ style "width" "200px" ]
        [ View.Simple.lines View.Simple.defaultColorPairs c datas
        ]


lineFacetsFreeY : Time.Zone -> List Series -> Html msg
lineFacetsFreeY tz datas =
    let
        c =
            View.Simple.defaultLineConfig tz 100 50
                |> View.Simple.withHighlight HighlightMinMax
    in
    div [ style "width" "200px" ]
        (View.Simple.lineFacets
            { x = Fixed, y = Free }
            c
            datas
        )
