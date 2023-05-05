module Example.Brush exposing (Frame, Model, Msg, init, line, lineFacetsFreeY, subscriptions, update)

import Brush exposing (Brush, OnBrush, OneDimensional)
import Facet exposing (Scaling(..))
import Html exposing (..)
import Html.Attributes exposing (style)
import Time
import Time.Extra as Time
import Timeseries exposing (Series)
import View.Simple exposing (Highlight(..))


type alias Model =
    { frame : Frame
    , brush : Brush OneDimensional
    }


type alias Frame =
    { height : Float, width : Float, padding : Float }


init : Frame -> Model
init frame =
    { frame = frame
    , brush = Brush.initX <| extent frame
    }


extent : Frame -> Brush.Extent
extent { height, width, padding } =
    { top = padding
    , bottom = height - padding
    , left = padding
    , right = width - padding
    }


type Msg
    = UpdateBrush OnBrush


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateBrush b ->
            { model | brush = Brush.update b model.brush }


subscriptions : Model -> Sub Msg
subscriptions model =
    Brush.subscriptions model.brush UpdateBrush


line : Time.Interval -> Time.Zone -> Series -> Model -> Html Msg
line tint tz data { frame, brush } =
    let
        c =
            View.Simple.lineConfig tint tz frame.width frame.height
                |> View.Simple.withPadding frame.padding
                |> View.Simple.withHighlight HighlightMinMax
                |> View.Simple.withBrushLabels
                    UpdateBrush
                    View.Simple.defaultBrushingAppearance
                    View.Simple.defaultBrushingLabels

        wstr =
            frame.width |> round |> String.fromInt
    in
    div
        []
        [ div [ style "width" <| wstr ++ "px" ]
            [ View.Simple.line c (Just brush) data
            ]
        , ul []
            (viewSelected tz data frame.padding brush
                |> List.map (\i -> li [] [ i ])
            )
        ]


lineFacetsFreeY : Time.Interval -> Time.Zone -> List Series -> Model -> Html Msg
lineFacetsFreeY tint tz data { frame, brush } =
    let
        c =
            View.Simple.lineConfig tint tz frame.width frame.height
                |> View.Simple.withPadding frame.padding
                |> View.Simple.withHighlight HighlightMinMax
                |> View.Simple.withBrushLabels
                    UpdateBrush
                    View.Simple.defaultBrushingAppearance
                    View.Simple.defaultBrushingLabels

        wstr =
            frame.width |> round |> String.fromInt
    in
    div
        []
        [ div [ style "width" <| wstr ++ "px" ]
            (View.Simple.lineFacets
                { x = Fixed, y = Free }
                c
                (Just brush)
                data
            )
        ]


viewSelected : Time.Zone -> Series -> Float -> Brush OneDimensional -> List (Html msg)
viewSelected tz data pad brush =
    -- NOTE this can't work yet because we need the x-scale to calculate
    [ text "" ]
