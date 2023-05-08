module Example.Brush exposing
    ( Frame
    , Model
    , Msg
    , columns
    , init
    , line
    , lineFacetsFreeY
    , subscriptions
    , update
    )

import Brush exposing (Brush, OnBrush, OneDimensional)
import Facet exposing (Scaling(..))
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Bem as Bem exposing (elementIf)
import Sparklines exposing (Highlight(..))
import Time
import Time.Extra as Time
import Timeseries exposing (Observation, Series)


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
            Sparklines.lineConfig tint tz frame.width frame.height
                |> Sparklines.withPadding frame.padding
                |> Sparklines.withHighlight HighlightMinMax
                |> Sparklines.withBrushLabels
                    UpdateBrush
                    Sparklines.defaultBrushingAppearance
                    Sparklines.defaultBrushingLabels

        wstr =
            frame.width |> round |> String.fromInt

        chdata =
            Sparklines.lineData c (Just brush) data
    in
    div
        []
        [ div [ style "width" <| wstr ++ "px" ]
            [ chdata.chart
            ]
        , ul [] <|
            List.map
                (\obs ->
                    li [] [ viewSelected tz chdata.highlighted obs ]
                )
                chdata.selected
        ]


lineFacetsFreeY : Time.Interval -> Time.Zone -> List Series -> Model -> Html Msg
lineFacetsFreeY tint tz data { frame, brush } =
    let
        c =
            Sparklines.lineConfig tint tz frame.width frame.height
                |> Sparklines.withPadding frame.padding
                |> Sparklines.withHighlight HighlightMinMax
                |> Sparklines.withBrushLabels
                    UpdateBrush
                    Sparklines.defaultBrushingAppearance
                    Sparklines.defaultBrushingLabels

        wstr =
            frame.width |> round |> String.fromInt
    in
    div
        []
        [ div [ style "width" <| wstr ++ "px" ]
            (Sparklines.lineFacets
                { x = Fixed, y = Free }
                c
                (Just brush)
                data
            )
        ]


columns : Time.Interval -> Time.Zone -> Series -> Model -> Html Msg
columns tint tz data { frame, brush } =
    let
        c =
            Sparklines.columnsConfig List.sum tint tz frame.width frame.height
                |> Sparklines.withPadding 5.0
                |> Sparklines.withHighlight HighlightMinMax
                |> Sparklines.withBrushLabels
                    UpdateBrush
                    Sparklines.defaultBrushingAppearance
                    Sparklines.defaultBrushingLabels
                |> Sparklines.withBandConfig
                    { paddingInner = 0.3
                    , paddingOuter = 0.0
                    , align = 0.5
                    }

        wstr =
            frame.width |> round |> String.fromInt

        chdata =
            Sparklines.columnsData c (Just brush) data
    in
    div
        []
        [ div
            [ style "width" <| wstr ++ "px" ]
            [ chdata.chart ]
        , ul [] <|
            List.map
                (\obs ->
                    li [] [ viewSelected tz chdata.highlighted obs ]
                )
                chdata.selected
        ]


viewSelected : Time.Zone -> Series -> Observation -> Html msg
viewSelected tz hdata ( x, y ) =
    let
        e =
            Bem.init "examples-brush" |> (\b -> b.element "selected")

        ishigh =
            hdata
                |> List.any
                    (\( x_, _ ) -> Time.posixToMillis x_ == Time.posixToMillis x)

        parts =
            Time.posixToParts tz x
    in
    div
        [ e |> elementIf "highlighted" ishigh ]
        [ span
            []
            [ text <| String.fromInt <| parts.year ]
        , span
            []
            [ text ":" ]
        , span
            []
            [ text <| String.fromInt <| round <| y ]
        ]
