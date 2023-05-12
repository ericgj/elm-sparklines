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
import Color
import DateFormat as DF
import Facet exposing (Scaling(..))
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Bem as Bem exposing (elementIf)
import Sparklines exposing (Highlight(..))
import Time
import Time.Extra as Time
import Timeseries exposing (Observation, Series)
import TypedSvg.Types exposing (Paint(..))


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



{- Note: don't take padding into account, because the brush will be rendered
   within the padding automatically.
-}


extent : Frame -> Brush.Extent
extent { height, width } =
    { top = 0
    , bottom = height
    , left = 0
    , right = width
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


line : Time.Zone -> Series -> Model -> Html Msg
line tz data { frame, brush } =
    let
        c =
            Sparklines.lineConfig tz frame.width frame.height
                |> Sparklines.withPadding frame.padding
                |> Sparklines.withHighlight HighlightMinMax
                |> Sparklines.withBrushLabels
                    UpdateBrush
                    Sparklines.defaultBrushingAppearance
                |> Sparklines.formattingLabelsX year
                |> Sparklines.coloringLabels (Paint <| Color.rgb 1 0.5 1)

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
                    li [] [ viewSelected year tz chdata.highlighted obs ]
                )
                chdata.selected
        ]


lineFacetsFreeY : Time.Zone -> List Series -> Model -> Html Msg
lineFacetsFreeY tz data { frame, brush } =
    let
        c =
            Sparklines.lineConfig tz frame.width frame.height
                |> Sparklines.withPadding frame.padding
                |> Sparklines.withHighlight HighlightMinMax
                |> Sparklines.withBrushLabels
                    UpdateBrush
                    Sparklines.defaultBrushingAppearance
                |> Sparklines.formattingLabelsX year

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
                |> Sparklines.formattingLabelsX monthShortYear
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
                    li [] [ viewSelected monthShortYear tz chdata.highlighted obs ]
                )
                chdata.selected
        ]


year : Time.Zone -> Time.Posix -> String
year =
    DF.format [ DF.yearNumber ]


monthShortYear : Time.Zone -> Time.Posix -> String
monthShortYear =
    DF.format
        [ DF.monthNameAbbreviated, DF.text " ", DF.yearNumberLastTwo ]


viewSelected : (Time.Zone -> Time.Posix -> String) -> Time.Zone -> Series -> Observation -> Html msg
viewSelected fmt tz hdata ( x, y ) =
    let
        e =
            Bem.init "examples-brush" |> (\b -> b.element "selected")

        ishigh =
            hdata
                |> List.any
                    (\( x_, _ ) -> Time.posixToMillis x_ == Time.posixToMillis x)
    in
    div
        [ e |> elementIf "highlighted" ishigh ]
        [ span
            []
            [ text <| fmt tz x ]
        , span
            []
            [ text ":" ]
        , span
            []
            [ text <| String.fromInt <| round <| y ]
        ]
