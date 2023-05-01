module Main exposing (main)

import Browser
import Data.Incarceration as Incarceration exposing (Incarceration, Region(..))
import Example.Simple
import Html exposing (..)
import Html.Attributes exposing (classList, style)
import Html.Bem as Bem exposing (elementList)
import Html.Events exposing (onClick)
import Http
import Http.Extra
import Time exposing (Month(..))
import Time.Extra as Time
import Timeseries exposing (Series)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


type Model
    = Model HttpState ModelData


type alias ModelData =
    { timeZone : Time.Zone
    , data : List Incarceration
    , simpleLine : Switch ()
    , simpleLines : Switch ()
    , simpleLineFacetsFreeY : Switch ()
    }



-- HTTP STATE


type HttpState
    = Loading
    | Loaded
    | Failed Http.Error


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        Loading
        { data = []
        , timeZone = Time.utc
        , simpleLine = switchOff ()
        , simpleLines = switchOff ()
        , simpleLineFacetsFreeY = switchOff ()
        }
    , loadData
    )



-- UPDATE


type Msg
    = Received (Result Http.Error (List Incarceration))
    | ToggleSimpleLine
    | UpdateSimpleLine ()
    | ToggleSimpleLines
    | UpdateSimpleLines ()
    | ToggleSimpleLineFacetsFreeY
    | UpdateSimpleLineFacetsFreeY ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updateNoCmd msg model |> noCmd


updateNoCmd : Msg -> Model -> Model
updateNoCmd msg model =
    case ( msg, model ) of
        ( Received (Err err), Model _ m ) ->
            Model (Failed err) m

        ( Received (Ok data), Model _ m ) ->
            Model Loaded { m | data = data }

        ( ToggleSimpleLine, Model s m ) ->
            Model s { m | simpleLine = toggle m.simpleLine }

        ( UpdateSimpleLine _, _ ) ->
            model

        ( ToggleSimpleLines, Model s m ) ->
            Model s { m | simpleLines = toggle m.simpleLines }

        ( UpdateSimpleLines _, _ ) ->
            model

        ( ToggleSimpleLineFacetsFreeY, Model s m ) ->
            Model s { m | simpleLineFacetsFreeY = toggle m.simpleLineFacetsFreeY }

        ( UpdateSimpleLineFacetsFreeY _, _ ) ->
            model


noCmd : a -> ( a, Cmd x )
noCmd a =
    ( a, Cmd.none )



-- CMD


loadData : Cmd Msg
loadData =
    Http.get
        { url = "sourcedata/incarceration.csv"
        , expect = Http.Extra.expectCsv Received Incarceration.csvDecode
        }



-- VIEW


view : Model -> Html Msg
view (Model st m) =
    let
        b =
            Bem.init "examples"
    in
    div []
        [ h1 [] [ text "Sparklines Examples" ]
        , hr [] []
        , viewHttpState st
        , m.simpleLine
            |> viewSwitch
                b
                "Simple line"
                (\_ ->
                    Example.Simple.line m.timeZone (totalSeries m.timeZone m.data)
                )
                ToggleSimpleLine
                UpdateSimpleLine
        , m.simpleLines
            |> viewSwitch
                b
                "Simple lines"
                (\_ ->
                    Example.Simple.lines
                        m.timeZone
                        [ femaleBlackSeries m.timeZone m.data
                        , femaleWhiteSeries m.timeZone m.data
                        ]
                )
                ToggleSimpleLines
                UpdateSimpleLines
        , m.simpleLineFacetsFreeY
            |> viewSwitch
                b
                "Simple line facets, free Y"
                (\_ ->
                    Example.Simple.lineFacetsFreeY
                        m.timeZone
                        [ maleBlackSeries m.timeZone m.data
                        , maleWhiteSeries m.timeZone m.data
                        , femaleBlackSeries m.timeZone m.data
                        , femaleWhiteSeries m.timeZone m.data
                        ]
                )
                ToggleSimpleLineFacetsFreeY
                UpdateSimpleLineFacetsFreeY
        ]


viewHttpState : HttpState -> Html msg
viewHttpState st =
    case st of
        Loading ->
            div [] [ text "Loading..." ]

        Loaded ->
            text ""

        Failed (Http.BadUrl s) ->
            div [] [ text <| "Bad URL: " ++ s ]

        Failed Http.Timeout ->
            div [] [ text <| "HTTP timed out." ]

        Failed Http.NetworkError ->
            div [] [ text <| "HTTP network error." ]

        Failed (Http.BadStatus code) ->
            div [] [ text <| "HTTP request returned error " ++ (code |> String.fromInt) ]

        Failed (Http.BadBody s) ->
            div [] [ text <| "Decoding error: " ++ s ]



-- SAMPLE DATA SERIES


totalSeries : Time.Zone -> List Incarceration -> Series
totalSeries =
    selectSeries (\i -> (i.totalM + i.totalF) |> toFloat)


maleTotalSeries : Time.Zone -> List Incarceration -> Series
maleTotalSeries =
    selectSeries (\i -> i.totalM |> toFloat)


femaleTotalSeries : Time.Zone -> List Incarceration -> Series
femaleTotalSeries =
    selectSeries (\i -> i.totalF |> toFloat)


femaleBlackSeries : Time.Zone -> List Incarceration -> Series
femaleBlackSeries =
    selectSeries (\i -> i.blackF |> toFloat)


femaleWhiteSeries : Time.Zone -> List Incarceration -> Series
femaleWhiteSeries =
    selectSeries (\i -> i.whiteF |> toFloat)


maleBlackSeries : Time.Zone -> List Incarceration -> Series
maleBlackSeries =
    selectSeries (\i -> i.blackM |> toFloat)


maleWhiteSeries : Time.Zone -> List Incarceration -> Series
maleWhiteSeries =
    selectSeries (\i -> i.whiteM |> toFloat)


selectSeries : (Incarceration -> Float) -> Time.Zone -> List Incarceration -> Series
selectSeries fn tz data =
    data
        |> List.filter (\i -> i.region == USTotal)
        |> List.map
            (\i ->
                ( yearToPosix tz i.year, fn i )
            )
        |> Timeseries.groupByIntervals List.sum Time.Year tz


yearToPosix : Time.Zone -> Int -> Time.Posix
yearToPosix tz year =
    Time.Parts year Jan 1 0 0 0 0 |> Time.partsToPosix tz



-- SWITCH


type Switch a
    = Switch ( a, Bool )


switchOn : a -> Switch a
switchOn a =
    Switch ( a, True )


switchOff : a -> Switch a
switchOff a =
    Switch ( a, False )


switchMap : (a -> b) -> Switch a -> Switch b
switchMap fn (Switch ( a, st )) =
    Switch ( fn a, st )


toggle : Switch a -> Switch a
toggle (Switch ( a, st )) =
    Switch ( a, not st )


viewSwitch : Bem.Block -> String -> (a -> Html msg) -> Msg -> (msg -> Msg) -> Switch a -> Html Msg
viewSwitch b title subview toggleMsg toMsg (Switch ( a, isOn )) =
    let
        e =
            b.element "switch"

        ec =
            b.element "switch-caret"

        inner =
            if isOn then
                [ a |> subview |> Html.map toMsg ]

            else
                []
    in
    div
        [ e |> elementList [ ( "on", isOn ), ( "off", not isOn ) ] ]
        [ h2 []
            [ span
                [ cursor "pointer"
                , Bem.element ec
                , classList
                    [ ( "fa-solid", True )
                    , ( "fa-caret-down", isOn )
                    , ( "fa-caret-right", not isOn )
                    ]
                , onClick toggleMsg
                ]
                []
            , span [] [ text title ]
            ]
        , div [] inner
        ]


cursor : String -> Html.Attribute msg
cursor s =
    style "cursor" s
