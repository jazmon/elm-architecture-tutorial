module Main exposing (..)

import Html exposing (Html, div, p)
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Date exposing (fromTime)
import Array


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    Time


init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )



-- UPDATE


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( newTime, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick



-- VIEW


hourLine : Model -> List (Attribute msg)
hourLine model =
    let
        angle =
            turns (Time.inHours model) / 12

        handX =
            toString (50 + 40 * cos angle)

        handY =
            toString (50 + 40 * sin angle)
    in
        [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#000f1a" ]


minuteLine : Model -> List (Attribute msg)
minuteLine model =
    let
        angle =
            turns (Time.inHours model)

        handX =
            toString (50 + 40 * cos angle)

        handY =
            toString (50 + 40 * sin angle)
    in
        [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ]


secondLine : Model -> List (Attribute msg)
secondLine model =
    let
        angle =
            turns (Time.inMinutes model)

        handX =
            toString (50 + 40 * cos angle)

        handY =
            toString (50 + 40 * sin angle)
    in
        [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#cb1122" ]


hourIndicator : number -> Svg msg
hourIndicator num =
    line [ id <| "hour" ++ toString (num), x1 "50", y1 "10", x2 "50", y2 "0", stroke "#cb1122" ] []


timeView : Model -> Html msg
timeView model =
    div []
        [ p [] [ text <| getHour model ++ ":" ++ getMinute model ++ ":" ++ getSecond model ] ]


view : Model -> Html Msg
view model =
    let
        radius : number
        radius =
            50

        hours =
            Array.initialize 11 identity
    in
        div
            [ Html.Attributes.style
                [ ( "margin", "1rem" )
                , ( "display", "flex" )
                , ( "height", "100%" )
                , ( "flex-direction", "column" )
                , ( "align-items", "center" )
                , ( "justify-content", "center" )
                ]
            ]
            [ svg
                [ viewBox "0 0 100 100"
                , width "300px"
                , Html.Attributes.style [ ( "flex-grow", "0" ) ]
                ]
                [ g []
                    [ circle
                        [ cx <| toString radius
                        , cy <| toString radius
                        , r <| toString radius
                        , fill "#0B79CE"
                        ]
                        []
                    ]
                , g []
                    (Array.toList
                        (Array.map hourIndicator hours)
                    )
                , g []
                    [ line (hourLine model) []
                    , line (secondLine model) []
                    , line (minuteLine model) []
                    ]
                ]
            , timeView model
            ]



-- utils


getHour : Model -> String
getHour model =
    toString <| Date.hour <| fromTime model


getMinute : Model -> String
getMinute model =
    toString <| Date.minute <| fromTime model


getSecond : Model -> String
getSecond model =
    toString <| Date.second <| fromTime model
