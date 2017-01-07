module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (append)
import Random
import Array exposing (Array, fromList, map)


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
    { dieFaces : Array Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model <| fromList [ 1, 1 ], Cmd.none )



-- UPDATE


type Msg
    = Roll
    | NewFace (Array Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, Array.map Random.generate NewFace (Random.int 1 6) model.dieFaces )

        NewFace newFace ->
            ( Model newFace, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ img [ src <| append "/examples/img/dice-" <| toString model.dieFaces ++ ".png", alt "dice" ] []
        , br [] []
        , button [ onClick Roll ] [ text "Roll" ]
        ]
