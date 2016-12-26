module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Maybe exposing (withDefault)


main : Program Never Model Msg
main =
    Html.program
        { init = init "cats"
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { topic : String
    , gifUrl : String
    , error : Maybe Http.Error
    }


init : String -> ( Model, Cmd Msg )
init topic =
    ( Model topic "waiting.gif" Nothing
    , getRandomGif topic
    )



-- UPDATE


type Msg
    = MorePlease
    | NewGif (Result Http.Error String)
    | NewTopic String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( model, getRandomGif model.topic )

        NewGif (Ok newUrl) ->
            ( Model model.topic newUrl, Cmd.none )

        NewGif (Err error) ->
            ( { model | error = Just error }, Cmd.none )

        NewTopic topic ->
            ( { model | topic = topic }, Cmd.none )



-- VIEW


errorMessage : Maybe Http.Error -> String
errorMessage maybe =
    case maybe of
        Nothing ->
            ""

        Just error ->
            case error of
                Http.BadUrl url ->
                    "Malformed URL \"" ++ url ++ "\""

                Http.Timeout ->
                    "Request timed out"

                Http.NetworkError ->
                    "Network error"

                Http.BadStatus response ->
                    "Bad status: " ++ toString response.status.code

                Http.BadPayload payload response ->
                    "Invalid payload" ++ toString payload


errorView : Model -> Html msg
errorView model =
    div []
        [ p [] [ text "error" ]
        , p [] [ text (errorMessage model.error) ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.topic ]
        , input [ type_ "text", onInput NewTopic ] []
        , br [] []
        , select
            [ name "select", onInput NewTopic ]
            [ option
                [ value "cats", selected True ]
                [ text "cats" ]
            , option
                [ value "dogs" ]
                [ text "dogs" ]
            , option
                [ value "horses" ]
                [ text "horses" ]
            ]
        , button [ onClick MorePlease ] [ text "More Please!" ]
        , br [] []
        , img [ src model.gifUrl ] []
        , br [] []
        , errorView model
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getRandomGif : String -> Cmd Msg
getRandomGif topic =
    let
        url =
            "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic

        request =
            Http.get url decodeGifUrl
    in
        Http.send NewGif request


decodeGifUrl : Decode.Decoder String
decodeGifUrl =
    Decode.at [ "data", "image_url" ] Decode.string
