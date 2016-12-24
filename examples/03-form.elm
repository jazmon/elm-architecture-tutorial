module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String exposing (any)
import Char exposing (isDigit, isUpper, isLower)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { name : String
    , age : Int
    , memer : Bool
    , gender : MaybeGender
    , password : String
    , passwordAgain : String
    , validate : Bool
    }


type Gender
    = Male
    | Female
    | Other


type MaybeGender
    = Just Gender
    | Nothing


model : Model
model =
    Model "" 0 False Nothing "" "" False



-- UPDATE


type Msg
    = Name String
    | Age String
    | Gender Gender
    | Password String
    | PasswordAgain String
    | Validate
    | Memer


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model
                | name = name
                , validate = False
            }

        Age age ->
            { model | age = Result.withDefault 0 (String.toInt age) }

        Memer ->
            { model
                | memer = not model.memer
                , validate = False
            }

        Gender gender ->
            { model
                | gender = Just gender
                , validate = False
            }

        Password password ->
            { model
                | password = password
                , validate = False
            }

        PasswordAgain password ->
            { model
                | passwordAgain = password
                , validate = False
            }

        Validate ->
            { model | validate = True }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Name", onInput Name ] []
        , br [] []
        , input [ type_ "number", placeholder "21", onInput Age ] []
        , br [] []
        , input [ type_ "password", placeholder "Password", onInput Password ] []
        , br [] []
        , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
        , br [] []
        , checkbox Memer "Memer"
        , br [] []
        , fieldset []
            [ radio "Male" (Gender Male)
            , radio "Female" (Gender Female)
            , radio "Other" (Gender Other)
            ]
        , br [] []
        , button [ type_ "submit", value "Submit", onClick Validate ] [ text "submit" ]
        , viewValidation model
        ]


radio : String -> msg -> Html msg
radio value msg =
    label
        [ style [ ( "padding", "20px" ) ]
        ]
        [ input [ type_ "radio", name "gender", onClick msg ] []
        , text value
        ]


checkbox : Msg -> String -> Html Msg
checkbox msg name =
    label
        [ style [ ( "padding", "20px" ) ]
        ]
        [ input [ type_ "checkbox", onClick msg ] []
        , text name
        ]


viewValidation : Model -> Html msg
viewValidation model =
    let
        ( color, message, validate ) =
            if not model.validate then
                ( "", "", False )
            else if String.length model.password < 8 then
                ( "red", "Password too short!", True )
            else if model.password == model.passwordAgain then
                ( "green", "OK", True )
            else if any isDigit model.password then
                ( "red", "no digits!", True )
            else if any isLower model.password then
                ( "red", "no lower case letters", True )
            else if any isUpper model.password then
                ( "red", "no upper case letters", True )
            else
                ( "red", "Passwords do not match!", True )
    in
        if validate == True then
            div [ style [ ( "color", color ) ] ] [ text message ]
        else
            div [] []
