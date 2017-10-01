module Main exposing (..)

import Html exposing (Html, div, fieldset, input, label, text, button)
import Html.Attributes exposing (name, style, type_)
import Html.Events exposing (onClick)


-- MODEL


type alias Model =
    { question : String
    , choiceOne : Int
    , choiceTwo : Int
    }


type Party
    = Jubilee
    | NASA
    | Wareva


type Msg
    = NoOp
    | Vote Party


initModel : Model
initModel =
    { question = ""
    , choiceOne = 0
    , choiceTwo = 0
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Vote party ->
            let
                partyName =
                    toString party
            in
                case partyName of
                    "Jubilee" ->
                        { model | choiceOne = model.choiceOne + 1 }

                    "NASA" ->
                        { model | choiceTwo = model.choiceTwo + 1 }

                    _ ->
                        model



-- VIEW


view : Model -> Html Msg
view model =
    Html.form []
        [ fieldset []
            [ radio "Jubilee" (Vote Jubilee)
            , radio "NASA" (Vote NASA)
            , radio "Wareva!!" (Vote Wareva)
            , button [ type_ "submit" ] [ text "Vote" ]
            ]
        ]


radio : String -> msg -> Html msg
radio value msg =
    label
        [ style [ ( "padding", "20px" ) ] ]
        [ input [ type_ "radio", name "party", onClick msg ] []
        , text value
        ]


main =
    Html.beginnerProgram { model = initModel, view = view, update = update }
