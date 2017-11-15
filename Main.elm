module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- MODEL


type alias Model =
    { choiceOne : Int
    , choiceTwo : Int
    , choiceThree : Int
    , totalVotes : Int
    , optionOne : String
    }


type Dessert
    = Eclair
    | Cake
    | Wareva


type Msg
    = NoOp
    | Vote Dessert
    | SubmitVote


initModel : Model
initModel =
    { choiceOne = 0
    , choiceTwo = 0
    , choiceThree = 0
    , totalVotes = 0
    , optionOne = ""
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        SubmitVote ->
            let
                selectedOption =
                    model.optionOne
            in
                case selectedOption of
                    "Eclair" ->
                        { model
                            | choiceOne = model.choiceOne + 1
                            , totalVotes = model.totalVotes + 1
                        }

                    "Cake" ->
                        { model
                            | choiceTwo = model.choiceTwo + 1
                            , totalVotes = model.totalVotes + 1
                        }

                    "Wareva" ->
                        { model
                            | choiceThree = model.choiceThree + 1
                            , totalVotes = model.totalVotes + 1
                        }

                    _ ->
                        model

        Vote dessert ->
            let
                dessertName =
                    toString dessert
            in
                { model | optionOne = dessertName }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "heading" ]
            [ h2 []
                [ text "Elm Opinion Poll"
                ]
            , p []
                [ text "What is your favorite dessert?"
                ]
            ]
        , div [ class "pollForm" ]
            [ radio "Eclair" (Vote Eclair)
            , radio "Cake" (Vote Cake)
            , radio "Wareva!!" (Vote Wareva)
            , button [ type_ "button", onClick SubmitVote ] [ text "Vote" ]
            ]
        , div [ class "totalVotes" ]
            [ h3 []
                [ text "Total Votes"
                ]
            ]
        ]


radio : String -> msg -> Html msg
radio value msg =
    label
        [ style [ ( "padding", "20px" ) ] ]
        [ input [ type_ "radio", name "dessert", onClick msg ] []
        , text value
        ]


main =
    Html.beginnerProgram { model = initModel, view = view, update = update }
