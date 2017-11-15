module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- MODEL


type alias Model =
    { choiceOne : Int
    , choiceTwo : Int
    , choiceThree : Int
    , totalVotes : Int
    , selectedOption : String
    }


type Msg
    = NoOp
    | Vote String
    | SubmitVote


initModel : Model
initModel =
    { choiceOne = 0
    , choiceTwo = 0
    , choiceThree = 0
    , totalVotes = 0
    , selectedOption = ""
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        SubmitVote ->
            if (String.isEmpty model.selectedOption) then
                model
            else
                submitvote model

        Vote dessert ->
            { model | selectedOption = dessert }


submitvote : Model -> Model
submitvote model =
    case model.selectedOption of
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



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text "Elm Opinion Poll" ]
        , pollForm model
        , pollTotals model
        ]


pollForm : Model -> Html Msg
pollForm model =
    Html.form [ onSubmit SubmitVote ]
        [ radio "Eclair" (Vote "Eclair")
        , radio "Cake" (Vote "Cake")
        , radio "Wareva!!" (Vote "Wareva")
        , button [ type_ "submit" ] [ text "Vote" ]
        ]


pollTotals : Model -> Html Msg
pollTotals model =
    footer []
        [ div [] [ text "Total:" ]
        , div [] [ text (toString model.totalVotes) ]
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
