module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- MODEL


type alias Model =
    { votes : List String
    , selectedChoice : String
    , newChoice : String
    , choiceError : Maybe String
    , pollChoices : List Choice
    , showResults : Bool
    }


type alias Choice =
    { choice : String, value : String, votes : Int }


init : Model
init =
    { votes = []
    , selectedChoice = ""
    , newChoice = ""
    , choiceError = Nothing
    , pollChoices = [ Choice "Cake" "cake" 0, Choice "Eclair" "eclair" 0, Choice "Bread Pudding" "bread-pudding" 0, Choice "Wareva" "wareva" 0 ]
    , showResults = False
    }



-- UPDATE --


type Msg
    = Vote
    | Select String
    | UpdateNewChoice String
    | AddChoice
    | ToggleResults


update : Msg -> Model -> Model
update msg model =
    case msg of
        Vote ->
            let
                selectedChoice =
                    model.selectedChoice

                updatedChoice =
                    List.map
                        (\pChoice ->
                            if pChoice.choice == selectedChoice then
                                { pChoice | votes = pChoice.votes + 1 }

                            else
                                pChoice
                        )
                        model.pollChoices
            in
            { model | pollChoices = updatedChoice, showResults = True }

        Select option ->
            { model | selectedChoice = option }

        UpdateNewChoice choice ->
            { model | newChoice = choice, choiceError = Nothing }

        AddChoice ->
            addChoice model

        ToggleResults ->
            { model | showResults = not model.showResults }


addChoice : Model -> Model
addChoice model =
    let
        trimmedChoice =
            String.trim model.newChoice

        normalizedChoice =
            String.toLower trimmedChoice

        choiceExists =
            model.pollChoices
                |> List.any (\pollChoice -> String.toLower pollChoice.choice == normalizedChoice)
    in
    if trimmedChoice == "" then
        { model | choiceError = Just "Enter a choice before adding it." }

    else if choiceExists then
        { model | choiceError = Just "That choice is already in the poll." }

    else
        { model
            | pollChoices = model.pollChoices ++ [ Choice trimmedChoice (choiceValue trimmedChoice) 0 ]
            , newChoice = ""
            , choiceError = Nothing
        }


choiceValue : String -> String
choiceValue choice =
    choice
        |> String.toLower
        |> String.words
        |> String.join "-"



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "app-shell" ]
        [ main_ [ class "container poll-container" ]
            [ section [ class "poll-card shadow-sm" ]
                [ div [ class "poll-header" ]
                    [ p [ class "poll-kicker" ] [ text "Dessert Poll" ]
                    , h1 [ class "display-6 mb-2" ] [ text "Elm Opinion Poll" ]
                    , p [ class "text-secondary mb-0" ] [ text "Pick your favorite dessert, add a new option, and see how the vote is shaping up." ]
                    ]
                , pollForm model
                , customChoiceForm model
                , pollFooter model
                ]
            ]
        ]


pollForm : Model -> Html Msg
pollForm model =
    Html.form [ class "poll-form", onSubmit Vote ]
        [ div [ class "list-group choice-list" ] (List.map viewRadioButton model.pollChoices)
        , button
            [ type_ "submit"
            , class "btn btn-primary btn-lg w-100 mt-4"
            , disabled (model.selectedChoice == "")
            ]
            [ text "Vote" ]
        ]


customChoiceForm : Model -> Html Msg
customChoiceForm model =
    Html.form [ class "custom-choice-form", onSubmit AddChoice ]
        [ label [ class "form-label fw-semibold", for "new-choice" ] [ text "Add a choice" ]
        , div [ class "input-group" ]
            [ input
                [ id "new-choice"
                , class "form-control"
                , type_ "text"
                , placeholder "e.g. Ice cream"
                , value model.newChoice
                , onInput UpdateNewChoice
                ]
                []
            , button
                [ type_ "submit"
                , class "btn btn-success"
                , disabled (String.trim model.newChoice == "")
                ]
                [ text "Add" ]
            ]
        , case model.choiceError of
            Just errorMessage ->
                div [ class "form-text text-danger" ] [ text errorMessage ]

            Nothing ->
                div [ class "form-text" ] [ text "New choices start with zero votes." ]
        ]


pollFooter : Model -> Html Msg
pollFooter model =
    footer [ class "poll-footer" ]
        [ button [ type_ "button", class "btn btn-outline-secondary w-100", onClick ToggleResults ]
            [ text
                (if model.showResults then
                    "Hide Results"

                 else
                    "View Results"
                )
            ]
        , if model.showResults then
            pollResults model

          else
            text ""
        ]


pollResults : Model -> Html Msg
pollResults model =
    let
        totals =
            List.sum <| List.map (\p -> p.votes) model.pollChoices
    in
    div [ class "results-panel" ]
        [ pollTotal totals
        , choiceList totals model.pollChoices
        ]


pollTotal : Int -> Html Msg
pollTotal totals =
    div [ class "results-total" ]
        [ span [] [ text "Total votes" ]
        , strong [] [ text (String.fromInt totals) ]
        ]


choiceList : Int -> List Choice -> Html Msg
choiceList totals choices =
    choices
        |> List.map (viewChoice totals)
        |> ul [ class "list-unstyled mb-0" ]


viewChoice : Int -> Choice -> Html Msg
viewChoice totals pollChoice =
    let
        votePercentage =
            percentage totals pollChoice.votes
    in
    li [ class "result-row" ]
        [ div [ class "d-flex align-items-center justify-content-between gap-3" ]
            [ span [ class "fw-semibold" ] [ text pollChoice.choice ]
            , span [ class "text-secondary small" ]
                [ text (String.fromInt pollChoice.votes)
                , text " votes"
                , text " · "
                , text (String.fromInt votePercentage)
                , text "%"
                ]
            ]
        , div
            [ class "progress"
            , attribute "role" "progressbar"
            , attribute "aria-valuenow" (String.fromInt votePercentage)
            , attribute "aria-valuemin" "0"
            , attribute "aria-valuemax" "100"
            ]
            [ div
                [ class "progress-bar"
                , style "width" (String.fromInt votePercentage ++ "%")
                ]
                []
            ]
        ]


percentage : Int -> Int -> Int
percentage totals votes =
    if totals == 0 then
        0

    else
        round ((toFloat votes / toFloat totals) * 100)


viewRadioButton : Choice -> Html Msg
viewRadioButton pollChoice =
    label
        [ class "list-group-item choice-option" ]
        [ input
            [ class "form-check-input me-3"
            , type_ "radio"
            , name "dessert"
            , value pollChoice.value
            , onClick (Select pollChoice.choice)
            ]
            []
        , span [ class "choice-label" ] [ text pollChoice.choice ]
        ]


initElement : () -> ( Model, Cmd Msg )
initElement _ =
    ( init, Cmd.none )


updateElement : Msg -> Model -> ( Model, Cmd Msg )
updateElement msg model =
    ( update msg model, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = initElement
        , update = updateElement
        , view = view
        , subscriptions = \_ -> Sub.none
        }
