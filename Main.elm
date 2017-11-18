module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- MODEL


type alias Model =
    { votes : List String
    , selectedChoice : String
    , pollChoices : List Choice
    }


type alias Choice =
    { choice : String, value : String, votes : Int }


init : Model
init =
    let
        model =
            Model []
                ""
                [ (Choice "Cake" "cake" 0), (Choice "Eclair" "eclair" 0), (Choice "Bread Pudding" "bread-pudding" 0), (Choice "Wareva" "wareva" 0) ]
    in
        model



-- UPDATE --


type Msg
    = Vote
    | Select String


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
                { model | pollChoices = updatedChoice }

        Select option ->
            { model | selectedChoice = option }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text "Elm Opinion Poll" ]
        , pollForm model
        , pollFooter model
        ]


pollForm : Model -> Html Msg
pollForm model =
    Html.form [ onSubmit Vote ]
        [ div [] (List.map viewRadioButton <| List.map .choice model.pollChoices)
        , button [ type_ "submit" ] [ text "Vote" ]
        ]


pollFooter : Model -> Html Msg
pollFooter model =
    footer []
        [ pollTotal model
        , choiceList model
        , div [] [ text (toString model) ]
        ]


pollTotal : Model -> Html Msg
pollTotal model =
    let
        totals =
            List.sum <| List.map (\p -> p.votes) model.pollChoices
    in
        div []
            [ text "Totals: ", text (toString totals) ]


choiceList : Model -> Html Msg
choiceList model =
    model.pollChoices
        |> List.map choice
        |> ul []


choice : Choice -> Html Msg
choice choice =
    li []
        [ div [] [ text choice.choice, text ":", text (toString choice.votes) ]
        ]


viewRadioButton : String -> Html Msg
viewRadioButton msg =
    label
        [ style [ ( "padding", "5px" ) ] ]
        [ input [ type_ "radio", name "dessert", onClick (Select msg) ] []
        , text msg
        ]


main =
    Html.beginnerProgram { model = init, view = view, update = update }
