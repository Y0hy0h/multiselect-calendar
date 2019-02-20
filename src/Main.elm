module Main exposing (main)

import Browser
import Calendar
import Date exposing (Date)
import Html exposing (Html, table, tbody, td, text, th, thead, tr)
import Task
import Time


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Model
    = Loading
    | Loaded DatesModel


type alias DatesModel =
    { today : Date
    , selected : List Date
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( Loading, Task.perform SetToday Date.today )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = SetToday Date
    | DatesMsg DatesMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetToday todayDate ->
            ( Loaded { today = todayDate, selected = [] }, Cmd.none )

        DatesMsg datesMsg ->
            case model of
                Loaded datesModel ->
                    updateDates datesMsg datesModel
                        |> Tuple.mapFirst Loaded

                _ ->
                    ( model, Cmd.none )


type DatesMsg
    = Add Date
    | Remove Date


updateDates : DatesMsg -> DatesModel -> ( DatesModel, Cmd Msg )
updateDates msg model =
    case msg of
        Add newDate ->
            ( { model | selected = model.selected ++ [ newDate ] }, Cmd.none )

        Remove oldDate ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            text "Loading"

        Loaded datesModel ->
            viewDates datesModel


viewDates : DatesModel -> Html Msg
viewDates model =
    let
        calendar =
            Calendar.forMonth (Date.year model.today) Time.Mar

        {- (Date.month model.today) -}
    in
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Mo" ]
                , th [] [ text "Di" ]
                , th [] [ text "Mi" ]
                , th [] [ text "Do" ]
                , th [] [ text "Fr" ]
                , th [] [ text "Sa" ]
                , th [] [ text "So" ]
                ]
            ]
        , tbody []
            (List.map
                (\week ->
                    tr []
                        (List.map
                            (\day ->
                                td [] [ text (String.fromInt <| Date.day day) ]
                            )
                            week
                        )
                )
                calendar
            )
        ]
