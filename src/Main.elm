module Main exposing (main)

import Browser
import Calendar
import Date exposing (Date)
import Html exposing (Html, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (classList)
import Html.Events exposing (onClick)
import List.Extra as List
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
            let
                newSelected =
                    List.remove oldDate model.selected
            in
            ( { model | selected = newSelected }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            text "Loading"

        Loaded datesModel ->
            viewDates datesModel
                |> Html.map DatesMsg


viewDates : DatesModel -> Html DatesMsg
viewDates model =
    viewCalendar model


viewCalendar : DatesModel -> Html DatesMsg
viewCalendar model =
    let
        calendar =
            Calendar.forMonth (Date.year model.today) (Date.month model.today)
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
                                let
                                    date =
                                        Calendar.dateFromCalendarDate day

                                    isToday =
                                        date == model.today

                                    isSelected =
                                        List.member date model.selected
                                in
                                viewCalendarDay { today = isToday, selected = isSelected } day
                            )
                            week
                        )
                )
                calendar
            )
        ]


viewCalendarDay : { today : Bool, selected : Bool } -> Calendar.CalendarDate -> Html DatesMsg
viewCalendarDay is day =
    let
        ( dayClass, dayDate ) =
            case day of
                Calendar.Previous date ->
                    ( "previous", date )

                Calendar.Current date ->
                    ( "current", date )

                Calendar.Next date ->
                    ( "next", date )

        action =
            if is.selected then
                Remove dayDate

            else
                Add dayDate
    in
    td
        [ classList
            [ ( dayClass, True )
            , ( "today", is.today )
            , ( "selected", is.selected )
            ]
        , onClick action
        ]
        [ text (String.fromInt <| Date.day dayDate)
        ]
