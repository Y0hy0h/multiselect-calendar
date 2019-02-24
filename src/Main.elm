port module Main exposing (main)

import Browser
import Calendar
import Date exposing (Date)
import Html exposing (Html, button, div, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, classList, type_)
import Html.Events exposing (onClick)
import Json.Encode as Encode
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


sendSelected : List Date -> Cmd msg
sendSelected selected =
    List.map Date.toIsoString selected
        |> Encode.list Encode.string
        |> selectedPort


port selectedPort : Encode.Value -> Cmd msg



-- MODEL


type Model
    = Loading
    | Loaded DatesModel


type alias DatesModel =
    { today : Date
    , month : Date
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
            ( Loaded
                { today = todayDate
                , month = Date.floor Date.Month todayDate
                , selected = []
                }
            , Cmd.none
            )

        DatesMsg datesMsg ->
            case model of
                Loaded datesModel ->
                    updateDates datesMsg datesModel
                        |> Tuple.mapFirst Loaded

                _ ->
                    ( model, Cmd.none )


type DatesMsg
    = CombinedActionMsg DateAction (Maybe MonthAction)
    | MonthActionMsg MonthAction


type DateAction
    = Add Date
    | Remove Date


type MonthAction
    = PreviousMonth
    | NextMonth


updateDates : DatesMsg -> DatesModel -> ( DatesModel, Cmd Msg )
updateDates msg model =
    case msg of
        CombinedActionMsg dateAction maybeMonthAction ->
            let
                ( dateModel, dateCmd ) =
                    updateSelection model dateAction
            in
            ( updateMonth maybeMonthAction dateModel, dateCmd )

        MonthActionMsg monthAction ->
            ( updateMonth (Just monthAction) model, Cmd.none )


updateSelection : DatesModel -> DateAction -> ( DatesModel, Cmd Msg )
updateSelection model action =
    let
        newModel =
            case action of
                Add newDate ->
                    { model | selected = model.selected ++ [ newDate ] }

                Remove oldDate ->
                    let
                        newSelected =
                            List.remove oldDate model.selected
                    in
                    { model | selected = newSelected }

        sortedModel =
            { newModel | selected = List.sortBy Date.toIsoString newModel.selected }
    in
    ( sortedModel, sendSelected sortedModel.selected )


updateMonth : Maybe MonthAction -> DatesModel -> DatesModel
updateMonth maybeMonthAction model =
    let
        moveMonth step month =
            Date.add Date.Months step month
    in
    case maybeMonthAction of
        Just PreviousMonth ->
            { model | month = moveMonth -1 model.month }

        Just NextMonth ->
            { model | month = moveMonth 1 model.month }

        Nothing ->
            model



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
    div [ class "calendar" ]
        [ div [ class "container" ]
            [ text (Date.format "MMMM y" model.month)
            , button [ onClick (MonthActionMsg PreviousMonth), type_ "button" ] [ text "^" ]
            , viewCalendar model
            , button [ onClick (MonthActionMsg NextMonth), type_ "button" ] [ text "v" ]
            ]
        ]


viewCalendar : DatesModel -> Html DatesMsg
viewCalendar model =
    let
        calendar =
            Calendar.forMonth (Date.year model.month) (Date.month model.month)
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
        ( dayClass, dayDate, maybeMonthAction ) =
            case day of
                Calendar.Previous date ->
                    ( "previous", date, Just PreviousMonth )

                Calendar.Current date ->
                    ( "current", date, Nothing )

                Calendar.Next date ->
                    ( "next", date, Just NextMonth )

        dateAction =
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
        , onClick (CombinedActionMsg dateAction maybeMonthAction)
        ]
        [ text (String.fromInt <| Date.day dayDate)
        ]
