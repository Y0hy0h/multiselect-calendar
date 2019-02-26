port module Main exposing (main)

import Browser
import Calendar
import Date exposing (Date)
import Html exposing (Html, button, div, form, input, li, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, classList, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Encode as Encode
import List.Extra as List
import Parser exposing ((|.), (|=))
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
    , dateInput : String
    }


init : List String -> ( Model, Cmd Msg )
init rawDates =
    let
        dates =
            List.filterMap parseDate rawDates
    in
    ( Loading, Task.perform (SetToday dates) Date.today )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = SetToday (List Date) Date
    | DatesMsg DatesMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetToday prefilledDates todayDate ->
            ( Loaded
                { today = todayDate
                , month = Date.floor Date.Month todayDate
                , selected = prefilledDates
                , dateInput = ""
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
    | GoToCurrentMonth
    | DateInputChanged String
    | DateInputSubmitted


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

        GoToCurrentMonth ->
            ( { model | month = Date.floor Date.Month model.today }, Cmd.none )

        DateInputChanged newDate ->
            ( { model | dateInput = newDate }, Cmd.none )

        DateInputSubmitted ->
            case parseDate model.dateInput of
                Just date ->
                    let
                        ( newModel, newCmd ) =
                            updateSelection model (Add date)
                    in
                    ( { newModel | dateInput = "" }, newCmd )

                Nothing ->
                    ( model, Cmd.none )


parseDate : String -> Maybe Date
parseDate rawDate =
    let
        paddedInt =
            Parser.succeed identity
                |. Parser.chompWhile (\c -> c == '0')
                |= Parser.int

        month =
            Parser.succeed Date.numberToMonth
                |= paddedInt

        date =
            Parser.succeed Date.fromCalendarDate
                |= paddedInt
                |. Parser.symbol "."
                |= month
                |. Parser.symbol "."
                |= paddedInt
    in
    case Date.fromIsoString rawDate of
        Ok d ->
            Just d

        Err _ ->
            Parser.run date rawDate |> Result.toMaybe


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
            [ div []
                [ button
                    [ class "today"
                    , onClick GoToCurrentMonth
                    ]
                    [ text "This month" ]
                , text (Date.format "MMMM y" model.month)
                ]
            , button
                [ class "previous-month"
                , onClick (MonthActionMsg PreviousMonth)
                ]
                [ text "^" ]
            , viewCalendar model
            , button
                [ class "next-month"
                , onClick (MonthActionMsg NextMonth)
                ]
                [ text "v" ]
            ]
        , viewDatesList model.dateInput model.selected
        ]


button : List (Html.Attribute DatesMsg) -> List (Html DatesMsg) -> Html DatesMsg
button attributes contents =
    Html.button (attributes ++ [ type_ "button" ]) contents


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


viewDatesList : String -> List Date -> Html DatesMsg
viewDatesList currentInput dates =
    let
        viewDateListItem date =
            li []
                [ text (Date.format "dd.MM.yyyy" date)
                , button [ onClick (CombinedActionMsg (Remove date) Nothing) ] [ text "X" ]
                ]
    in
    div []
        [ ul [] (List.map viewDateListItem dates)
        , form [ onSubmit DateInputSubmitted ]
            [ input [ type_ "date", onInput DateInputChanged, value currentInput ] []
            , Html.button [ type_ "submit" ] [ text "+" ]
            ]
        ]
