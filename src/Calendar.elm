module Calendar exposing (forMonth)

import Date exposing (Date)


type alias CalendarMonth =
    List (List Date)


forMonth : Int -> Date.Month -> CalendarMonth
forMonth year month =
    let
        days =
            calendarMonthDays year month
    in
    List.foldl
        (\day ( week, result ) ->
            if List.length week < 7 then
                ( week ++ [ day ], result )

            else
                ( [ day ], result ++ [ week ] )
        )
        ( [], [] )
        days
        |> (\( week, result ) -> result ++ [ week ])


calendarMonthDays : Int -> Date.Month -> List Date
calendarMonthDays year month =
    let
        beginningOfMonth =
            Date.fromCalendarDate year month 1

        start =
            firstOfSameWeek beginningOfMonth

        until =
            Date.add Date.Months 1 beginningOfMonth
                |> lastOfSameWeek
    in
    Date.range Date.Day 1 start until


firstOfSameWeek : Date -> Date
firstOfSameWeek date =
    Date.floor Date.Week date


lastOfSameWeek : Date -> Date
lastOfSameWeek date =
    Date.ceiling Date.Week date
