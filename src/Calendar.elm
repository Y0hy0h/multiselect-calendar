module Calendar exposing (monthdays)

import Date exposing (Date)


monthdays : Int -> Date.Month -> List Date
monthdays year month =
    let
        start =
            Date.fromCalendarDate year month 1

        until =
            Date.add Date.Months 1 start
    in
    Date.range Date.Day 1 start until
