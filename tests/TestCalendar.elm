module TestCalendar exposing (suite)

import Calendar
import Date
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Time


suite : Test
suite =
    describe "Calendar"
        [ describe "forMonth"
            [ test "generates correct date range for February 2019" <|
                \_ ->
                    let
                        start =
                            Date.fromCalendarDate 2019 Time.Jan 28

                        until =
                            Date.fromCalendarDate 2019 Time.Mar 4

                        dates =
                            Date.range Date.Day 1 start until
                    in
                    Calendar.forMonth 2019 Time.Feb
                        |> Expect.equal dates
            , test "generates correct date range for March 2019" <|
                \_ ->
                    let
                        start =
                            Date.fromCalendarDate 2019 Time.Feb 25

                        until =
                            Date.fromCalendarDate 2019 Time.Apr 1

                        dates =
                            Date.range Date.Day 1 start until
                    in
                    Calendar.forMonth 2019 Time.Mar
                        |> Expect.equal dates
            ]
        ]
