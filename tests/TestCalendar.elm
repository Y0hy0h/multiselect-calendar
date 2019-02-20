module TestCalendar exposing (suite)

import Calendar
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Time


suite : Test
suite =
    describe "Calendar"
        [ describe "monthdays"
            [ test "handles regular month" <|
                \_ ->
                    Calendar.monthdays 2019 Time.Jan
                        |> List.length
                        |> Expect.equal 31
            , test "handles non-leap-year February" <|
                \_ ->
                    Calendar.monthdays 2019 Time.Feb
                        |> List.length
                        |> Expect.equal 28
            , test "handles leap-year February" <|
                \_ ->
                    Calendar.monthdays 2020 Time.Feb
                        |> List.length
                        |> Expect.equal 29
            ]
        ]
