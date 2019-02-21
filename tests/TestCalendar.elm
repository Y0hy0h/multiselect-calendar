module TestCalendar exposing (suite)

import Calendar
import Date exposing (Date)
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
                        dates : List (List Date)
                        dates =
                            makeDates
                                [ ( ( 2019, Time.Jan, 21 ), ( 2019, Time.Jan, 27 ) )
                                , ( ( 2019, Time.Jan, 28 ), ( 2019, Time.Feb, 3 ) )
                                , ( ( 2019, Time.Feb, 4 ), ( 2019, Time.Feb, 10 ) )
                                , ( ( 2019, Time.Feb, 11 ), ( 2019, Time.Feb, 17 ) )
                                , ( ( 2019, Time.Feb, 18 ), ( 2019, Time.Feb, 24 ) )
                                , ( ( 2019, Time.Feb, 25 ), ( 2019, Time.Mar, 3 ) )
                                , ( ( 2019, Time.Mar, 4 ), ( 2019, Time.Mar, 10 ) )
                                ]
                    in
                    Calendar.forMonth 2019 Time.Feb
                        |> List.map (\week -> List.map Calendar.dateFromCalendarDate week)
                        |> Expect.equal dates
            , test "generates correct date range for March 2019" <|
                \_ ->
                    let
                        dates : List (List Date)
                        dates =
                            makeDates
                                [ ( ( 2019, Time.Feb, 18 ), ( 2019, Time.Feb, 24 ) )
                                , ( ( 2019, Time.Feb, 25 ), ( 2019, Time.Mar, 3 ) )
                                , ( ( 2019, Time.Mar, 4 ), ( 2019, Time.Mar, 10 ) )
                                , ( ( 2019, Time.Mar, 11 ), ( 2019, Time.Mar, 17 ) )
                                , ( ( 2019, Time.Mar, 18 ), ( 2019, Time.Mar, 24 ) )
                                , ( ( 2019, Time.Mar, 25 ), ( 2019, Time.Mar, 31 ) )
                                , ( ( 2019, Time.Apr, 1 ), ( 2019, Time.Apr, 7 ) )
                                ]
                    in
                    Calendar.forMonth 2019 Time.Mar
                        |> List.map (\week -> List.map Calendar.dateFromCalendarDate week)
                        |> Expect.equal dates
            ]
        ]


makeDates : List ( ( Int, Time.Month, Int ), ( Int, Time.Month, Int ) ) -> List (List Date)
makeDates =
    let
        dateFromTuple ( year, month, day ) =
            Date.fromCalendarDate year month day
    in
    List.map
        (\( start, until ) ->
            Date.range Date.Day
                1
                (dateFromTuple start)
                (dateFromTuple until |> Date.add Date.Days 1)
        )
