module Tmp exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra as LE


part1 : String -> List Char
part1 =
    String.toList
        >> List.filter Char.isDigit


part2 : List Char -> String -> List Char
part2 init line =
    if not (String.isEmpty line) then
        part2
            (numbers
                |> List.foldl
                    (\( word, char ) previous ->
                        if
                            String.startsWith word line
                                || String.startsWith (String.fromChar char) line
                        then
                            previous ++ [ char ]

                        else
                            previous
                    )
                    init
            )
            (String.dropLeft 1 line)

    else
        init


sumFirstAndLastDigit : List Char -> Int
sumFirstAndLastDigit list =
    [ list |> List.head |> Maybe.map String.fromChar
    , list |> LE.last |> Maybe.map String.fromChar
    ]
        |> List.filterMap identity
        |> String.concat
        |> String.toInt
        |> Maybe.withDefault 0


numbers : List ( String, Char )
numbers =
    [ ( "one", '1' )
    , ( "two", '2' )
    , ( "three", '3' )
    , ( "four", '4' )
    , ( "five", '5' )
    , ( "six", '6' )
    , ( "seven", '7' )
    , ( "eight", '8' )
    , ( "nine", '9' )
    ]


view : () -> Html ()
view _ =
    div [ style "margin" "1em" ]
        [ h1 [] [ text "Day 1" ]
        , h2 [] [ text "Part 1" ]
        , pre []
            [ sample1
                |> String.split "\n"
                |> List.map (part1 >> sumFirstAndLastDigit)
                |> List.sum
                |> String.fromInt
                |> text
            ]
        , h2 [] [ text "Part 2" ]
        , pre []
            [ sample2
                |> String.split "\n"
                |> List.map (part2 [] >> sumFirstAndLastDigit)
                |> List.sum
                |> String.fromInt
                |> text
            ]
        ]


sample1 : String
sample1 =
    """1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"""


sample2 : String
sample2 =
    """two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"""


main : Program () () ()
main =
    Browser.sandbox
        { init = ()
        , view = view
        , update = \_ _ -> ()
        }
