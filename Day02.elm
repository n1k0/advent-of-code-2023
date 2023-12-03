module Tmp exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra as LE


type alias GameStats =
    { red : Int, green : Int, blue : Int }


part1 : String -> Int
part1 =
    parseGames
        >> List.map (Tuple.mapSecond (isPossibleGame { red = 12, green = 13, blue = 14 }))
        >> List.filter Tuple.second
        >> List.map Tuple.first
        >> List.sum


part2 : String -> Int
part2 =
    parseGames
        >> List.map (\( _, { red, green, blue } ) -> red * green * blue)
        >> List.sum


parseGames : String -> List ( Int, GameStats )
parseGames =
    String.split "\n"
        >> List.filterMap
            (String.split ":"
                >> (\lineParts ->
                        case lineParts of
                            [ title, sets ] ->
                                Just
                                    ( extractNumber title
                                    , sets
                                        |> String.trim
                                        |> String.split "; "
                                        |> List.map
                                            (String.split ", "
                                                >> List.map parseBalls
                                                >> sumBalls
                                            )
                                        |> sumMaxShown
                                    )

                            _ ->
                                Nothing
                   )
            )


isPossibleGame : GameStats -> GameStats -> Bool
isPossibleGame limits { red, green, blue } =
    red <= limits.red && green <= limits.green && blue <= limits.blue


parseBalls : String -> ( Int, String )
parseBalls text =
    ( extractNumber text
    , text |> String.split " " |> LE.last |> Maybe.withDefault ""
    )


sumMaxShown : List GameStats -> GameStats
sumMaxShown =
    List.foldl
        (\{ red, green, blue } acc ->
            { acc
                | red = pickMax red acc.red
                , green = pickMax green acc.green
                , blue = pickMax blue acc.blue
            }
        )
        { red = 0, green = 0, blue = 0 }


sumBalls : List ( Int, String ) -> GameStats
sumBalls balls =
    balls
        |> List.foldl
            (\( n, color ) acc ->
                if color == "red" then
                    { acc | red = acc.red + n }

                else if color == "green" then
                    { acc | green = acc.green + n }

                else
                    { acc | blue = acc.blue + n }
            )
            { red = 0, green = 0, blue = 0 }


extractNumber : String -> Int
extractNumber =
    String.toList
        >> List.filter Char.isDigit
        >> String.fromList
        >> String.toInt
        >> Maybe.withDefault 0


pickMax : number -> number -> number
pickMax a b =
    if a > b then
        a

    else
        b


view : () -> Html ()
view _ =
    div [ style "margin" "1em" ]
        [ h1 [] [ text "Day 2" ]
        , h2 [] [ text "Part 1" ]
        , pre [] [ text <| String.fromInt <| part1 sample ]
        , h2 [] [ text "Part 2" ]
        , pre [] [ text <| String.fromInt <| part2 sample ]
        ]


sample : String
sample =
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""


main : Program () () ()
main =
    Browser.sandbox
        { init = ()
        , view = view
        , update = \_ _ -> ()
        }
