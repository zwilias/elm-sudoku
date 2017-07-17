port module App exposing (..)

import Array
import List.Extra as List
import Json.Encode exposing (Value, string)
import Main exposing (..)
import Task
import Time


update : Maybe Sudoku -> () -> ( (), Cmd msg )
update solution _ =
    () ! [ emitSolution solution ]


solve : Sudoku -> Cmd (Maybe Sudoku)
solve board =
    Time.now
        |> Task.map
            (\_ ->
                resolveSudoku board
            )
        |> Task.perform identity


main : Program Never () (Maybe Sudoku)
main =
    Platform.program
        { init = () ! [ solve boardToSolve, emitBoard boardToSolve ]
        , update = update
        , subscriptions = always Sub.none
        }


boardToSolve : Sudoku
boardToSolve =
    let
        cells =
            [ [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
            , [ Just 8, Nothing, Nothing, Just 6, Nothing, Nothing, Nothing, Nothing, Nothing ]
            , [ Nothing, Nothing, Nothing, Nothing, Just 3, Just 2, Just 9, Just 8, Just 6 ]
            , [ Nothing, Nothing, Just 7, Nothing, Just 8, Nothing, Just 2, Nothing, Just 1 ]
            , [ Nothing, Nothing, Nothing, Nothing, Just 2, Nothing, Just 3, Just 4, Nothing ]
            , [ Just 9, Nothing, Nothing, Just 7, Nothing, Nothing, Just 6, Nothing, Nothing ]
            , [ Just 1, Nothing, Nothing, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing ]
            , [ Just 5, Just 9, Nothing, Nothing, Just 7, Nothing, Nothing, Nothing, Nothing ]
            , [ Nothing, Just 8, Nothing, Just 1, Just 5, Nothing, Nothing, Nothing, Nothing ]
            ]
                |> List.concat
                |> List.map
                    (\n ->
                        case n of
                            Nothing ->
                                emptyCell

                            Just value ->
                                fixedCell value
                    )
                |> Array.fromList
    in
        { cells = cells, current_position = 0 }


emitBoard : Sudoku -> Cmd msg
emitBoard board =
    "Attempting to solve:\n\n"
        ++ stringify board
        |> Json.Encode.string
        |> emit


emitSolution : Maybe Sudoku -> Cmd msg
emitSolution solution =
    case solution of
        Just board ->
            "\n\nSolved!\n\n"
                ++ stringify board
                |> Json.Encode.string
                |> emit

        Nothing ->
            Json.Encode.string "\n\nFailed :()" |> emit


stringifyRow : List String -> String
stringifyRow row =
    List.groupsOf 3 row
        |> List.map (String.join " ")
        |> String.join " | "


stringify : Sudoku -> String
stringify sudoku =
    Array.toList sudoku.cells
        |> List.map (.value)
        |> List.map
            (\n ->
                if n == 0 then
                    " "
                else
                    toString n
            )
        |> List.groupsOf 9
        |> List.map stringifyRow
        |> List.groupsOf 3
        |> List.map (String.join "\n")
        |> List.intersperse "------+-------+------"
        |> String.join "\n"


port emit : Value -> Cmd msg
