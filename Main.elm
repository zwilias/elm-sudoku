module Main exposing (..)

import Array exposing (Array)
import List.Extra as List
import Set
import Html
import Html.Attributes exposing (..)


main : Html.Html msg
main =
    let
        sudoku =
            case resolveSudoku sudokuGrid of
                Nothing ->
                    initSudoku

                Just s ->
                    s
    in
        Html.div []
            [ Html.h1 [] [ Html.text "Sudoku - Power thigh" ]
            , Html.h2 [] [ Html.text "Attempting to solve" ]
            , Html.div [ class "sudoku" ] <| stringify sudokuGrid
            , Html.h2 [] [ Html.text "Solution" ]
            , Html.div [ class "sudoku" ] <| stringify sudoku
            ]


type alias Sudoku =
    { cells : Array Cell
    , current_position : Int
    }


type alias Cell =
    { value : Int
    , editable : Bool
    }



-- Init Sudoku


emptyCell : Cell
emptyCell =
    { value = 0, editable = True }


fixedCell : Int -> Cell
fixedCell value =
    { value = value, editable = False }


initSudoku : Sudoku
initSudoku =
    { cells = Array.repeat 81 emptyCell
    , current_position = 0
    }


sudokuGrid : Sudoku
sudokuGrid =
    { cells =
        Array.fromList
            [ -- First line
              -- 1
              emptyCell
            , emptyCell
            , fixedCell 3
              -- 2
            , fixedCell 9
            , emptyCell
            , emptyCell
              -- 3
            , fixedCell 7
            , fixedCell 6
            , emptyCell
              -- 1
            , emptyCell
            , fixedCell 4
            , emptyCell
              -- 2
            , emptyCell
            , emptyCell
            , fixedCell 6
              -- 3
            , emptyCell
            , emptyCell
            , fixedCell 9
              -- 1
            , fixedCell 6
            , emptyCell
            , fixedCell 7
              -- 2
            , emptyCell
            , fixedCell 1
            , emptyCell
              -- 3
            , emptyCell
            , emptyCell
            , fixedCell 4
              -- Second Line
              -- 4
            , fixedCell 2
            , emptyCell
            , emptyCell
              -- 5
            , fixedCell 6
            , fixedCell 7
            , emptyCell
              -- 6
            , emptyCell
            , fixedCell 9
            , emptyCell
              -- 4
            , emptyCell
            , emptyCell
            , fixedCell 4
              -- 5
            , fixedCell 3
            , emptyCell
            , fixedCell 5
              -- 6
            , fixedCell 6
            , emptyCell
            , emptyCell
              -- 4
            , emptyCell
            , fixedCell 1
            , emptyCell
              -- 5
            , emptyCell
            , fixedCell 4
            , fixedCell 9
              -- 6
            , emptyCell
            , emptyCell
            , fixedCell 7
              -- Last line
              -- 7
            , fixedCell 7
            , emptyCell
            , emptyCell
              -- 8
            , emptyCell
            , fixedCell 9
            , emptyCell
              -- 9
            , fixedCell 2
            , emptyCell
            , fixedCell 1
              -- 7
            , fixedCell 3
            , emptyCell
            , emptyCell
              -- 8
            , fixedCell 2
            , emptyCell
            , emptyCell
              -- 9
            , emptyCell
            , fixedCell 4
            , emptyCell
              -- 7
            , emptyCell
            , fixedCell 2
            , fixedCell 9
              -- 8
            , emptyCell
            , emptyCell
            , fixedCell 8
              -- 9
            , fixedCell 5
            , emptyCell
            , emptyCell
            ]
    , current_position = 0
    }



-- Move within the Sudoku grid


get_current_cell : Sudoku -> Cell
get_current_cell sudoku =
    let
        current_cell =
            Array.get sudoku.current_position sudoku.cells
                |> Maybe.withDefault emptyCell
    in
        current_cell


next_cell : Sudoku -> Maybe Sudoku
next_cell sudoku =
    if sudoku.current_position == 81 then
        Nothing
    else
        let
            next_position =
                (sudoku.current_position + 1)
        in
            Just { sudoku | current_position = next_position }


previous_cell : Sudoku -> Maybe Sudoku
previous_cell sudoku =
    if sudoku.current_position == 0 then
        Nothing
    else
        Just { sudoku | current_position = sudoku.current_position - 1 }


next_editable_cell : Sudoku -> Maybe Sudoku
next_editable_cell sudoku =
    let
        next_sudoku =
            next_cell sudoku
    in
        case next_sudoku of
            Nothing ->
                Nothing

            Just current_sudoku ->
                let
                    current_cell =
                        get_current_cell current_sudoku
                in
                    if current_cell.editable then
                        Just current_sudoku
                    else
                        next_editable_cell current_sudoku


previous_editable_cell : Sudoku -> Maybe Sudoku
previous_editable_cell sudoku =
    let
        next_sudoku =
            previous_cell sudoku
    in
        case next_sudoku of
            Nothing ->
                Nothing

            Just current_sudoku ->
                let
                    current_cell =
                        get_current_cell current_sudoku
                in
                    if current_cell.editable then
                        Just current_sudoku
                    else
                        previous_editable_cell current_sudoku



-- Sudoku


sudokuValid : Sudoku -> Bool
sudokuValid sudoku =
    linesValid sudoku && columnsValid sudoku && squaresValid sudoku


resolveSudoku : Sudoku -> Maybe Sudoku
resolveSudoku sudoku =
    if sudokuValid sudoku then
        if sudoku.current_position == 81 then
            -- Sudoku resolved
            Just sudoku
        else
            increment_sudoku sudoku
    else
        -- Sudoku invalid
        Nothing


brute_force : Maybe Sudoku -> Maybe Sudoku
brute_force sudoku =
    case sudoku of
        Nothing ->
            Nothing

        Just current_sudoku ->
            resolveSudoku current_sudoku


set_current_value : Int -> Sudoku -> Sudoku
set_current_value value sudoku =
    let
        current_cell =
            get_current_cell sudoku

        next_cell =
            { current_cell | value = value }
    in
        { sudoku
            | cells =
                Array.set sudoku.current_position next_cell sudoku.cells
        }


increment_sudoku : Sudoku -> Maybe Sudoku
increment_sudoku sudoku =
    -- Get current cell
    --   If editable
    --     Increment until valid
    --   If not editable
    --     Go to next_editable_cell
    --       If doesn't exist no solution
    --       If exists
    --          Increment it until next valid solution
    let
        current_cell =
            get_current_cell sudoku
    in
        if current_cell.editable then
            if current_cell.value == 9 then
                set_current_value 0 sudoku
                    |> previous_editable_cell
                    |> brute_force
            else
                let
                    next_sudoku =
                        set_current_value (current_cell.value + 1) sudoku
                in
                    if sudokuValid next_sudoku then
                        next_editable_cell next_sudoku
                            |> brute_force
                    else
                        increment_sudoku next_sudoku
        else
            next_editable_cell sudoku
                |> brute_force



-- Zone valid


listValid : Array Cell -> Bool
listValid list =
    let
        values =
            Array.map (\n -> n.value) list

        zero =
            Array.filter (\n -> n == 0) values

        numbers =
            Array.filter (\n -> n > 0 && n < 10) values

        unique_numbers =
            Set.fromList <| Array.toList numbers
    in
        (List.length <| Set.toList unique_numbers) == Array.length numbers



-- Are lines valid


linesValid : Sudoku -> Bool
linesValid sudoku =
    let
        lines =
            [ Array.slice 0 9 sudoku.cells
            , Array.slice 9 18 sudoku.cells
            , Array.slice 18 27 sudoku.cells
            , Array.slice 27 36 sudoku.cells
            , Array.slice 36 45 sudoku.cells
            , Array.slice 45 54 sudoku.cells
            , Array.slice 54 63 sudoku.cells
            , Array.slice 63 72 sudoku.cells
            , Array.slice 72 81 sudoku.cells
            ]
    in
        List.foldl (&&) True <| List.map listValid lines



-- Are columns valid


getColumn : Int -> Sudoku -> Array Cell
getColumn id sudoku =
    Array.fromList
        [ Maybe.withDefault emptyCell <| Array.get id sudoku.cells
        , Maybe.withDefault emptyCell <| Array.get (id + 9) sudoku.cells
        , Maybe.withDefault emptyCell <| Array.get (id + 9 * 2) sudoku.cells
        , Maybe.withDefault emptyCell <| Array.get (id + 9 * 3) sudoku.cells
        , Maybe.withDefault emptyCell <| Array.get (id + 9 * 4) sudoku.cells
        , Maybe.withDefault emptyCell <| Array.get (id + 9 * 5) sudoku.cells
        , Maybe.withDefault emptyCell <| Array.get (id + 9 * 6) sudoku.cells
        , Maybe.withDefault emptyCell <| Array.get (id + 9 * 7) sudoku.cells
        , Maybe.withDefault emptyCell <| Array.get (id + 9 * 8) sudoku.cells
        ]


columnsValid : Sudoku -> Bool
columnsValid sudoku =
    let
        columns =
            [ getColumn 0 sudoku
            , getColumn 1 sudoku
            , getColumn 2 sudoku
            , getColumn 3 sudoku
            , getColumn 4 sudoku
            , getColumn 5 sudoku
            , getColumn 6 sudoku
            , getColumn 7 sudoku
            , getColumn 8 sudoku
            ]
    in
        List.foldl (&&) True <| List.map listValid columns



-- Are square valid


getSquare : Int -> Sudoku -> Array Cell
getSquare id sudoku =
    Array.fromList
        [ Maybe.withDefault emptyCell <| Array.get id sudoku.cells
        , Maybe.withDefault emptyCell <| Array.get (id + 1) sudoku.cells
        , Maybe.withDefault emptyCell <| Array.get (id + 2) sudoku.cells
        , Maybe.withDefault emptyCell <| Array.get (id + 9) sudoku.cells
        , Maybe.withDefault emptyCell <| Array.get (id + 10) sudoku.cells
        , Maybe.withDefault emptyCell <| Array.get (id + 11) sudoku.cells
        , Maybe.withDefault emptyCell <| Array.get (id + 18) sudoku.cells
        , Maybe.withDefault emptyCell <| Array.get (id + 19) sudoku.cells
        , Maybe.withDefault emptyCell <| Array.get (id + 20) sudoku.cells
        ]


squaresValid : Sudoku -> Bool
squaresValid sudoku =
    let
        squares =
            [ getSquare 0 sudoku
            , getSquare 3 sudoku
            , getSquare 6 sudoku
            , getSquare (3 * 9) sudoku
            , getSquare (3 * 9 + 3) sudoku
            , getSquare (3 * 9 + 6) sudoku
            , getSquare (9 * 9) sudoku
            , getSquare (9 * 9 + 3) sudoku
            , getSquare (9 * 9 + 6) sudoku
            ]
    in
        List.foldl (&&) True <| List.map listValid squares


stringifyRow : List String -> String
stringifyRow row =
    List.groupsOf 3 row
        |> List.map (String.join " ")
        |> String.join " | "


stringify : Sudoku -> List (Html.Html msg)
stringify sudoku =
    Array.toList sudoku.cells
        |> List.map (.value)
        |> List.map
            (\n ->
                if n == 0 then
                    "_"
                else
                    toString n
            )
        |> List.groupsOf 9
        |> List.map stringifyRow
        |> List.map (\n -> Html.div [] [ Html.text n ])
        |> List.groupsOf 3
        |> List.map (Html.div [])
        |> List.intersperse (Html.div [] [ Html.text "------+-------+------" ])
