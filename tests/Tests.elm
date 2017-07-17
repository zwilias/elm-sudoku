module Tests exposing (..)

import Array
import Expect
import Main exposing (..)
import Test exposing (..)


cells : Test
cells =
    describe "cells"
        [ describe "get_current_cell" [ validateGetCurrentCell ]
        , describe "next_cell" [ goToTheNextCell, nextCellStopAtTheEnd ]
        , describe "previous_cell" [ goToThePreviousCell, previousCellStopAtTheEnd ]
        , describe "next_editable_cell"
            [ goToTheNextEditableCell, nextEditableCellStopAtTheEnd ]
        , describe "previous_editable_cell"
            [ goToThePreviousEditableCell, previousEditableCellStopAtTheEnd ]
        ]



-- get_current_cell


validateGetCurrentCell : Test
validateGetCurrentCell =
    let
        current_cell =
            initSudoku
                |> set_current_value 8
                |> get_current_cell
    in
        test "Current cell" <|
            \_ ->
                Expect.equal current_cell { value = 8, editable = True }



-- next_cell


goToTheNextCell : Test
goToTheNextCell =
    let
        sudoku =
            initSudoku
                |> next_cell
    in
        test "Got to the next cell" <|
            \_ ->
                case sudoku of
                    Nothing ->
                        Expect.fail "Should go to the next cell"

                    Just s ->
                        Expect.equal 1 s.current_position


nextCellStopAtTheEnd : Test
nextCellStopAtTheEnd =
    let
        sudoku =
            { initSudoku | current_position = 80 }
                |> next_cell
    in
        test "Next cell stop at the end" <|
            \_ ->
                case sudoku of
                    Nothing ->
                        Expect.pass

                    Just s ->
                        Expect.fail "Should not increment"



-- previous_cell


goToThePreviousCell : Test
goToThePreviousCell =
    let
        sudoku =
            { initSudoku | current_position = 80 }
                |> previous_cell
    in
        test "Got to the previous cell" <|
            \_ ->
                case sudoku of
                    Nothing ->
                        Expect.fail "Should go to the previous cell"

                    Just s ->
                        Expect.equal 79 s.current_position


previousCellStopAtTheEnd : Test
previousCellStopAtTheEnd =
    let
        sudoku =
            initSudoku
                |> previous_cell
    in
        test "Previous cell stop at the end" <|
            \_ ->
                case sudoku of
                    Nothing ->
                        Expect.pass

                    Just s ->
                        Expect.fail "Should not go to the previous one"



-- next_editable_cell


goToTheNextEditableCell : Test
goToTheNextEditableCell =
    let
        sudoku =
            { cells =
                Array.fromList
                    [ emptyCell
                    , { emptyCell | editable = False }
                    , { emptyCell | editable = False }
                    , { emptyCell | editable = False }
                    , emptyCell
                    ]
            , current_position = 0
            }
                |> next_editable_cell
    in
        test "Got to the next editable cell" <|
            \_ ->
                case sudoku of
                    Nothing ->
                        Expect.fail "Should go to the next editable cell"

                    Just s ->
                        Expect.equal 4 s.current_position


nextEditableCellStopAtTheEnd : Test
nextEditableCellStopAtTheEnd =
    let
        sudoku =
            { cells =
                Array.set 79 { emptyCell | editable = False } initSudoku.cells
                    |> Array.set 80 { emptyCell | editable = False }
            , current_position = 78
            }
                |> next_editable_cell
    in
        test "Next editable cell stop at the end" <|
            \_ ->
                case sudoku of
                    Nothing ->
                        Expect.pass

                    Just s ->
                        let
                            _ =
                                Debug.log "Strange" s.current_position
                        in
                            Expect.fail "Should not find a next editable cell"



-- previous_editable_cell


goToThePreviousEditableCell : Test
goToThePreviousEditableCell =
    let
        sudoku =
            { cells =
                Array.fromList
                    [ emptyCell
                    , { emptyCell | editable = False }
                    , { emptyCell | editable = False }
                    , { emptyCell | editable = False }
                    , emptyCell
                    ]
            , current_position = 4
            }
                |> previous_editable_cell
    in
        test "Got to the previous editable cell" <|
            \_ ->
                case sudoku of
                    Nothing ->
                        Expect.fail "Should go to the previous editable cell"

                    Just s ->
                        Expect.equal 0 s.current_position


previousEditableCellStopAtTheEnd : Test
previousEditableCellStopAtTheEnd =
    let
        sudoku =
            { cells =
                Array.set 0 { emptyCell | editable = False } initSudoku.cells
                    |> Array.set 1 { emptyCell | editable = False }
            , current_position = 2
            }
                |> previous_editable_cell
    in
        test "Previous editable cell stop at the end" <|
            \_ ->
                case sudoku of
                    Nothing ->
                        Expect.pass

                    Just s ->
                        let
                            _ =
                                Debug.log "Strange" s.current_position
                        in
                            Expect.fail "Should not find a previous editable cell"



-- validate lines


validLines =
    [ ( [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ], True )
    , ( [ 1, 2, 0, 0, 0, 0, 0, 0, 0 ], True )
    , ( [ 1, 2, 3, 0, 0, 0, 0, 0, 0 ], True )
    , ( [ 1, 2, 3, 4, 0, 0, 0, 0, 0 ], True )
    , ( [ 1, 2, 3, 4, 5, 0, 0, 0, 0 ], True )
    , ( [ 1, 2, 3, 4, 5, 6, 0, 0, 0 ], True )
    , ( [ 1, 2, 3, 4, 5, 6, 7, 0, 0 ], True )
    , ( [ 1, 2, 3, 4, 5, 6, 7, 8, 0 ], True )
    , ( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], True )
    , ( [ 9, 9, 0, 0, 0, 0, 0, 0, 0 ], False )
    , ( [ 1, 1, 0, 0, 0, 0, 0, 0, 0 ], False )
    , ( [ 2, 2, 3, 0, 0, 0, 0, 0, 0 ], False )
    , ( [ 3, 2, 3, 4, 0, 0, 0, 0, 0 ], False )
    , ( [ 4, 2, 3, 4, 5, 0, 0, 0, 0 ], False )
    , ( [ 5, 2, 3, 4, 5, 6, 0, 0, 0 ], False )
    , ( [ 6, 2, 3, 4, 5, 6, 7, 0, 0 ], False )
    , ( [ 7, 2, 3, 4, 5, 6, 7, 8, 0 ], False )
    , ( [ 8, 2, 3, 4, 5, 6, 7, 8, 9 ], False )
    ]


validateLine : ( List Int, Bool ) -> Test
validateLine ( list, isValid ) =
    test
        (toString list ++ " should "
            ++ if isValid then
                "validate"
               else
                "invalidate"
        )
    <|
        \_ ->
            List.map (\n -> { emptyCell | value = n }) list
                |> Array.fromList
                |> listValid
                |> Expect.equal isValid


validation : Test
validation =
    describe "validation"
        [ describe "validate list" <| List.map validateLine validLines
        ]



-- columns : Test
-- columns =
--     describe "columns"
--         [ describe "Valid" (List.range 0 8 |> List.map (isValidColumnTest True validBoard))
--         , describe "Invalid" (List.range 0 8 |> List.map (isValidColumnTest False invalidBoard1))
--         , describe "More invalid" (List.range 0 8 |> List.map (isValidColumnTest False invalidBoard2))
--         ]
--
--
-- rows : Test
-- rows =
--     describe "rows"
--         [ describe "Valid" (List.range 0 8 |> List.map (isValidRowTest True validBoard))
--         , describe "Invalid" (List.range 0 8 |> List.map (isValidRowTest False invalidBoard1))
--         , describe "More invalid" (List.range 0 8 |> List.map (isValidRowTest False invalidBoard2))
--         ]
--
--
-- squares : Test
-- squares =
--     describe "squares"
--         [ describe "valid" (squareTest True validBoard)
--         , describe "invalid" (squareTest False invalidBoard1)
--         , describe "More invalid" (squareTest False invalidBoard2)
--         ]
--
--
-- squareTest : Bool -> Board -> List Test
-- squareTest valid board =
--     List.range 0 8
--         |> List.concatMap
--             (\row ->
--                 List.range 0 8
--                     |> List.map
--                         (\col ->
--                             isValidSquareTest valid board row col
--                         )
--             )
--
--
-- isValidColumnTest : Bool -> Board -> Int -> Test
-- isValidColumnTest valid board col =
--     test (toString col ++ " is valid") <|
--         \_ ->
--             Board.isValidColumn col board
--                 |> Expect.equal valid
--
--
-- isValidRowTest : Bool -> Board -> Int -> Test
-- isValidRowTest valid board row =
--     test (toString row ++ " is valid") <|
--         \_ ->
--             Board.isValidRow row board
--                 |> Expect.equal valid
--
--
-- isValidSquareTest : Bool -> Board -> Int -> Int -> Test
-- isValidSquareTest valid board x y =
--     test ("Square at " ++ toString x ++ "/" ++ toString y) <|
--         \_ ->
--             Board.isValidSquare (Position x y) board
--                 |> Expect.equal valid
--
--
-- invalidBoard1 : Board
-- invalidBoard1 =
--     List.repeat 81 (Just 1)
--         |> Array.fromList
--         |> Board
--
--
-- invalidBoard2 : Board
-- invalidBoard2 =
--     List.repeat 81 Nothing
--         |> Array.fromList
--         |> Board
--
--
-- validBoard : Board
-- validBoard =
--     List.range 0 2
--         |> List.concatMap
--             (\x ->
--                 List.range 0 2
--                     |> List.map (\i -> (i * 3) % 9 + x)
--             )
--         |> List.concatMap
--             (\x ->
--                 List.range x (x + 8)
--                     |> List.map (\i -> ((x + i) % 9) + 1)
--             )
--         |> List.map Just
--         |> Array.fromList
--         |> Board
