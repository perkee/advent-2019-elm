module Thirteen exposing (main)

import Browser
import Dict exposing (Dict)
import DictGraphics as DG
import Html as H
import Html.Events as HE
import Json.Decode as JD
import Keyboard as K
import Keyboard.Events as KE


inputTape : Int -> String
inputTape i =
    String.fromInt i ++ ",380,379,385,1008,2563,747932,381,1005,381,12,99,109,2564,1101,0,0,383,1101,0,0,382,20102,1,382,1,21002,383,1,2,21101,37,0,0,1105,1,578,4,382,4,383,204,1,1001,382,1,382,1007,382,37,381,1005,381,22,1001,383,1,383,1007,383,26,381,1005,381,18,1006,385,69,99,104,-1,104,0,4,386,3,384,1007,384,0,381,1005,381,94,107,0,384,381,1005,381,108,1106,0,161,107,1,392,381,1006,381,161,1102,1,-1,384,1105,1,119,1007,392,35,381,1006,381,161,1101,0,1,384,20101,0,392,1,21102,1,24,2,21101,0,0,3,21102,138,1,0,1105,1,549,1,392,384,392,21001,392,0,1,21102,1,24,2,21102,1,3,3,21101,161,0,0,1106,0,549,1102,1,0,384,20001,388,390,1,21002,389,1,2,21101,180,0,0,1105,1,578,1206,1,213,1208,1,2,381,1006,381,205,20001,388,390,1,20101,0,389,2,21101,205,0,0,1106,0,393,1002,390,-1,390,1101,0,1,384,21001,388,0,1,20001,389,391,2,21101,0,228,0,1105,1,578,1206,1,261,1208,1,2,381,1006,381,253,20102,1,388,1,20001,389,391,2,21101,0,253,0,1106,0,393,1002,391,-1,391,1102,1,1,384,1005,384,161,20001,388,390,1,20001,389,391,2,21101,0,279,0,1105,1,578,1206,1,316,1208,1,2,381,1006,381,304,20001,388,390,1,20001,389,391,2,21101,0,304,0,1106,0,393,1002,390,-1,390,1002,391,-1,391,1101,1,0,384,1005,384,161,21002,388,1,1,20102,1,389,2,21102,0,1,3,21101,0,338,0,1105,1,549,1,388,390,388,1,389,391,389,20102,1,388,1,20101,0,389,2,21101,0,4,3,21101,0,365,0,1105,1,549,1007,389,25,381,1005,381,75,104,-1,104,0,104,0,99,0,1,0,0,0,0,0,0,369,16,21,1,1,18,109,3,22101,0,-2,1,21202,-1,1,2,21102,0,1,3,21101,0,414,0,1105,1,549,21201,-2,0,1,21201,-1,0,2,21102,429,1,0,1105,1,601,1201,1,0,435,1,386,0,386,104,-1,104,0,4,386,1001,387,-1,387,1005,387,451,99,109,-3,2105,1,0,109,8,22202,-7,-6,-3,22201,-3,-5,-3,21202,-4,64,-2,2207,-3,-2,381,1005,381,492,21202,-2,-1,-1,22201,-3,-1,-3,2207,-3,-2,381,1006,381,481,21202,-4,8,-2,2207,-3,-2,381,1005,381,518,21202,-2,-1,-1,22201,-3,-1,-3,2207,-3,-2,381,1006,381,507,2207,-3,-4,381,1005,381,540,21202,-4,-1,-1,22201,-3,-1,-3,2207,-3,-4,381,1006,381,529,21202,-3,1,-7,109,-8,2106,0,0,109,4,1202,-2,37,566,201,-3,566,566,101,639,566,566,1201,-1,0,0,204,-3,204,-2,204,-1,109,-4,2105,1,0,109,3,1202,-1,37,593,201,-2,593,593,101,639,593,593,21001,0,0,-2,109,-3,2106,0,0,109,3,22102,26,-2,1,22201,1,-1,1,21102,1,487,2,21101,0,575,3,21102,1,962,4,21102,1,630,0,1105,1,456,21201,1,1601,-2,109,-3,2105,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,2,2,2,2,0,2,0,0,0,2,0,2,2,2,2,2,2,0,2,2,2,0,2,0,2,0,2,2,0,2,2,0,2,0,1,1,0,2,0,0,2,0,2,2,2,0,2,2,2,2,2,2,2,2,2,0,2,2,0,2,2,2,2,2,0,2,2,2,0,0,0,1,1,0,2,0,2,2,2,0,0,2,2,2,2,0,2,0,2,0,0,2,2,2,2,2,2,2,0,2,2,0,2,0,0,2,0,0,1,1,0,2,0,2,2,2,2,0,2,2,0,0,0,0,0,2,2,2,2,2,0,0,0,0,0,2,2,2,0,0,2,0,0,2,0,1,1,0,0,2,0,0,0,0,2,0,2,0,2,2,2,2,0,0,2,0,2,2,0,0,2,2,0,2,0,2,2,2,2,0,2,0,1,1,0,2,2,0,2,2,0,0,0,0,0,2,0,2,0,0,2,2,2,0,2,2,0,2,2,2,2,0,2,2,2,2,0,0,0,1,1,0,2,2,2,2,2,2,0,2,0,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,0,2,0,2,0,0,1,1,0,2,2,2,2,2,0,0,2,0,2,0,2,2,0,0,0,0,2,0,0,2,2,2,2,2,2,0,0,0,2,2,2,0,0,1,1,0,0,2,2,2,2,0,0,2,0,2,2,0,0,0,0,0,0,2,2,2,2,2,2,2,2,0,2,2,2,0,0,0,2,0,1,1,0,2,0,2,2,2,0,2,0,2,2,2,2,0,2,2,2,2,0,0,2,2,0,0,2,0,0,0,2,2,2,2,0,0,0,1,1,0,2,0,2,2,2,2,2,2,2,0,0,2,0,0,2,2,2,0,0,2,2,0,2,0,2,0,2,2,2,2,2,0,2,0,1,1,0,2,2,2,0,2,0,2,2,0,2,2,2,2,2,0,0,0,2,2,0,2,2,0,2,0,0,0,0,2,2,2,0,0,0,1,1,0,2,2,0,0,2,2,2,0,0,2,0,2,2,2,0,0,2,2,2,0,0,2,2,2,2,0,0,0,2,0,2,2,2,0,1,1,0,0,0,0,0,2,2,0,2,2,0,2,2,0,2,2,2,0,0,0,2,2,0,0,2,2,0,2,0,0,2,2,0,2,0,1,1,0,2,0,2,0,2,2,0,0,2,2,0,2,0,2,0,2,2,0,2,0,0,2,2,0,2,2,0,2,2,2,2,2,2,0,1,1,0,2,0,0,2,0,2,2,2,0,2,2,2,2,2,2,0,0,2,0,0,2,2,0,0,2,2,2,2,0,0,2,2,0,0,1,1,0,2,2,2,2,0,2,2,2,0,2,2,2,2,0,0,2,2,2,0,2,0,2,0,2,2,0,2,2,0,0,2,2,0,0,1,1,0,2,2,2,2,2,0,0,0,2,2,2,2,2,2,0,2,2,2,0,2,2,0,2,2,2,2,2,0,0,0,2,2,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,36,15,49,65,33,64,90,69,80,38,6,11,42,79,20,83,19,67,76,39,23,24,21,94,31,77,25,39,50,66,66,19,40,89,50,59,3,34,57,47,41,75,90,15,29,53,32,72,19,20,88,89,42,54,77,15,96,66,74,77,51,46,71,16,76,96,20,52,26,56,86,80,43,98,91,64,12,86,11,44,14,18,35,40,53,17,44,43,29,69,75,25,8,46,26,18,14,61,89,42,10,21,57,69,13,74,3,68,8,46,35,29,35,79,81,28,30,87,3,67,75,1,95,98,88,18,29,40,28,23,17,5,52,51,5,40,42,89,31,86,67,42,53,43,42,45,63,43,10,46,95,10,26,65,77,38,24,17,30,21,18,25,59,25,72,54,83,34,36,48,60,48,28,3,6,14,92,84,20,31,68,38,16,11,95,36,89,38,69,70,73,49,15,23,70,65,23,11,34,69,6,60,11,38,70,75,18,43,29,53,26,59,95,27,46,3,78,68,7,61,36,20,77,54,43,54,45,26,86,98,21,11,83,60,30,47,46,83,25,74,36,3,54,22,98,70,10,49,35,14,24,38,31,77,95,33,8,17,43,42,93,81,56,13,72,60,18,70,36,64,15,24,49,60,92,47,67,34,24,58,15,96,13,83,55,67,17,43,84,72,55,38,43,90,94,55,11,56,16,8,68,87,14,19,93,6,23,21,41,17,19,13,37,85,69,77,83,91,70,61,5,13,98,87,45,88,13,71,63,98,41,13,81,19,30,34,83,44,70,84,76,22,68,30,55,42,96,1,71,42,32,95,14,33,50,96,61,95,35,18,67,84,7,39,10,95,7,33,69,55,82,19,94,52,60,46,63,62,93,92,39,69,42,60,35,64,69,62,50,29,13,53,90,62,1,45,92,16,89,3,8,81,45,61,88,12,34,27,23,31,73,65,30,43,19,9,44,45,81,17,57,18,3,64,84,70,15,49,34,53,62,58,11,39,90,28,81,61,38,11,96,52,92,71,49,22,69,25,23,4,98,98,3,83,29,70,39,59,79,56,21,45,75,82,48,52,60,44,89,57,42,63,67,30,16,57,26,28,17,65,90,73,22,8,26,72,47,13,68,19,45,45,49,26,20,6,35,65,85,1,59,51,27,13,88,84,63,66,12,78,43,60,79,92,31,44,72,1,18,12,95,6,50,61,66,79,81,21,8,81,33,63,67,31,12,92,48,13,17,27,15,43,45,1,7,58,17,97,45,36,61,28,23,87,97,27,5,97,2,84,30,29,36,60,95,21,97,32,76,78,83,93,28,35,73,26,27,10,90,50,29,24,78,1,71,6,76,44,89,6,94,44,17,80,66,5,43,23,49,52,40,47,39,81,80,80,87,38,26,2,43,97,15,50,79,73,32,73,12,20,53,73,82,7,38,63,78,68,29,96,14,29,52,54,95,6,59,93,98,46,66,91,16,88,95,55,37,2,44,16,97,30,35,19,96,3,8,47,64,4,49,74,89,1,76,90,77,80,46,48,63,11,93,97,71,37,82,75,91,7,33,20,59,8,93,83,83,49,85,92,33,89,58,72,37,27,56,37,91,39,7,52,19,77,20,3,52,57,12,63,14,34,6,89,93,21,62,53,75,3,97,76,75,68,24,83,84,26,66,16,45,46,6,57,48,84,29,1,60,89,63,40,29,63,63,70,10,74,97,94,95,49,55,87,98,2,98,50,93,18,88,39,80,34,41,57,78,12,41,15,13,11,55,22,65,37,21,46,78,17,78,8,62,1,16,9,33,94,26,55,33,22,25,22,93,71,62,82,51,86,66,97,88,82,9,93,9,30,46,37,95,36,21,80,21,36,89,96,44,97,80,42,29,82,87,78,4,58,19,80,95,85,90,64,4,27,65,5,64,71,43,64,92,92,23,80,14,61,12,11,41,12,16,49,93,67,27,68,29,35,66,14,10,46,11,12,79,76,26,62,4,51,35,22,67,83,62,94,95,53,1,94,61,91,5,54,68,24,3,24,98,38,33,78,72,15,9,82,21,59,73,39,23,97,5,13,39,90,61,10,73,92,48,34,47,54,3,54,69,89,67,13,54,41,51,92,51,59,53,76,3,38,93,45,28,10,90,78,40,24,14,58,72,98,19,70,79,18,62,20,79,3,79,73,54,17,10,31,1,70,42,77,747932"


type alias Tape =
    Dict Int Cell


type alias Model =
    { point : Int
    , tape : Tape
    , state : State
    , outputs : List Int
    , relativeBase : Int
    , inputs : List Int
    }


type Cell
    = Plain Int
    | JustWrittenTo Int Cell -- current value, old value
    | NewCell Int


cellToInt : Cell -> Int
cellToInt c =
    case c of
        JustWrittenTo current _ ->
            current

        NewCell current ->
            current

        Plain current ->
            current


type alias OpNumber =
    Int


type alias ModeNumber =
    Int


type alias Parts =
    { opCode : OpCode
    , modes : List (Int -> Operand)
    }


modeNumberToIntToOperand : ModeNumber -> Int -> Operand
modeNumberToIntToOperand m =
    case m of
        2 ->
            Relative

        1 ->
            Immediate

        _ ->
            -- This is cheating because I'm swallowing errors when IDK the actual mode
            Position


numArgsForCode : OpCode -> Int
numArgsForCode n =
    case n of
        AddCode ->
            3

        MulCode ->
            3

        InputCode ->
            1

        OutputCode ->
            1

        JzCode ->
            2

        JnzCode ->
            2

        LtCode ->
            3

        EqCode ->
            3

        SetbCode ->
            1

        _ ->
            0


keepAddingModes : Int -> Parts -> Parts
keepAddingModes remainingCode parts =
    if List.length parts.modes < numArgsForCode parts.opCode then
        keepAddingModes (remainingCode // 10)
            { parts
                | modes = (remainderBy 10 remainingCode |> modeNumberToIntToOperand) :: parts.modes
            }

    else
        parts


intToParts : OpNumber -> Parts
intToParts i =
    keepAddingModes (i // 100)
        { opCode = remainderBy 100 i |> intToOpCode
        , modes = []
        }


flattenCell : a -> Cell -> Cell
flattenCell _ =
    cellToInt >> Plain


type Msg
    = ProvideInput Int
    | Reset


type State
    = Start
    | JustRead Op
    | JustComputed Int Int -- Addr, Value,
    | JustWrote
    | AwaitInput (List (Int -> Operand)) -- Addr
    | GotInput (List (Int -> Operand)) --Addr
    | JustOutput Operand Int -- Addr, Value
    | ComputedJump Int Bool -- Destination, Will Jump,
    | Done (List Int) -- All
    | Error String


type Operand
    = Position Int
    | Immediate Int
    | Relative Int


type OpCode
    = UnknownCode Int
    | DoneCode
    | AddCode
    | MulCode
    | InputCode
    | OutputCode
    | JnzCode
    | JzCode
    | LtCode
    | EqCode
    | SetbCode


intToOpCode : Int -> OpCode
intToOpCode i =
    Dict.fromList
        [ ( 99, DoneCode )
        , ( 1, AddCode )
        , ( 2, MulCode )
        , ( 3, InputCode )
        , ( 4, OutputCode )
        , ( 5, JnzCode )
        , ( 6, JzCode )
        , ( 7, LtCode )
        , ( 8, EqCode )
        , ( 9, SetbCode )
        ]
        |> Dict.get i
        |> Maybe.withDefault (UnknownCode i)


type Op
    = Add Operand Operand Operand -- left operand, right operand, dest addr
    | Mul Operand Operand Operand -- left operand, right operand, dest addr
    | Input Operand Operand -- input value (will be constant 1), dest addr
    | Output Operand -- source addr
    | JNZ Operand Operand -- input operand, destination address
    | JZ Operand Operand -- input operand, destination address
    | Lt Operand Operand Operand -- left operand, right operand, dest addr for value
    | Eq Operand Operand Operand -- left operand, right operand, dest addr
    | Setb Operand -- value


compareToInt : (comparable -> comparable -> Bool) -> comparable -> comparable -> Int
compareToInt fn a b =
    if fn a b then
        1

    else
        0


makeOperand : Model -> Int -> (Int -> Operand) -> Maybe Operand
makeOperand model index mode =
    Dict.get (model.point + index + 1) model.tape |> Maybe.map (cellToInt >> mode)


valuesErr : String -> Int -> Int -> State
valuesErr errString shouldLength point =
    Error <|
        "could not get "
            ++ String.fromInt shouldLength
            ++ " values after point during "
            ++ errString
            ++ " at "
            ++ String.fromInt point


modesErr : String -> Int -> Int -> Int -> State
modesErr errString shouldLength length point =
    Error <|
        "Not enough modes for "
            ++ errString
            ++ " at "
            ++ String.fromInt point
            ++ ": need "
            ++ String.fromInt shouldLength
            ++ " got "
            ++ (length |> String.fromInt)


justReadFrom3 : (Operand -> Operand -> Operand -> Op) -> List (Int -> Operand) -> Model -> String -> State
justReadFrom3 makeOp modes model errString =
    case modes |> List.reverse |> List.indexedMap (makeOperand model) of
        [ Just leftOperand, Just rightOperand, Just destOperand ] ->
            JustRead <|
                makeOp leftOperand
                    rightOperand
                    destOperand

        [ _, _, _ ] ->
            valuesErr errString
                (List.length modes)
                model.point

        _ ->
            modesErr errString
                3
                (List.length modes)
                model.point


justReadFrom2 : (Operand -> Operand -> Op) -> List (Int -> Operand) -> Model -> String -> State
justReadFrom2 makeOp modes model errString =
    case modes |> List.reverse |> List.indexedMap (makeOperand model) of
        [ Just inputOperand, Just destOperand ] ->
            JustRead <|
                makeOp inputOperand
                    destOperand

        [ _, _ ] ->
            valuesErr errString
                (List.length modes)
                model.point

        _ ->
            modesErr errString
                3
                (List.length modes)
                model.point


justReadFrom1 : (Operand -> Op) -> List (Int -> Operand) -> Model -> String -> State
justReadFrom1 makeOp modes model errString =
    case modes |> List.reverse |> List.indexedMap (makeOperand model) of
        [ Just destOperand ] ->
            JustRead <|
                makeOp
                    destOperand

        [ _ ] ->
            valuesErr errString
                (List.length modes)
                model.point

        _ ->
            modesErr errString
                1
                (List.length modes)
                model.point


updateState : Model -> Parts -> State
updateState model { opCode, modes } =
    case opCode of
        DoneCode ->
            Done []

        AddCode ->
            justReadFrom3 Add modes model "ADD"

        MulCode ->
            justReadFrom3 Mul modes model "MUL"

        InputCode ->
            AwaitInput modes |> Debug.log "awaiting input"

        OutputCode ->
            justReadFrom1 Output modes model "OUTPUT"

        JnzCode ->
            justReadFrom2 JNZ modes model "JNZ"

        JzCode ->
            justReadFrom2 JZ modes model "JZ"

        LtCode ->
            justReadFrom3 Lt modes model "LT"

        EqCode ->
            justReadFrom3 Eq modes model "EQ"

        SetbCode ->
            justReadFrom1 Setb modes model "SET_BASE"

        UnknownCode n ->
            Error <| "unknown op code " ++ String.fromInt n


read : Model -> Model
read model =
    case Dict.get model.point model.tape |> Maybe.map (cellToInt >> intToParts >> updateState model) of
        Just state ->
            { model
                | state = state
            }

        Nothing ->
            { model
                | state = Error <| "point" ++ String.fromInt model.point ++ " is out of the tape"
            }


math2 : (Int -> Int -> Int) -> Int -> Int -> Operand -> State
math2 fn left right dest =
    case
        ( left, right, dest )
    of
        ( lv, rv, Position destAddr ) ->
            JustComputed destAddr <| fn lv rv

        ( _, _, Immediate value ) ->
            Error <|
                "You can't use an immediate value ("
                    ++ String.fromInt value
                    ++ ") for a destination while performing "

        ( _, _, _ ) ->
            Error <| "for math 2 could not read values to " ++ " from addresses"


math1 : (Int -> a) -> Int -> Operand -> (Int -> a -> State) -> State
math1 fn input dest stateConstructor =
    case
        ( input, dest )
    of
        ( _, Position destAddr ) ->
            stateConstructor destAddr <| fn input

        ( _, Immediate value ) ->
            Error <|
                "You can't use an immediate value ("
                    ++ String.fromInt value
                    ++ ") for a destination while performing "

        ( _, _ ) ->
            Error <| "for math 1 could not read values to " ++ " from addresses"


mathJump : (Int -> a) -> Int -> Int -> (Int -> a -> State) -> State
mathJump fn input dest stateConstructor =
    stateConstructor dest <| fn input


getInt : Tape -> Int -> Operand -> Int
getInt tape base operand =
    case operand of
        Relative offset ->
            Dict.get (base + offset) tape |> Maybe.map cellToInt |> Maybe.withDefault 0

        Immediate value ->
            value

        Position addr ->
            Dict.get addr tape |> Maybe.map cellToInt |> Maybe.withDefault 0


getOutput : Tape -> Int -> Operand -> State
getOutput tape base operand =
    getInt tape base operand |> JustOutput operand


toPositional : Int -> Operand -> Operand
toPositional base o =
    case o of
        Relative offset ->
            Position <| base + offset

        _ ->
            o


execute : Op -> Model -> Model
execute op model =
    case op of
        Add left right dest ->
            { model
                | state =
                    math2
                        (+)
                        (getInt model.tape model.relativeBase left)
                        (getInt model.tape model.relativeBase right)
                        (toPositional model.relativeBase dest)
                , point = model.point + 4
            }

        Mul left right dest ->
            { model
                | state =
                    math2
                        (*)
                        (getInt model.tape model.relativeBase left)
                        (getInt model.tape model.relativeBase right)
                        (toPositional model.relativeBase dest)
                , point = model.point + 4
            }

        Input input dest ->
            { model
                | state =
                    math1
                        identity
                        (getInt model.tape model.relativeBase input)
                        (toPositional model.relativeBase dest)
                        JustComputed
                , inputs = model.inputs |> List.drop 1
                , point = model.point + 2
            }

        Output source ->
            { model
                | state =
                    getOutput model.tape model.relativeBase source
                , point = model.point + 2
            }

        JNZ input point ->
            { model
                | state =
                    mathJump
                        ((/=) 0)
                        (getInt model.tape model.relativeBase input)
                        (getInt model.tape model.relativeBase point)
                        ComputedJump
                , point = model.point + 3
            }

        JZ input point ->
            { model
                | state =
                    mathJump
                        ((==) 0)
                        (getInt model.tape model.relativeBase input)
                        (getInt model.tape model.relativeBase point)
                        ComputedJump
                , point = model.point + 3
            }

        Lt left right dest ->
            { model
                | state =
                    math2
                        (compareToInt (<))
                        (getInt model.tape model.relativeBase left)
                        (getInt model.tape model.relativeBase right)
                        (toPositional model.relativeBase dest)
                , point = model.point + 4
            }

        Eq left right dest ->
            { model
                | state =
                    math2
                        (compareToInt (==))
                        (getInt model.tape model.relativeBase left)
                        (getInt model.tape model.relativeBase right)
                        (toPositional model.relativeBase dest)
                , point = model.point + 4
            }

        Setb source ->
            case getInt model.tape model.relativeBase source of
                rb ->
                    { model
                        | relativeBase = model.relativeBase + rb
                        , state = JustWrote
                        , point = model.point + 2
                    }


stepModel : Model -> Model
stepModel model =
    case model.state of
        Start ->
            read model

        JustWrote ->
            read model

        JustRead op ->
            execute op model

        JustComputed addr value ->
            case Dict.get addr model.tape of
                Just old ->
                    { model
                        | tape = Dict.insert addr (JustWrittenTo value old) <| Dict.map flattenCell model.tape
                        , state = JustWrote
                    }

                Nothing ->
                    { model
                        | tape = Dict.insert addr (NewCell value) <| Dict.map flattenCell model.tape
                        , state = JustWrote
                    }

        AwaitInput _ ->
            model

        GotInput modes ->
            case List.head model.inputs of
                Just n ->
                    { model
                        | state =
                            justReadFrom1 (n |> Immediate |> Input)
                                modes
                                model
                                "INPUT"
                        , inputs = List.drop 1 model.inputs
                    }

                Nothing ->
                    model

        ComputedJump addr willJump ->
            if willJump then
                { model
                    | point = addr
                    , state = JustWrote
                }

            else
                { model
                    | state = JustWrote
                }

        JustOutput _ value ->
            read
                { model
                    | outputs = value :: model.outputs
                }

        Error _ ->
            model

        Done _ ->
            model


compose2 : (a -> b -> c) -> (c -> d) -> a -> b -> d
compose2 first second a =
    first a >> second


toDone : Model -> Maybe (List Int)
toDone m =
    case m.state of
        Done n ->
            Just n

        Error _ ->
            Nothing

        _ ->
            m |> stepModel |> toDone


toInput : Model -> Model
toInput m =
    case m.state of
        AwaitInput _ ->
            m

        Done _ ->
            m

        Error _ ->
            m

        _ ->
            m |> stepModel |> toInput


makeModel : Int -> Model
makeModel input =
    { point = 0
    , tape =
        inputTape input
            |> String.split ","
            |> List.filterMap (String.toInt >> Maybe.map Plain)
            |> List.indexedMap Tuple.pair
            |> Dict.fromList
    , state = Start
    , outputs = []
    , relativeBase = 0
    , inputs = []
    }



-- ty https://rtoal.github.io/ple/resources/book-replacement-pages/new-elm-chapter.pdf


insertEverywhere : a -> List a -> List (List a)
insertEverywhere x xs =
    case xs of
        [] ->
            [ [ x ] ]

        y :: ys ->
            (x :: y :: ys) :: List.map ((::) y) (insertEverywhere x ys)


permutations : List a -> List (List a)
permutations =
    List.foldr (List.concatMap << insertEverywhere) [ [] ]


pairWithInput : (a -> b) -> a -> ( a, b )
pairWithInput fn a =
    ( a, fn a )


type alias Tile =
    ( ( Int, Int ), Int )


outputsToTiles : List Tile -> List Int -> List Tile
outputsToTiles soFar ints =
    case ints of
        t :: y :: x :: rest ->
            outputsToTiles (( ( x, y ), t ) :: soFar) rest

        _ ->
            soFar


isBlock : Tile -> Bool
isBlock ( _, t ) =
    t == 2


tileToChar : Maybe Int -> Char
tileToChar i =
    Dict.fromList
        [ ( 1, '+' )
        , ( 2, '#' )
        , ( 3, '#' )
        , ( 4, 'o' )
        ]
        |> Dict.get (Maybe.withDefault 0 i)
        |> Maybe.withDefault ' '


view : Model -> H.Html Msg
view model =
    let
        tiles =
            model.outputs
                |> outputsToTiles []

        pix =
            DG.fromList tiles
    in
    H.div
        [ KE.on KE.Keydown [ ( K.ArrowLeft, ProvideInput -1 ) ]
        , KE.on KE.Keydown [ ( K.ArrowRight, ProvideInput 1 ) ]
        , KE.on KE.Keydown [ ( K.ArrowDown, ProvideInput 0 ) ]
        , KE.on KE.Keydown [ ( K.ArrowUp, ProvideInput 0 ) ]
        ]
        [ H.div []
            [ H.text "Score: "
            , DG.getFrom pix ( -1, 0 )
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "no score"
                |> H.text
            , H.text " blocks left: "
            , List.foldl
                (\( _, t ) count ->
                    if t == 2 then
                        count + 1

                    else
                        count
                )
                0
                tiles
                |> String.fromInt
                |> H.text
            ]
        , pix
            |> Dict.remove ( -1, 0 )
            |> DG.toMatrix
            |> List.map
                (List.map tileToChar >> String.fromList)
            |> String.join "\n"
            |> H.text
            |> List.singleton
            |> H.pre []
        , H.div []
            [ H.button [ HE.onClick <| ProvideInput -1 ] [ H.text " < " ]
            , H.button [ HE.onClick <| ProvideInput 0 ] [ H.text " | " ]
            , H.button [ HE.onClick <| ProvideInput 1 ] [ H.text " > " ]
            , H.button [ HE.onClick <| Reset ] [ H.text "reset" ]
            ]
        ]


update : Msg -> Model -> Model
update msg model =
    case ( msg, model.state ) of
        ( ProvideInput n, AwaitInput modes ) ->
            { model | inputs = n :: model.inputs, state = GotInput modes }
                |> toInput

        ( Reset, _ ) ->
            makeModel 2 |> toInput

        ( _, _ ) ->
            model


main : Program () Model Msg
main =
    Browser.sandbox
        { init = makeModel 2 |> toInput
        , update = update
        , view = view
        }
