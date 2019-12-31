module Fifteen exposing (main)

import Browser
import Dict exposing (Dict)
import DictGraphics as DG
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Keyboard as K
import Keyboard.Arrows as KA
import Keyboard.Events as KE


inputTape : Int -> String
inputTape _ =
    "3,1033,1008,1033,1,1032,1005,1032,31,1008,1033,2,1032,1005,1032,58,1008,1033,3,1032,1005,1032,81,1008,1033,4,1032,1005,1032,104,99,101,0,1034,1039,101,0,1036,1041,1001,1035,-1,1040,1008,1038,0,1043,102,-1,1043,1032,1,1037,1032,1042,1105,1,124,1001,1034,0,1039,102,1,1036,1041,1001,1035,1,1040,1008,1038,0,1043,1,1037,1038,1042,1106,0,124,1001,1034,-1,1039,1008,1036,0,1041,1001,1035,0,1040,101,0,1038,1043,101,0,1037,1042,1106,0,124,1001,1034,1,1039,1008,1036,0,1041,1002,1035,1,1040,1002,1038,1,1043,1001,1037,0,1042,1006,1039,217,1006,1040,217,1008,1039,40,1032,1005,1032,217,1008,1040,40,1032,1005,1032,217,1008,1039,1,1032,1006,1032,165,1008,1040,35,1032,1006,1032,165,1102,1,2,1044,1105,1,224,2,1041,1043,1032,1006,1032,179,1101,0,1,1044,1106,0,224,1,1041,1043,1032,1006,1032,217,1,1042,1043,1032,1001,1032,-1,1032,1002,1032,39,1032,1,1032,1039,1032,101,-1,1032,1032,101,252,1032,211,1007,0,63,1044,1105,1,224,1102,1,0,1044,1105,1,224,1006,1044,247,1001,1039,0,1034,102,1,1040,1035,1001,1041,0,1036,1001,1043,0,1038,101,0,1042,1037,4,1044,1105,1,0,60,55,93,19,49,51,86,12,18,69,42,30,84,1,28,84,15,15,70,11,75,8,67,37,76,61,72,2,49,82,25,57,77,51,87,60,21,66,5,90,56,21,74,75,51,54,83,69,57,85,99,40,94,14,84,69,34,51,92,29,28,2,76,1,35,70,5,91,91,61,86,2,35,74,78,44,98,44,5,78,4,79,53,99,80,11,75,29,2,82,31,71,82,60,22,90,68,11,84,69,8,66,74,53,22,69,19,49,55,69,75,36,65,18,83,37,17,10,78,89,4,74,29,51,96,11,64,15,99,52,51,99,14,78,66,7,99,20,26,64,91,12,94,38,65,87,91,69,5,87,28,2,62,45,83,35,52,19,21,83,25,51,93,92,7,70,39,92,84,31,1,98,92,58,30,75,22,89,79,44,14,66,11,93,36,45,90,42,18,87,73,99,5,95,94,20,64,78,70,98,41,52,98,5,73,94,19,57,64,88,59,83,33,51,71,25,93,43,14,92,83,44,83,41,52,31,91,95,51,36,98,65,45,10,89,58,51,52,88,94,59,98,2,45,93,83,46,74,76,11,38,9,84,99,43,97,6,28,64,28,72,81,87,74,68,14,27,80,96,44,10,96,36,2,33,96,78,45,30,87,89,90,50,2,72,77,10,12,64,74,53,7,74,57,81,28,68,11,8,47,16,88,17,42,99,58,92,36,70,32,83,37,49,16,97,61,88,91,54,17,33,55,29,22,85,82,30,81,40,62,69,94,47,69,25,77,33,87,67,40,44,96,31,75,27,80,8,16,75,67,41,82,52,95,17,56,99,84,66,53,65,70,87,61,15,82,86,55,96,8,24,79,99,8,79,80,7,64,69,1,67,5,74,20,64,4,98,13,53,2,64,23,33,78,77,51,91,13,24,69,49,56,77,64,10,75,11,67,86,48,98,95,19,94,20,11,62,97,62,83,97,12,95,97,90,20,72,75,49,56,16,65,52,88,95,61,44,86,83,94,9,25,71,99,46,80,80,32,38,56,83,49,89,55,75,98,52,77,85,29,42,94,29,7,75,81,16,28,57,24,92,57,67,27,83,42,75,88,62,50,2,94,3,42,73,17,80,73,91,62,67,84,16,76,44,16,70,36,79,90,41,90,91,62,26,86,94,34,68,59,27,82,74,18,19,98,56,2,90,96,70,28,67,38,51,84,83,13,34,4,52,67,77,31,93,12,41,86,26,61,59,67,73,80,19,48,60,94,57,72,56,36,77,73,57,59,94,69,5,37,90,72,62,4,85,12,65,94,81,5,99,30,58,73,18,90,89,6,87,82,27,41,87,46,97,19,85,11,81,79,17,12,94,46,99,56,77,86,11,20,65,97,37,1,71,21,37,72,29,41,83,39,24,86,72,25,26,20,75,78,34,75,33,38,89,13,31,55,82,81,15,88,36,76,82,22,24,84,73,53,8,82,83,71,15,82,44,88,41,74,80,86,19,59,65,70,76,62,59,79,34,20,30,28,67,35,93,34,56,65,98,97,59,93,54,84,11,85,70,95,17,69,28,79,65,52,69,72,10,72,2,68,84,56,12,64,74,83,13,69,78,5,51,91,41,88,72,10,97,33,97,33,86,19,96,59,64,44,42,88,4,57,20,84,54,44,92,28,17,86,15,50,5,76,37,10,97,39,33,94,5,82,7,92,9,84,55,64,23,69,9,96,49,81,28,69,76,92,53,88,92,92,61,78,44,74,99,96,51,79,65,71,58,86,34,96,96,96,26,88,0,0,21,21,1,10,1,0,0,0,0,0,0"


type alias Tape =
    Dict Int Cell


type alias Processor =
    { point : Int
    , tape : Tape
    , state : State
    , outputs : Dict ( Int, Int ) Substrate
    , relativeBase : Int
    , inputs : List Int
    , currentLocation : ( Int, Int )
    , newLocation : ( Int, Int )
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
    | Undo
    | KeyboardMsg K.Msg


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


makeOperand : Processor -> Int -> (Int -> Operand) -> Maybe Operand
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


justReadFrom3 : (Operand -> Operand -> Operand -> Op) -> List (Int -> Operand) -> Processor -> String -> State
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


justReadFrom2 : (Operand -> Operand -> Op) -> List (Int -> Operand) -> Processor -> String -> State
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


justReadFrom1 : (Operand -> Op) -> List (Int -> Operand) -> Processor -> String -> State
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


updateState : Processor -> Parts -> State
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


read : Processor -> Processor
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


execute : Op -> Processor -> Processor
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

                -- , inputs = model.inputs |> List.drop 1
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


type Distance
    = FromOrigin Int
    | FromOriginAndOxygen Int Int
    | NoDistance


type Substrate
    = Wall
    | Floor Distance
    | Oxygen Distance
    | Origin Distance
    | Droid


substrateToDistance : Substrate -> Distance
substrateToDistance s =
    case s of
        Floor d ->
            d

        Origin d ->
            d

        Oxygen d ->
            d

        _ ->
            NoDistance


distanceToInts : Distance -> ( Maybe Int, Maybe Int )
distanceToInts d =
    case d of
        FromOrigin n ->
            ( Just n, Nothing )

        FromOriginAndOxygen n m ->
            ( Just n, Just m )

        NoDistance ->
            ( Nothing, Nothing )


distance : ( Int, Int ) -> Dict ( Int, Int ) Substrate -> Distance
distance ( x, y ) outputs =
    let
        fromOrigin =
            case
                Dict.get ( x, y ) outputs
                    |> Maybe.map substrateToDistance
            of
                Just (FromOrigin n) ->
                    Just n

                _ ->
                    Nothing

        neighbors =
            [ Dict.get ( x - 1, y ) outputs
            , Dict.get ( x + 1, y ) outputs
            , Dict.get ( x, y - 1 ) outputs
            , Dict.get ( x, y + 1 ) outputs
            ]
                |> List.filterMap
                    (Maybe.map (substrateToDistance >> distanceToInts))

        fromOrigins =
            neighbors |> List.filterMap Tuple.first |> List.minimum

        fromOxygens =
            neighbors |> List.filterMap Tuple.second |> List.minimum
    in
    case ( fromOrigins, fromOxygens ) of
        ( Just n, Just m ) ->
            FromOriginAndOxygen (n + 1) (m + 1)

        ( Just n, Nothing ) ->
            FromOrigin (n + 1)

        ( Nothing, _ ) ->
            NoDistance


stepProcessor : Processor -> Processor
stepProcessor processor =
    case processor.state of
        Start ->
            read processor

        JustWrote ->
            read processor

        JustRead op ->
            execute op processor

        JustComputed addr value ->
            case Dict.get addr processor.tape of
                Just old ->
                    { processor
                        | tape = Dict.insert addr (JustWrittenTo value old) <| Dict.map flattenCell processor.tape
                        , state = JustWrote
                    }

                Nothing ->
                    { processor
                        | tape = Dict.insert addr (NewCell value) <| Dict.map flattenCell processor.tape
                        , state = JustWrote
                    }

        AwaitInput _ ->
            processor

        GotInput modes ->
            case List.head processor.inputs of
                Just n ->
                    { processor
                        | state =
                            justReadFrom1 (n |> Immediate |> Input)
                                modes
                                processor
                                "INPUT"

                        --, inputs = List.drop 1 processor.inputs
                    }

                Nothing ->
                    processor

        ComputedJump addr willJump ->
            if willJump then
                { processor
                    | point = addr
                    , state = JustWrote
                }

            else
                { processor
                    | state = JustWrote
                }

        JustOutput _ value ->
            case value of
                0 ->
                    { processor
                        | outputs =
                            Dict.insert processor.newLocation
                                Wall
                                processor.outputs
                    }
                        |> read

                1 ->
                    { processor
                        | outputs =
                            processor.outputs
                                |> Dict.insert
                                    processor.newLocation
                                    (Floor <|
                                        distance
                                            processor.newLocation
                                            processor.outputs
                                    )
                                |> Dict.insert ( 0, 0 ) (Origin <| FromOrigin 0)
                        , currentLocation = processor.newLocation
                    }
                        |> read

                2 ->
                    { processor
                        | outputs =
                            Dict.insert processor.newLocation
                                (Oxygen <|
                                    distance
                                        processor.newLocation
                                        processor.outputs
                                )
                                processor.outputs
                        , currentLocation = processor.newLocation
                    }
                        |> read

                _ ->
                    { processor
                        | state =
                            Error <|
                                "Unknown output: "
                                    ++ String.fromInt value
                    }

        Error _ ->
            processor

        Done _ ->
            processor


compose2 : (a -> b -> c) -> (c -> d) -> a -> b -> d
compose2 first second a =
    first a >> second


toDone : Processor -> Maybe (List Int)
toDone m =
    case m.state of
        Done n ->
            Just n

        Error _ ->
            Nothing

        _ ->
            m |> stepProcessor |> toDone


toInput : Processor -> Processor
toInput m =
    case m.state of
        AwaitInput _ ->
            m

        Done _ ->
            m

        Error _ ->
            m

        _ ->
            m |> stepProcessor |> toInput


makeProcessor : Int -> Processor
makeProcessor input =
    { point = 0
    , tape =
        inputTape input
            |> String.split ","
            |> List.filterMap (String.toInt >> Maybe.map Plain)
            |> List.indexedMap Tuple.pair
            |> Dict.fromList
    , state = Start
    , outputs = Dict.empty
    , relativeBase = 0
    , inputs =
        [ 4
        , 3
        , 3
        , 4
        , 4
        , 3
        , 3
        , 3
        , 3
        , 3
        , 1
        , 1
        , 1
        , 1
        , 1
        , 4
        , 4
        , 4
        , 4
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 3
        , 3
        , 3
        , 3
        , 4
        , 4
        , 4
        , 4
        , 4
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 2
        , 2
        , 2
        , 3
        , 3
        , 3
        , 3
        , 3
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 3
        , 3
        , 3
        , 3
        , 3
        , 1
        , 1
        , 1
        , 1
        , 1
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 2
        , 2
        , 2
        , 2
        , 2
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 1
        , 1
        , 1
        , 1
        , 3
        , 3
        , 3
        , 3
        , 3
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 2
        , 2
        , 2
        , 3
        , 3
        , 3
        , 2
        , 2
        , 2
        , 4
        , 4
        , 4
        , 2
        , 2
        , 4
        , 4
        , 2
        , 2
        , 4
        , 4
        , 2
        , 2
        , 1
        , 4
        , 4
        , 4
        , 1
        , 1
        , 1
        , 1
        , 1
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 1
        , 1
        , 1
        , 1
        , 1
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 2
        , 2
        , 2
        , 2
        , 2
        , 1
        , 1
        , 1
        , 1
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 3
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 2
        , 2
        , 2
        , 2
        , 2
        , 3
        , 3
        , 3
        , 3
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 2
        , 3
        , 4
        , 2
        , 1
        , 1
        , 1
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 4
        , 4
        , 3
        , 3
        , 1
        , 1
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 2
        , 2
        , 2
        , 2
        , 2
        , 3
        , 3
        , 3
        , 3
        , 1
        , 1
        , 1
        , 3
        , 3
        , 3
        , 2
        , 3
        , 3
        , 3
        , 3
        , 2
        , 2
        , 2
        , 2
        , 3
        , 3
        , 3
        , 3
        , 3
        , 1
        , 1
        , 1
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 2
        , 2
        , 2
        , 2
        , 3
        , 3
        , 3
        , 3
        , 3
        , 1
        , 1
        , 1
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 2
        , 2
        , 2
        , 2
        , 2
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 1
        , 1
        , 1
        , 1
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 1
        , 1
        , 1
        , 1
        , 1
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 1
        , 1
        , 4
        , 4
        , 1
        , 1
        , 4
        , 4
        , 1
        , 1
        , 3
        , 3
        , 1
        , 1
        , 4
        , 4
        , 1
        , 1
        , 4
        , 4
        , 1
        , 1
        , 1
        , 1
        , 1
        , 3
        , 3
        , 3
        , 3
        , 1
        , 1
        , 1
        , 1
        , 1
        , 3
        , 3
        , 3
        , 3
        , 2
        , 3
        , 4
        , 1
        , 4
        , 4
        , 4
        , 1
        , 1
        , 1
        , 1
        , 1
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 3
        , 3
        , 3
        , 3
        , 3
        , 1
        , 1
        , 1
        , 1
        , 1
        , 3
        , 3
        , 3
        , 3
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 1
        , 1
        , 1
        , 1
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 2
        , 1
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 3
        , 3
        , 3
        , 3
        , 1
        , 1
        , 1
        , 1
        , 2
        , 2
        , 4
        , 4
        , 4
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 3
        , 3
        , 3
        , 3
        , 1
        , 3
        , 3
        , 3
        , 2
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 2
        , 2
        , 2
        , 2
        , 2
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 2
        , 2
        , 2
        , 2
        , 4
        , 4
        , 4
        , 4
        , 2
        , 2
        , 2
        , 2
        , 3
        , 3
        , 3
        , 3
        , 2
        , 2
        , 2
        , 2
        , 3
        , 3
        , 3
        , 2
        , 2
        , 3
        , 3
        , 3
        , 2
        , 2
        , 2
        , 2
        , 2
        , 3
        , 3
        , 3
        , 3
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 2
        , 2
        , 2
        , 2
        , 2
        , 3
        , 3
        , 3
        , 3
        , 4
        , 4
        , 4
        , 4
        , 4
        , 4
        , 2
        , 2
        , 2
        , 3
        , 3
        , 3
        , 3
        , 2
        , 2
        , 2
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 3
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 4
        , 4
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 2
        , 3
        , 3
        , 3
        , 3
        , 3
        , 1
        , 1
        , 1
        , 1
        , 1
        , 2
        , 2
        , 2
        , 1
        , 1
        , 1
        , 2
        , 2
        , 1
        , 1
        , 2
        , 2
        , 2
        , 2
        , 1
        , 1
        , 1
        , 1
        , 2
        , 2
        , 2
        ]
    , currentLocation = ( 0, 0 )
    , newLocation = ( 0, 0 )
    }


tileToCell : Maybe Substrate -> H.Html msg
tileToCell i =
    case i of
        Just Wall ->
            H.td [ HA.style "background-color" "black" ] []

        Just (Floor d) ->
            H.td [ HA.style "background-color" "white" ]
                [ d |> Debug.toString |> H.text ]

        Just (Oxygen d) ->
            H.td [ HA.style "background-color" "blue" ]
                [ (d |> Debug.toString |> (++) "O") |> H.text ]

        Just Droid ->
            H.td [ HA.style "background-color" "grey" ]
                [ "\u{1F916}" |> H.text ]

        Just (Origin d) ->
            H.td [ HA.style "background-color" "red" ]
                [ (d |> Debug.toString |> (++) "+") |> H.text ]

        Nothing ->
            H.td [ HA.style "background-color" "lightgrey" ]
                [ "?" |> H.text ]


view : Model -> H.Html Msg
view model =
    let
        pix =
            model.processor.outputs
                |> Dict.insert model.processor.currentLocation Droid
    in
    H.div
        []
        [ H.div []
            [ " [ "
                ++ (model.processor.inputs |> List.map String.fromInt |> String.join ", ")
                ++ " ] "
                |> H.text
            ]
        , H.div []
            [ ("Current location ("
                ++ (model.processor.currentLocation
                        |> Tuple.first
                        |> String.fromInt
                   )
                ++ ", "
                ++ (model.processor.currentLocation
                        |> Tuple.second
                        |> String.fromInt
                   )
                ++ "), New location ("
                ++ (model.processor.newLocation
                        |> Tuple.first
                        |> String.fromInt
                   )
                ++ ", "
                ++ (model.processor.newLocation
                        |> Tuple.second
                        |> String.fromInt
                   )
                ++ ") Distance: "
                ++ (Dict.get model.processor.currentLocation model.processor.outputs
                        |> Maybe.map (substrateToDistance >> Debug.toString)
                        |> Maybe.withDefault "no distance??"
                   )
              )
                |> H.text
            ]
        , pix
            |> DG.toMatrix
            |> List.map
                (List.map tileToCell >> H.tr [])
            |> List.reverse
            |> H.tbody []
            |> List.singleton
            |> H.table []
        , H.div []
            [ H.button [ HE.onClick <| ProvideInput 3 ] [ H.text " < " ]
            , H.button [ HE.onClick <| ProvideInput 2 ] [ H.text " v " ]
            , H.button [ HE.onClick <| ProvideInput 1 ] [ H.text " ^ " ]
            , H.button [ HE.onClick <| ProvideInput 4 ] [ H.text " > " ]
            , H.button [ HE.onClick <| Undo ] [ H.text "Undo" ]
            , H.text "or use arrows"
                |> List.singleton
                |> H.p []
            ]
        , H.node "style" [] [ H.text "td {min-width: 26px}" ]
        ]


directionToDeltas : Int -> ( Int, Int )
directionToDeltas n =
    [ ( 1, ( 0, 1 ) )
    , ( 2, ( 0, -1 ) )
    , ( 3, ( -1, 0 ) )
    , ( 4, ( 1, 0 ) )
    ]
        |> Dict.fromList
        |> Dict.get n
        |> Maybe.withDefault ( 0, 0 )


update : Msg -> Model -> Model
update msg model =
    case ( msg, model.processor.state ) of
        ( ProvideInput n, AwaitInput modes ) ->
            let
                processor =
                    model.processor

                ( dx, dy ) =
                    directionToDeltas n

                ( x, y ) =
                    processor.currentLocation

                newProc =
                    { processor
                        | inputs = n :: processor.inputs
                        , state = GotInput modes
                        , newLocation = ( x + dx, y + dy )
                    }
                        |> toInput
            in
            { model
                | processor = newProc
                , saves = processor :: model.saves |> List.take 500 -- undo only 500
            }

        ( ProvideInput _, _ ) ->
            model

        ( Undo, _ ) ->
            case List.head model.saves of
                Just p ->
                    { processor = p
                    , saves = List.drop 1 model.saves
                    }

                Nothing ->
                    model

        ( KeyboardMsg keebMsg, _ ) ->
            case K.update keebMsg [] |> KA.arrowsDirection of
                KA.North ->
                    update (ProvideInput 1) model

                KA.South ->
                    update (ProvideInput 2) model

                KA.West ->
                    update (ProvideInput 3) model

                KA.East ->
                    update (ProvideInput 4) model

                _ ->
                    -- this event fires on keyup as well,
                    -- in which case the direction is NoDirection
                    -- So do nothing for that one
                    model


type alias Model =
    { processor : Processor
    , saves : List Processor
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map KeyboardMsg K.subscriptions


andNone : x -> ( x, Cmd msg )
andNone x =
    ( x, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                { processor = makeProcessor 2 |> toInput
                , saves = []
                }
                    |> andNone
        , update = compose2 update andNone
        , view = view
        , subscriptions = subscriptions
        }
