module NineOne exposing (main)

import Browser
import Css
import Dict exposing (Dict)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Html.Styled.Lazy as Lazy
import Platform.Sub
import Process
import Task
import Time


inputTape : String
inputTape =
    "1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1102,3,1,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1101,0,33,1017,1101,24,0,1014,1101,519,0,1028,1102,34,1,1004,1101,0,31,1007,1101,0,844,1025,1102,0,1,1020,1102,38,1,1003,1102,39,1,1008,1102,849,1,1024,1101,0,22,1001,1102,25,1,1009,1101,1,0,1021,1101,0,407,1022,1101,404,0,1023,1101,0,35,1013,1101,27,0,1011,1101,0,37,1016,1102,1,26,1019,1102,28,1,1015,1101,0,30,1000,1102,1,36,1005,1101,0,29,1002,1101,23,0,1012,1102,1,32,1010,1102,21,1,1006,1101,808,0,1027,1102,20,1,1018,1101,0,514,1029,1102,1,815,1026,109,14,2107,24,-5,63,1005,63,199,4,187,1105,1,203,1001,64,1,64,1002,64,2,64,109,-1,2108,21,-7,63,1005,63,225,4,209,1001,64,1,64,1106,0,225,1002,64,2,64,109,-16,1201,6,0,63,1008,63,35,63,1005,63,249,1001,64,1,64,1106,0,251,4,231,1002,64,2,64,109,9,2102,1,2,63,1008,63,37,63,1005,63,271,1105,1,277,4,257,1001,64,1,64,1002,64,2,64,109,11,1208,-8,23,63,1005,63,293,1105,1,299,4,283,1001,64,1,64,1002,64,2,64,109,8,21107,40,39,-8,1005,1017,319,1001,64,1,64,1106,0,321,4,305,1002,64,2,64,109,-28,2101,0,6,63,1008,63,39,63,1005,63,341,1106,0,347,4,327,1001,64,1,64,1002,64,2,64,109,19,2107,26,-7,63,1005,63,363,1106,0,369,4,353,1001,64,1,64,1002,64,2,64,109,1,1202,-9,1,63,1008,63,39,63,1005,63,395,4,375,1001,64,1,64,1105,1,395,1002,64,2,64,109,9,2105,1,-3,1106,0,413,4,401,1001,64,1,64,1002,64,2,64,109,-13,1207,-4,26,63,1005,63,435,4,419,1001,64,1,64,1105,1,435,1002,64,2,64,109,-1,21101,41,0,7,1008,1019,41,63,1005,63,461,4,441,1001,64,1,64,1105,1,461,1002,64,2,64,109,7,21107,42,43,-2,1005,1017,479,4,467,1105,1,483,1001,64,1,64,1002,64,2,64,109,-6,21108,43,46,0,1005,1013,499,1106,0,505,4,489,1001,64,1,64,1002,64,2,64,109,17,2106,0,-2,4,511,1105,1,523,1001,64,1,64,1002,64,2,64,109,-27,1202,-1,1,63,1008,63,28,63,1005,63,547,1001,64,1,64,1106,0,549,4,529,1002,64,2,64,109,18,1206,-1,567,4,555,1001,64,1,64,1106,0,567,1002,64,2,64,109,-16,21102,44,1,6,1008,1011,43,63,1005,63,587,1106,0,593,4,573,1001,64,1,64,1002,64,2,64,109,8,21102,45,1,-1,1008,1012,45,63,1005,63,619,4,599,1001,64,1,64,1105,1,619,1002,64,2,64,109,7,1205,1,633,4,625,1106,0,637,1001,64,1,64,1002,64,2,64,109,-8,2102,1,-3,63,1008,63,25,63,1005,63,659,4,643,1105,1,663,1001,64,1,64,1002,64,2,64,109,14,1206,-5,679,1001,64,1,64,1105,1,681,4,669,1002,64,2,64,109,-28,2101,0,2,63,1008,63,30,63,1005,63,707,4,687,1001,64,1,64,1106,0,707,1002,64,2,64,109,21,21101,46,0,0,1008,1019,48,63,1005,63,727,1106,0,733,4,713,1001,64,1,64,1002,64,2,64,109,-3,21108,47,47,1,1005,1017,751,4,739,1106,0,755,1001,64,1,64,1002,64,2,64,109,-13,1207,0,37,63,1005,63,771,1105,1,777,4,761,1001,64,1,64,1002,64,2,64,109,7,2108,21,-9,63,1005,63,797,1001,64,1,64,1105,1,799,4,783,1002,64,2,64,109,22,2106,0,-5,1001,64,1,64,1106,0,817,4,805,1002,64,2,64,109,-4,1205,-8,829,1106,0,835,4,823,1001,64,1,64,1002,64,2,64,109,-4,2105,1,0,4,841,1105,1,853,1001,64,1,64,1002,64,2,64,109,-30,1208,6,30,63,1005,63,871,4,859,1105,1,875,1001,64,1,64,1002,64,2,64,109,-2,1201,9,0,63,1008,63,22,63,1005,63,897,4,881,1106,0,901,1001,64,1,64,4,64,99,21101,27,0,1,21102,1,915,0,1106,0,922,21201,1,66266,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21102,942,1,0,1105,1,922,22101,0,1,-1,21201,-2,-3,1,21101,0,957,0,1106,0,922,22201,1,-1,-2,1105,1,968,21202,-2,1,-2,109,-3,2106,0,0"


type alias Tape =
    Dict Int Cell


type RunState
    = Play
    | Pause


type alias Model =
    { point : Int
    , tape : Tape
    , state : State
    , outputs : List ( Operand, Int )
    , runState : RunState
    , input : String
    , relativeBase : Int
    , cycles : Int
    }


type Cell
    = Plain Int
    | JustWrittenTo Int Cell -- current value, old value
    | NewCell Int


initialModel : Model
initialModel =
    { point = 0
    , tape =
        inputTape
            |> String.split ","
            |> List.filterMap (String.toInt >> Maybe.map Plain)
            |> List.indexedMap Tuple.pair
            |> Dict.fromList
    , state = Start
    , outputs = []
    , runState = Pause
    , input = ""
    , relativeBase = 0
    , cycles = 0
    }


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
    = Tick ()
    | Step
    | ToggleRun
    | UpdateInput String


type State
    = Start
    | JustRead Op
    | JustComputed Int Int -- Addr, Value,
    | JustWrote
    | JustOutput Operand Int -- Addr, Value
    | ComputedJump Int Bool -- Destination, Will Jump,
    | Done Int -- Value in Cell 0
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
            case Dict.get 0 model.tape of
                Just val ->
                    Done <| cellToInt val

                Nothing ->
                    Error "Done but could not get value at 0"

        AddCode ->
            justReadFrom3 Add modes model "ADD"

        MulCode ->
            justReadFrom3 Mul modes model "MUL"

        InputCode ->
            case String.toInt model.input |> Maybe.map (Immediate >> Input) of
                Just i ->
                    justReadFrom1 i modes model "INPUT"

                Nothing ->
                    model.state

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


math2 : String -> (Int -> Int -> Int) -> Maybe Int -> Maybe Int -> Operand -> State
math2 errstring fn left right dest =
    case
        ( left, right, dest )
    of
        ( Just lv, Just rv, Position destAddr ) ->
            JustComputed destAddr <| fn lv rv

        ( _, _, Immediate value ) ->
            Error <|
                "You can't use an immediate value ("
                    ++ String.fromInt value
                    ++ ") for a destination while performing "
                    ++ errstring

        ( _, _, _ ) ->
            Error <| "for math 2 could not read values to " ++ errstring ++ " from addresses"


math1 : String -> (Int -> a) -> Maybe Int -> Operand -> (Int -> a -> State) -> State
math1 errstring fn input dest stateConstructor =
    case
        ( input, dest )
    of
        ( Just iv, Position destAddr ) ->
            stateConstructor destAddr <| fn iv

        ( _, Immediate value ) ->
            Error <|
                "You can't use an immediate value ("
                    ++ String.fromInt value
                    ++ ") for a destination while performing "
                    ++ errstring

        ( _, _ ) ->
            Error <| "for math 1 could not read values to " ++ errstring ++ " from addresses"


mathJump : String -> (Int -> a) -> Maybe Int -> Maybe Int -> (Int -> a -> State) -> State
mathJump errstring fn input dest stateConstructor =
    case
        ( input, dest )
    of
        ( Just iv, Just dv ) ->
            stateConstructor dv <| fn iv

        ( _, _ ) ->
            Error <| "for math jump could not read values to " ++ errstring ++ " from addresses"


getInt : Tape -> Int -> Operand -> Maybe Int
getInt tape base operand =
    case operand of
        Relative offset ->
            Dict.get (base + offset) tape |> Maybe.map cellToInt

        Immediate value ->
            Just value

        Position addr ->
            Dict.get addr tape |> Maybe.map cellToInt


getOutput : Tape -> Int -> Operand -> State
getOutput tape base operand =
    case getInt tape base operand of
        Just v ->
            JustOutput operand v

        Nothing ->
            Error <| "Trying to output from " ++ printOperand operand ++ " but got nothing."


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
                        (printOperands "ADD" [ left, right ])
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
                        (printOperands "MUL" [ left, right ])
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
                        (printOperands "INPUT" [ input ])
                        identity
                        (getInt model.tape model.relativeBase input)
                        (toPositional model.relativeBase dest)
                        JustComputed
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
                        (printOperands "JNZ" [ input ])
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
                        (printOperands "JZ" [ input ])
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
                        (printOperands "LT" [ left, right ])
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
                        (printOperands "EQ" [ left, right ])
                        (compareToInt (==))
                        (getInt model.tape model.relativeBase left)
                        (getInt model.tape model.relativeBase right)
                        (toPositional model.relativeBase dest)
                , point = model.point + 4
            }

        Setb source ->
            case getInt model.tape model.relativeBase source of
                Just rb ->
                    { model
                        | relativeBase = model.relativeBase + rb
                        , state = JustWrote
                        , point = model.point + 2
                    }

                Nothing ->
                    { model
                        | state = Error <| "Could not set relative base by operand" ++ printOperand source
                    }


stepModel : Model -> Model
stepModel model =
    case model.state of
        Start ->
            read { model | cycles = model.cycles + 1 }

        JustWrote ->
            read { model | cycles = model.cycles + 1 }

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

        JustOutput operand value ->
            read
                { model
                    | outputs = ( operand, value ) :: model.outputs
                    , cycles = model.cycles + 1
                }

        Error _ ->
            model

        Done _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.runState, model.input |> String.toInt ) of
        ( UpdateInput input, _, _ ) ->
            ( { model | input = input }, Cmd.none )

        ( _, _, Nothing ) ->
            ( { model | runState = Pause }, Cmd.none )

        ( Tick _, Play, Just _ ) ->
            ( limitedRecurse 100000 stepModel model
            , Task.perform Tick (Process.sleep 1)
            )

        ( Tick _, Pause, Just _ ) ->
            ( model, Cmd.none )

        ( Step, _, Just _ ) ->
            ( stepModel model, Cmd.none )

        ( ToggleRun, Play, Just _ ) ->
            ( { model | runState = Pause }, Cmd.none )

        ( ToggleRun, Pause, Just _ ) ->
            ( { model | runState = Play }, Task.perform Tick (Process.sleep 10) )


type CellAt
    = Current
    | Arg
    | Dest
    | Rest


intToCellAt : Int -> CellAt
intToCellAt n =
    case n of
        0 ->
            Current

        1 ->
            Arg

        2 ->
            Arg

        3 ->
            Dest

        _ ->
            Rest


viewCell : Int -> ( Int, Cell ) -> Html msg
viewCell point ( index, cell ) =
    H.div
        [ HA.class "tape__cell"
        , HA.css
            [ Css.backgroundColor <|
                case index - point |> intToCellAt of
                    Current ->
                        Css.rgb 12 128 12

                    Arg ->
                        Css.rgb 128 200 12

                    Dest ->
                        Css.rgb 128 255 128

                    Rest ->
                        Css.rgb 200 200 200
            , Css.display Css.block
            , Css.width <| Css.rem 3
            , Css.height <| Css.rem 3
            , Css.lineHeight <| Css.rem 3
            , Css.float Css.left
            , Css.margin4 Css.zero (Css.rem 0.25) (Css.rem 0.25) Css.zero
            , Css.textAlign Css.center
            , Css.position Css.relative
            ]
        ]
    <|
        case cell of
            Plain value ->
                [ H.span [] [ H.text <| String.fromInt <| value ]
                , H.span
                    [ HA.css
                        [ Css.display Css.block
                        , Css.position Css.absolute
                        , Css.top Css.zero
                        , Css.left Css.zero
                        , Css.lineHeight <| Css.rem 1
                        ]
                    ]
                    [ H.text <| String.fromInt <| index ]
                ]

            JustWrittenTo current old ->
                [ H.span [] [ H.text <| String.fromInt <| current ]
                , H.span
                    [ HA.css
                        [ Css.display Css.block
                        , Css.position Css.absolute
                        , Css.top Css.zero
                        , Css.left Css.zero
                        , Css.lineHeight <| Css.rem 1
                        ]
                    ]
                    [ H.text <| String.fromInt <| index ]
                , H.del
                    [ HA.css
                        [ Css.display Css.block
                        , Css.position Css.absolute
                        , Css.bottom Css.zero
                        , Css.right Css.zero
                        , Css.lineHeight <| Css.rem 1
                        ]
                    ]
                  <|
                    List.singleton <|
                        H.text <|
                            String.fromInt <|
                                cellToInt old
                ]

            NewCell current ->
                [ H.span [] [ H.text <| String.fromInt <| current ]
                , H.span
                    [ HA.css
                        [ Css.display Css.block
                        , Css.position Css.absolute
                        , Css.top Css.zero
                        , Css.left Css.zero
                        , Css.lineHeight <| Css.rem 1
                        ]
                    ]
                    [ H.text <| String.fromInt <| index ]
                , H.del
                    [ HA.css
                        [ Css.display Css.block
                        , Css.position Css.absolute
                        , Css.bottom Css.zero
                        , Css.right Css.zero
                        , Css.lineHeight <| Css.rem 1
                        ]
                    ]
                  <|
                    List.singleton <|
                        H.text "!!!"
                ]


printOperand : Operand -> String
printOperand o =
    case o of
        Relative n ->
            "(R: " ++ String.fromInt n ++ ")"

        Immediate n ->
            "(I: " ++ String.fromInt n ++ ")"

        Position n ->
            "(P: " ++ String.fromInt n ++ ")"


printOperands : String -> List Operand -> String
printOperands label nums =
    label :: (nums |> List.map printOperand) |> String.join " "


printOp : Op -> String
printOp op =
    case op of
        Add a b c ->
            printOperands "MUL" [ a, b, c ]

        Mul a b c ->
            printOperands "MUL" [ a, b, c ]

        Input a b ->
            printOperands "INPUT" [ a, b ]

        Output a ->
            printOperands "OUTPUT" [ a ]

        JNZ a b ->
            printOperands "JNZ" [ a, b ]

        JZ a b ->
            printOperands "JZ" [ a, b ]

        Lt a b c ->
            printOperands "LT" [ a, b, c ]

        Eq a b c ->
            printOperands "EQ" [ a, b, c ]

        Setb a ->
            printOperands "SETB" [ a ]


printState : State -> String
printState s =
    case s of
        Start ->
            "Start"

        JustRead op ->
            printOp op

        JustOutput operand value ->
            printOperands "Output: " [ operand ] ++ ": " ++ String.fromInt value

        JustComputed addr value ->
            String.fromInt addr ++ " <= " ++ String.fromInt value

        ComputedJump dest jump ->
            "will "
                ++ (if jump then
                        ""

                    else
                        "not "
                   )
                ++ "jump to "
                ++ String.fromInt dest

        Error errString ->
            "error " ++ errString

        JustWrote ->
            "wrote"

        Done val ->
            "done: " ++ String.fromInt val


compose2 : (a -> b -> c) -> (c -> d) -> a -> b -> d
compose2 first second a =
    first a >> second


viewProcessor : ( List ( Int, Cell ), ( Int, Cell ) -> Html Msg ) -> Html Msg
viewProcessor ( tapeList, viewCellForPoint ) =
    tapeList
        |> List.map (Lazy.lazy viewCellForPoint)
        |> H.div [ HA.class "tape" ]


printOutput : ( Operand, Int ) -> String
printOutput ( o, int ) =
    "(" ++ printOperand o ++ ", " ++ String.fromInt int ++ ")"


printStrings : List String -> String
printStrings s =
    (List.length s |> String.fromInt)
        ++ ": "
        ++ String.join ", " s


maybeToBoolean : Maybe a -> Bool
maybeToBoolean m =
    case m of
        Just _ ->
            True

        Nothing ->
            False


view : Model -> Html Msg
view model =
    let
        viewCell_ =
            viewCell model.point
    in
    H.div
        [ HA.css
            [ Css.float Css.left
            ]
        ]
        [ H.div []
            [ H.button
                [ HE.onClick Step
                , HA.disabled <| not <| maybeToBoolean <| String.toInt model.input
                ]
                [ H.text "step" ]
            , H.button
                [ HE.onClick ToggleRun
                , HA.disabled <| not <| maybeToBoolean <| String.toInt model.input
                ]
                [ H.text <|
                    case model.runState of
                        Play ->
                            "pause"

                        Pause ->
                            "play"
                ]
            , H.label [ HA.for "input" ] [ H.text "(For part 1 use 1, for part 2 use 2) Input: " ]
            , H.input
                [ HE.onClick ToggleRun
                , HA.value model.input
                , HA.id "input"
                , HE.onInput UpdateInput
                ]
                []
            , H.span [] [ H.text <| "Relative base: " ++ String.fromInt model.relativeBase ]
            ]
        , H.div [] [ printState model.state |> H.text ]
        , H.div []
            [ List.map printOutput model.outputs
                |> printStrings
                |> (++) " Outputs "
                |> H.text
            ]
        , H.div []
            [ model.cycles
                |> String.fromInt
                |> (++) "Cycles: "
                |> H.text
            ]
        , case model.state of
            Done _ ->
                Lazy.lazy viewProcessor ( Dict.toList model.tape, viewCell_ )

            _ ->
                H.text ""
        ]


limitedRecurse : Int -> (a -> a) -> a -> a
limitedRecurse count fn arg =
    case count - 1 of
        0 ->
            arg

        c ->
            limitedRecurse c fn <| fn arg


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view >> H.toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }
