module Eleven exposing (main)

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
    "3,8,1005,8,306,1106,0,11,0,0,0,104,1,104,0,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,1002,8,1,28,2,107,3,10,1,101,19,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,102,1,8,59,2,5,13,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,85,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,107,1006,0,43,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,101,0,8,132,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,154,2,4,1,10,2,4,9,10,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1001,8,0,183,1,1102,5,10,1,1102,1,10,1006,0,90,2,9,12,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,221,1006,0,76,1006,0,27,1,102,9,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,102,1,8,252,2,4,9,10,1006,0,66,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,101,0,8,282,1,102,19,10,101,1,9,9,1007,9,952,10,1005,10,15,99,109,628,104,0,104,1,21102,1,387240010644,1,21101,0,323,0,1105,1,427,21102,846541370112,1,1,21101,334,0,0,1106,0,427,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,3425718295,1,1,21102,381,1,0,1105,1,427,21102,179410541715,1,1,21101,0,392,0,1106,0,427,3,10,104,0,104,0,3,10,104,0,104,0,21101,0,718078255872,1,21101,0,415,0,1105,1,427,21102,1,868494234468,1,21102,1,426,0,1105,1,427,99,109,2,21202,-1,1,1,21101,0,40,2,21101,458,0,3,21101,0,448,0,1106,0,491,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,453,454,469,4,0,1001,453,1,453,108,4,453,10,1006,10,485,1102,0,1,453,109,-2,2105,1,0,0,109,4,2102,1,-1,490,1207,-3,0,10,1006,10,508,21102,1,0,-3,22102,1,-3,1,22101,0,-2,2,21102,1,1,3,21102,1,527,0,1106,0,532,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,555,2207,-4,-2,10,1006,10,555,22101,0,-4,-4,1105,1,623,22101,0,-4,1,21201,-3,-1,2,21202,-2,2,3,21101,574,0,0,1105,1,532,21202,1,1,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,593,21102,0,1,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,615,21201,-1,0,1,21101,615,0,0,106,0,490,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0"


type alias Tape =
    Dict Int Cell


type RunState
    = Play
    | Pause


type OutputMode
    = Paint
    | Direction


type alias Hull =
    Dict Location Int


toggleOutputMode : OutputMode -> OutputMode
toggleOutputMode o =
    case o of
        Paint ->
            Direction

        Direction ->
            Paint


type alias Model =
    { point : Int
    , tape : Tape
    , state : State
    , outputs : List ( Operand, Int )
    , runState : RunState
    , input : String
    , relativeBase : Int
    , cycles : Int
    , turtle : Turtle
    }


type Direction
    = North
    | South
    | East
    | West


type Turn
    = Left
    | Right


intToTurn : Int -> Turn
intToTurn n =
    case n of
        0 ->
            Left

        _ ->
            Right


turnTurtle : Turtle -> Turn -> Turtle
turnTurtle turtle turn =
    case ( turtle.dir, turn ) of
        ( North, Left ) ->
            { turtle | dir = West }

        ( North, Right ) ->
            { turtle | dir = East }

        ( South, Left ) ->
            { turtle | dir = East }

        ( South, Right ) ->
            { turtle | dir = West }

        ( East, Left ) ->
            { turtle | dir = North }

        ( East, Right ) ->
            { turtle | dir = South }

        ( West, Left ) ->
            { turtle | dir = South }

        ( West, Right ) ->
            { turtle | dir = North }


advanceTurtle : Turtle -> Turtle
advanceTurtle t =
    let
        ( x, y ) =
            t.pos

        turtle =
            { t
                | outputMode = Paint
            }
    in
    case turtle.dir of
        North ->
            { turtle | pos = ( x, y + 1 ) }

        South ->
            { turtle | pos = ( x, y - 1 ) }

        East ->
            { turtle | pos = ( x + 1, y ) }

        West ->
            { turtle | pos = ( x - 1, y ) }


outPutToTurtle : Turtle -> Int -> Turtle
outPutToTurtle t n =
    case t.outputMode of
        Paint ->
            { t
                | hull = Dict.insert t.pos n t.hull
                , outputMode = Direction
            }

        Direction ->
            n |> intToTurn |> turnTurtle t |> advanceTurtle


getInputFromTurtle : Turtle -> Int
getInputFromTurtle t =
    Dict.get t.pos t.hull |> Maybe.withDefault 0


type alias Turtle =
    { pos : Location
    , dir : Direction
    , outputMode : OutputMode
    , hull : Hull
    }


type alias Location =
    ( Int, Int )


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
    , turtle =
        { hull = Dict.fromList [ ( ( 0, 0 ), 1 ) ]
        , outputMode = Paint
        , dir = North
        , pos = ( 0, 0 )
        }
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
    | ToggleInitHullColor


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
            justReadFrom1 (getInputFromTurtle model.turtle |> Immediate |> Input) modes model "INPUT"

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


math2 : String -> (Int -> Int -> Int) -> Int -> Int -> Operand -> State
math2 errstring fn left right dest =
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
                    ++ errstring

        ( _, _, _ ) ->
            Error <| "for math 2 could not read values to " ++ errstring ++ " from addresses"


math1 : String -> (Int -> a) -> Int -> Operand -> (Int -> a -> State) -> State
math1 errstring fn input dest stateConstructor =
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
                    ++ errstring

        ( _, _ ) ->
            Error <| "for math 1 could not read values to " ++ errstring ++ " from addresses"


mathJump : String -> (Int -> a) -> Int -> Int -> (Int -> a -> State) -> State
mathJump errstring fn input dest stateConstructor =
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
                    , turtle = outPutToTurtle model.turtle value
                }

        Error _ ->
            model

        Done _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.runState ) of
        ( UpdateInput input, _ ) ->
            ( { model | input = input }, Cmd.none )

        ( Tick _, Play ) ->
            ( limitedRecurse 100000 stepModel model
            , Task.perform Tick (Process.sleep 1)
            )

        ( Tick _, Pause ) ->
            ( model, Cmd.none )

        ( Step, _ ) ->
            ( stepModel model, Cmd.none )

        ( ToggleRun, Play ) ->
            ( { model | runState = Pause }, Cmd.none )

        ( ToggleRun, Pause ) ->
            ( { model | runState = Play }, Task.perform Tick (Process.sleep 10) )

        ( ToggleInitHullColor, _ ) ->
            let
                turtle =
                    model.turtle

                hull =
                    turtle.hull

                currentColor =
                    Dict.get ( 0, 0 ) hull |> Maybe.withDefault 0
            in
            ( { model
                | turtle =
                    { turtle
                        | hull =
                            Dict.insert ( 0, 0 )
                                (if currentColor == 1 then
                                    0

                                 else
                                    1
                                )
                                hull
                    }
              }
            , Cmd.none
            )


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


thrush : a -> (a -> b) -> b
thrush a fn =
    fn a


getFromHull : Hull -> ( Int, Int ) -> String
getFromHull h loc =
    case Dict.get loc h |> Maybe.withDefault 0 of
        1 ->
            "⬜"

        _ ->
            "⬛"


renderHull : Hull -> String
renderHull hull =
    let
        minx =
            Dict.keys hull
                |> List.map Tuple.first
                |> List.minimum
                |> Maybe.withDefault -999

        maxx =
            Dict.keys hull
                |> List.map Tuple.first
                |> List.maximum
                |> Maybe.withDefault 999

        miny =
            Dict.keys hull
                |> List.map Tuple.second
                |> List.minimum
                |> Maybe.withDefault -999

        maxy =
            Dict.keys hull
                |> List.map Tuple.second
                |> List.maximum
                |> Maybe.withDefault 999

        xs =
            List.range minx maxx

        ys =
            List.range miny maxy
    in
    List.map (Tuple.pair xs) ys
        |> List.map ((Tuple.mapFirst <| List.map Tuple.pair) >> (\( l, y ) -> List.map (thrush y) l))
        |> List.map (List.map (getFromHull hull) >> String.join "")
        |> List.reverse
        |> String.join "\n"


view : Model -> Html Msg
view model =
    let
        viewCell_ =
            viewCell model.point

        hull =
            model.turtle |> .hull
    in
    H.div
        [ HA.css
            [ Css.float Css.left
            ]
        ]
        ([ H.div []
            [ H.button
                [ HE.onClick Step
                ]
                [ H.text "step" ]
            , H.button
                [ HE.onClick ToggleRun
                ]
                [ H.text <|
                    case model.runState of
                        Play ->
                            "pause"

                        Pause ->
                            "play"
                ]
            , H.span [] [ H.text <| "Relative base: " ++ String.fromInt model.relativeBase ]
            ]
         , H.div [] [ printState model.state |> H.text ]
         , H.div []
            [ "Initial square color (part 1 is black, part 2 is white): " |> H.text
            , H.button
                [ HE.onClick ToggleInitHullColor
                ]
                [ (case Dict.get ( 0, 0 ) model.turtle.hull |> Maybe.withDefault 0 of
                    1 ->
                        "White ⬜"

                    _ ->
                        "Black ⬛"
                  )
                    |> H.text
                ]
            ]
         , H.div []
            [ model.cycles
                |> String.fromInt
                |> (++) "Cycles: "
                |> H.text
            ]
         ]
            ++ (case model.state of
                    Done _ ->
                        [ renderHull model.turtle.hull
                            |> (++) "hull looks like \n"
                            |> H.text
                            |> List.singleton
                            |> H.pre []
                        , " min x "
                            ++ (Dict.keys hull
                                    |> List.map Tuple.first
                                    |> List.minimum
                                    |> Maybe.withDefault -999
                                    |> String.fromInt
                               )
                            ++ " max x "
                            ++ (Dict.keys hull
                                    |> List.map Tuple.first
                                    |> List.maximum
                                    |> Maybe.withDefault 999
                                    |> String.fromInt
                               )
                            ++ " min y "
                            ++ (Dict.keys hull
                                    |> List.map Tuple.second
                                    |> List.minimum
                                    |> Maybe.withDefault -999
                                    |> String.fromInt
                               )
                            ++ " max y "
                            ++ (Dict.keys hull
                                    |> List.map Tuple.second
                                    |> List.maximum
                                    |> Maybe.withDefault 999
                                    |> String.fromInt
                               )
                            |> H.text
                        , hull |> Dict.size |> String.fromInt |> (++) "; hull paint: " |> H.text
                        , Lazy.lazy viewProcessor ( Dict.toList model.tape, viewCell_ )
                        , H.div []
                            [ List.map printOutput model.outputs
                                |> printStrings
                                |> (++) " Outputs "
                                |> H.text
                            ]
                        ]

                    _ ->
                        []
               )
        )


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
