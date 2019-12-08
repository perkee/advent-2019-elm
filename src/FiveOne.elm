module FiveOne exposing (main)

import Array exposing (Array)
import Browser
import Css
import Dict
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Html.Styled.Lazy as Lazy
import Time


inputTape : String
inputTape =
    "3,225,1,225,6,6,1100,1,238,225,104,0,1101,65,39,225,2,14,169,224,101,-2340,224,224,4,224,1002,223,8,223,101,7,224,224,1,224,223,223,1001,144,70,224,101,-96,224,224,4,224,1002,223,8,223,1001,224,2,224,1,223,224,223,1101,92,65,225,1102,42,8,225,1002,61,84,224,101,-7728,224,224,4,224,102,8,223,223,1001,224,5,224,1,223,224,223,1102,67,73,224,1001,224,-4891,224,4,224,102,8,223,223,101,4,224,224,1,224,223,223,1102,54,12,225,102,67,114,224,101,-804,224,224,4,224,102,8,223,223,1001,224,3,224,1,224,223,223,1101,19,79,225,1101,62,26,225,101,57,139,224,1001,224,-76,224,4,224,1002,223,8,223,1001,224,2,224,1,224,223,223,1102,60,47,225,1101,20,62,225,1101,47,44,224,1001,224,-91,224,4,224,1002,223,8,223,101,2,224,224,1,224,223,223,1,66,174,224,101,-70,224,224,4,224,102,8,223,223,1001,224,6,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,108,226,226,224,102,2,223,223,1005,224,329,101,1,223,223,1107,226,677,224,1002,223,2,223,1005,224,344,101,1,223,223,8,226,677,224,102,2,223,223,1006,224,359,101,1,223,223,108,677,677,224,1002,223,2,223,1005,224,374,1001,223,1,223,1108,226,677,224,1002,223,2,223,1005,224,389,101,1,223,223,1007,677,677,224,1002,223,2,223,1006,224,404,1001,223,1,223,1108,677,677,224,102,2,223,223,1006,224,419,1001,223,1,223,1008,226,677,224,102,2,223,223,1005,224,434,101,1,223,223,107,677,677,224,102,2,223,223,1006,224,449,1001,223,1,223,1007,226,677,224,102,2,223,223,1005,224,464,101,1,223,223,7,677,226,224,102,2,223,223,1005,224,479,101,1,223,223,1007,226,226,224,102,2,223,223,1005,224,494,101,1,223,223,7,677,677,224,102,2,223,223,1006,224,509,101,1,223,223,1008,677,677,224,1002,223,2,223,1006,224,524,1001,223,1,223,108,226,677,224,1002,223,2,223,1006,224,539,101,1,223,223,8,226,226,224,102,2,223,223,1006,224,554,101,1,223,223,8,677,226,224,102,2,223,223,1005,224,569,1001,223,1,223,1108,677,226,224,1002,223,2,223,1006,224,584,101,1,223,223,1107,677,226,224,1002,223,2,223,1005,224,599,101,1,223,223,107,226,226,224,102,2,223,223,1006,224,614,1001,223,1,223,7,226,677,224,102,2,223,223,1005,224,629,1001,223,1,223,107,677,226,224,1002,223,2,223,1005,224,644,1001,223,1,223,1107,677,677,224,102,2,223,223,1006,224,659,101,1,223,223,1008,226,226,224,1002,223,2,223,1006,224,674,1001,223,1,223,4,223,99,226"



--"3,9,8,9,10,9,4,9,99,-1,8" -- Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
-- "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" -- outputs 1 if input /= 0 or 0 if input == 0


type alias Tape =
    Array Cell


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
    }


type Cell
    = Plain Int
    | JustWrittenTo Int Cell -- current value, old value


initialModel : Model
initialModel =
    { point = 0
    , tape =
        inputTape
            |> String.split ","
            |> List.filterMap (String.toInt >> Maybe.map Plain)
            |> Array.fromList
    , state = Start
    , outputs = []
    , runState = Pause
    , input = ""
    }


cellToInt : Cell -> Int
cellToInt c =
    case c of
        JustWrittenTo current _ ->
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


flattenCell : Cell -> Cell
flattenCell =
    cellToInt >> Plain


type Msg
    = Tick Time.Posix
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


compareToInt : (comparable -> comparable -> Bool) -> comparable -> comparable -> Int
compareToInt fn a b =
    if fn a b then
        1

    else
        0


makeOperand : Model -> Int -> (Int -> Operand) -> Maybe Operand
makeOperand model index mode =
    Array.get (model.point + index + 1) model.tape |> Maybe.map (cellToInt >> mode)


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
            case Array.get 0 model.tape of
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

        UnknownCode n ->
            Error <| "unknown op code " ++ String.fromInt n


read : Model -> Model
read model =
    case Array.get model.point model.tape |> Maybe.map (cellToInt >> intToParts >> updateState model) of
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
            Error errstring


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
            Error <| "could not read values to " ++ errstring ++ " from addresses"


mathJump : String -> (Int -> a) -> Maybe Int -> Maybe Int -> (Int -> a -> State) -> State
mathJump errstring fn input dest stateConstructor =
    case
        ( input, dest )
    of
        ( Just iv, Just dv ) ->
            stateConstructor dv <| fn iv

        ( _, _ ) ->
            Error <| "could not read values to " ++ errstring ++ " from addresses"


getInt : Tape -> Operand -> Maybe Int
getInt tape operand =
    case operand of
        Immediate value ->
            Just value

        Position addr ->
            Array.get addr tape |> Maybe.map cellToInt


getOutput : Tape -> Operand -> State
getOutput tape operand =
    case getInt tape operand of
        Just v ->
            JustOutput operand v

        Nothing ->
            Error <| "Trying to output from " ++ printOperand operand ++ " but got nothing."


execute : Op -> Model -> Model
execute op model =
    case op of
        Add left right dest ->
            { model
                | state =
                    math2
                        (printOperands "ADD" [ left, right ])
                        (+)
                        (getInt model.tape left)
                        (getInt model.tape right)
                        dest
                , point = model.point + 4
            }

        Mul left right dest ->
            { model
                | state =
                    math2
                        (printOperands "MUL" [ left, right ])
                        (*)
                        (getInt model.tape left)
                        (getInt model.tape right)
                        dest
                , point = model.point + 4
            }

        Input input dest ->
            { model
                | state =
                    math1
                        (printOperands "INPUT" [ input ])
                        identity
                        (getInt model.tape input)
                        dest
                        JustComputed
                , point = model.point + 2
            }

        Output source ->
            { model
                | state =
                    getOutput model.tape source
                , point = model.point + 2
            }

        JNZ input point ->
            { model
                | state =
                    mathJump
                        (printOperands "JNZ" [ input ])
                        ((/=) 0)
                        (getInt model.tape input)
                        (getInt model.tape point)
                        ComputedJump
                , point = model.point + 3
            }

        JZ input point ->
            { model
                | state =
                    mathJump
                        (printOperands "JZ" [ input ])
                        ((==) 0)
                        (getInt model.tape input)
                        (getInt model.tape point)
                        ComputedJump
                , point = model.point + 3
            }

        Lt left right dest ->
            { model
                | state =
                    math2
                        (printOperands "LT" [ left, right ])
                        (compareToInt (<))
                        (getInt model.tape left)
                        (getInt model.tape right)
                        dest
                , point = model.point + 4
            }

        Eq left right dest ->
            { model
                | state =
                    math2
                        (printOperands "EQ" [ left, right ])
                        (compareToInt (==))
                        (getInt model.tape left)
                        (getInt model.tape right)
                        dest
                , point = model.point + 4
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
            case Array.get addr model.tape of
                Just old ->
                    { model
                        | tape = Array.set addr (JustWrittenTo value old) <| Array.map flattenCell model.tape
                        , state = JustWrote
                    }

                Nothing ->
                    { model
                        | state = Error <| "could write but cannot remember the old value at " ++ String.fromInt addr
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
                }

        Error _ ->
            model

        Done _ ->
            model


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case ( msg, model.runState, model.input |> String.toInt ) of
        ( UpdateInput input, _, _ ) ->
            ( { model | input = input }, Cmd.none )

        ( _, _, Nothing ) ->
            ( { model | runState = Pause }, Cmd.none )

        ( Tick _, Play, Just _ ) ->
            ( stepModel model, Cmd.none )

        ( Tick _, Pause, Just _ ) ->
            ( model, Cmd.none )

        ( Step, _, Just _ ) ->
            ( stepModel model, Cmd.none )

        ( ToggleRun, Play, Just _ ) ->
            ( { model | runState = Pause }, Cmd.none )

        ( ToggleRun, Pause, Just _ ) ->
            ( { model | runState = Play }, Cmd.none )


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


printOperand : Operand -> String
printOperand o =
    case o of
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


viewProcessor : ( List Cell, ( Int, Cell ) -> Html Msg ) -> Html Msg
viewProcessor ( tape, viewCellForPoint ) =
    tape
        |> List.indexedMap (compose2 Tuple.pair (Lazy.lazy viewCellForPoint))
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
            , H.label [ HA.for "input" ] [ H.text "(For part 1 use 1, for part 2 use 5) Input: " ]
            , H.input
                [ HE.onClick ToggleRun
                , HA.value model.input
                , HA.id "input"
                , HE.onInput UpdateInput
                ]
                []
            ]
        , H.div [] [ printState model.state |> H.text ]
        , H.div []
            [ List.map printOutput model.outputs
                |> printStrings
                |> (++) " Outputs "
                |> H.text
            ]
        , Lazy.lazy viewProcessor ( Array.toList model.tape, viewCell_ )
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 100 Tick


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( initialModel, Cmd.none )
        , view = view >> H.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
