module Seven exposing (main)

import Dict exposing (Dict)
import Html
import Html.Styled as H


inputTape : String
inputTape =
    "3,8,1001,8,10,8,105,1,0,0,21,46,55,76,89,106,187,268,349,430,99999,3,9,101,4,9,9,1002,9,2,9,101,5,9,9,1002,9,2,9,101,2,9,9,4,9,99,3,9,1002,9,5,9,4,9,99,3,9,1001,9,2,9,1002,9,4,9,101,2,9,9,1002,9,3,9,4,9,99,3,9,1001,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,4,9,1001,9,4,9,102,5,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,99"


type alias Tape =
    Dict Int Cell


type alias Model =
    { point : Int
    , tape : Tape
    , state : State
    , outputs : List Int
    , inputs : List Int
    , relativeBase : Int
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
    = Tick ()
    | Step
    | ToggleRun


type State
    = Start
    | JustRead Op
    | JustComputed Int Int -- Addr, Value,
    | JustWrote
    | JustOutput Operand Int -- Addr, Value
    | ComputedJump Int Bool -- Destination, Will Jump,
    | Done Int -- Latest Output
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
            case List.head model.outputs of
                Just val ->
                    Done val

                Nothing ->
                    Error "Done but could not get latest output"

        AddCode ->
            justReadFrom3 Add modes model "ADD"

        MulCode ->
            justReadFrom3 Mul modes model "MUL"

        InputCode ->
            case List.head model.inputs of
                Just n ->
                    justReadFrom1 (n |> Immediate |> Input) modes model "INPUT"

                Nothing ->
                    Error <| "No more inputs"

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
                    | outputs = value :: model.outputs
                }

        Error _ ->
            model

        Done _ ->
            model


compose2 : (a -> b -> c) -> (c -> d) -> a -> b -> d
compose2 first second a =
    first a >> second


toOutput : Model -> Maybe Int
toOutput m =
    case m.state of
        Done n ->
            Just n

        Error _ ->
            Nothing

        _ ->
            m |> stepModel |> toOutput


makeModel : Int -> Maybe Int -> Maybe Model
makeModel phase previousOutput =
    case previousOutput of
        Just n ->
            Just
                { point = 0
                , tape =
                    inputTape
                        |> String.split ","
                        |> List.filterMap (String.toInt >> Maybe.map Plain)
                        |> List.indexedMap Tuple.pair
                        |> Dict.fromList
                , state = Start
                , outputs = []
                , inputs = [ phase, n ]
                , relativeBase = 0
                }

        Nothing ->
            Nothing



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


getTotalBoost : List Int -> Maybe Int
getTotalBoost =
    List.foldl (compose2 makeModel <| Maybe.andThen toOutput) <| Just 0


printBoost : ( List Int, Maybe Int ) -> String
printBoost ( input, mo ) =
    let
        ip =
            List.map String.fromInt input |> String.join " "
    in
    case mo of
        Just o ->
            ip ++ ": " ++ String.fromInt o

        Nothing ->
            ip ++ ": " ++ "no output somewhere"


main : Html.Html msg
main =
    List.range 0 4
        |> permutations
        |> List.map (pairWithInput getTotalBoost)
        |> List.sortBy (Tuple.second >> Maybe.withDefault -99999999999)
        |> List.reverse
        |> List.map (printBoost >> H.text >> List.singleton >> H.li [])
        |> H.ul []
        |> H.toUnstyled
