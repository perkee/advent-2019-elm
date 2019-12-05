module TwoOne exposing (main)

import Array exposing (Array)
import Browser
import Css
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Html.Styled.Lazy as Lazy
import Time


input : String
input =
    "1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,6,19,1,5,19,23,1,23,6,27,1,5,27,31,1,31,6,35,1,9,35,39,2,10,39,43,1,43,6,47,2,6,47,51,1,5,51,55,1,55,13,59,1,59,10,63,2,10,63,67,1,9,67,71,2,6,71,75,1,5,75,79,2,79,13,83,1,83,5,87,1,87,9,91,1,5,91,95,1,5,95,99,1,99,13,103,1,10,103,107,1,107,9,111,1,6,111,115,2,115,13,119,1,10,119,123,2,123,6,127,1,5,127,131,1,5,131,135,1,135,6,139,2,139,10,143,2,143,9,147,1,147,6,151,1,151,13,155,2,155,9,159,1,6,159,163,1,5,163,167,1,5,167,171,1,10,171,175,1,13,175,179,1,179,2,183,1,9,183,0,99,2,14,0,0"


possibilitities : Int -> List Int
possibilitities n =
    let
        start =
            n * 1000
    in
    List.range start (start + 999)


firstArray : Int -> Int -> Array Cell
firstArray noun verb =
    1
        :: noun
        :: verb
        :: (String.split "," "3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,6,19,1,5,19,23,1,23,6,27,1,5,27,31,1,31,6,35,1,9,35,39,2,10,39,43,1,43,6,47,2,6,47,51,1,5,51,55,1,55,13,59,1,59,10,63,2,10,63,67,1,9,67,71,2,6,71,75,1,5,75,79,2,79,13,83,1,83,5,87,1,87,9,91,1,5,91,95,1,5,95,99,1,99,13,103,1,10,103,107,1,107,9,111,1,6,111,115,2,115,13,119,1,10,119,123,2,123,6,127,1,5,127,131,1,5,131,135,1,135,6,139,2,139,10,143,2,143,9,147,1,147,6,151,1,151,13,155,2,155,9,159,1,6,159,163,1,5,163,167,1,5,167,171,1,10,171,175,1,13,175,179,1,179,2,183,1,9,183,0,99,2,14,0,0"
                |> List.filterMap String.toInt
           )
        |> List.map Plain
        |> Array.fromList


type alias Model =
    { point : Int
    , tape : Array Cell
    , state : State
    , inputs : ( Int, Int )
    }


type Cell
    = Plain Int
    | JustWrittenTo Int Cell -- current value, old value


initialModel : Int -> Int -> Model
initialModel noun verb =
    { point = 0
    , tape = firstArray noun verb --String.split "," input |> List.filterMap (String.toInt >> Maybe.map Plain) |> Array.fromList
    , state = Start
    , inputs = ( noun, verb )
    }


cellToInt : Cell -> Int
cellToInt c =
    case c of
        JustWrittenTo current _ ->
            current

        Plain current ->
            current


flattenCell : Cell -> Cell
flattenCell =
    cellToInt >> Plain


type Msg
    = Go Time.Posix


type State
    = Start
    | JustRead Op
    | JustComputed Int Int -- Addr, Value,
    | JustWrote
    | Done Int
    | Error String


type Op
    = Add Int Int Int -- left addr, right addr, dest addr
    | Mul Int Int Int -- left addr, right addr, dest addr


read : Model -> Model
read model =
    case Array.get model.point model.tape |> Maybe.map cellToInt of
        Just 99 ->
            case Array.get 0 model.tape of
                Just val ->
                    { model | state = Done <| cellToInt val }

                Nothing ->
                    { model | state = Error "Done but could not get value at 0" }

        Just 1 ->
            case
                ( Array.get (model.point + 1) model.tape
                , Array.get (model.point + 2) model.tape
                , Array.get (model.point + 3) model.tape
                )
            of
                ( Just leftAddr, Just rightAddr, Just destAddr ) ->
                    { model
                        | state =
                            JustRead <|
                                Add (cellToInt leftAddr)
                                    (cellToInt rightAddr)
                                    (cellToInt destAddr)
                    }

                ( _, _, _ ) ->
                    { model
                        | state =
                            Error <|
                                "could not get three values after point"
                                    ++ String.fromInt model.point
                    }

        Just 2 ->
            case
                ( Array.get (model.point + 1) model.tape
                , Array.get (model.point + 2) model.tape
                , Array.get (model.point + 3) model.tape
                )
            of
                ( Just leftAddr, Just rightAddr, Just destAddr ) ->
                    { model
                        | state =
                            JustRead <|
                                Mul (cellToInt leftAddr)
                                    (cellToInt rightAddr)
                                    (cellToInt destAddr)
                    }

                ( _, _, _ ) ->
                    { model
                        | state =
                            Error <|
                                "could not get three values after point"
                                    ++ String.fromInt model.point
                    }

        Just n ->
            { model
                | state = Error <| "unknown op code " ++ String.fromInt n
            }

        Nothing ->
            { model
                | state = Error <| "point" ++ String.fromInt model.point ++ " is out of the tape"
            }


math : String -> (Int -> Int -> Int) -> Maybe Cell -> Maybe Cell -> Int -> State
math errstring op left right dest =
    case
        ( Maybe.map cellToInt left, Maybe.map cellToInt right )
    of
        ( Just lv, Just rv ) ->
            JustComputed dest <| op lv rv

        ( _, _ ) ->
            Error errstring


update : Msg -> Model -> ( Model, Cmd msg )
update _ model =
    ( case model.state of
        Start ->
            read model

        JustWrote ->
            read model

        JustRead op ->
            case op of
                Add la ra dest ->
                    { model
                        | state =
                            math
                                (printNumbers "could not read values to add from addresses" [ la, ra ])
                                (+)
                                (Array.get la model.tape)
                                (Array.get ra model.tape)
                                dest
                    }

                Mul la ra dest ->
                    { model
                        | state =
                            math
                                (printNumbers "could not read values to multiply from addresses" [ la, ra ])
                                (*)
                                (Array.get la model.tape)
                                (Array.get ra model.tape)
                                dest
                    }

        JustComputed addr value ->
            case Array.get addr model.tape of
                Just old ->
                    { model
                        | tape = Array.set addr (JustWrittenTo value old) <| Array.map flattenCell model.tape
                        , point = model.point + 4
                        , state = JustWrote
                    }

                Nothing ->
                    { model
                        | state = Error <| "could write but cannot remember the old value at " ++ String.fromInt addr
                    }

        _ ->
            model
    , Cmd.none
    )


type CellAt
    = OpCode
    | Arg
    | Dest
    | Rest


intToCellAt : Int -> CellAt
intToCellAt n =
    case n of
        0 ->
            OpCode

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
                    OpCode ->
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


printNumbers : String -> List Int -> String
printNumbers label nums =
    label :: (nums |> List.map String.fromInt) |> String.join " "


showThreeNumbers : String -> Int -> Int -> Int -> Html msg
showThreeNumbers label a b c =
    label :: ([ a, b, c ] |> List.map String.fromInt) |> String.join " " |> H.text


viewState : State -> Html msg
viewState s =
    case s of
        Start ->
            H.text "Start"

        JustRead op ->
            case op of
                Add a b c ->
                    showThreeNumbers "add" a b c

                Mul a b c ->
                    showThreeNumbers "add" a b c

        JustComputed addr value ->
            H.text <| String.fromInt addr ++ " <= " ++ String.fromInt value

        Error errString ->
            H.text <| "error " ++ errString

        JustWrote ->
            H.text "wrote"

        Done val ->
            H.text <| "done: " ++ String.fromInt val


toDouble : a -> b -> ( a, b )
toDouble a b =
    ( a, b )


compose2 : (a -> b -> c) -> (c -> d) -> a -> b -> d
compose2 first second a =
    first a >> second


viewProcessor : ( List Cell, ( Int, Cell ) -> Html Msg ) -> Html Msg
viewProcessor ( tape, viewCellForPoint ) =
    tape
        |> List.indexedMap (compose2 toDouble (Lazy.lazy viewCellForPoint))
        |> H.div [ HA.class "tape" ]


stateCanContinue : State -> Bool
stateCanContinue s =
    case s of
        Done _ ->
            True

        Error _ ->
            True

        _ ->
            False


modelsCanContinue : List Model -> Bool
modelsCanContinue =
    List.any (.state >> stateCanContinue)


viewMany : List Model -> Html Msg
viewMany models =
    H.div [] <|
        List.map view models


view : Model -> Html Msg
view model =
    let
        viewCell_ =
            viewCell model.point

        ( noun, verb ) =
            model.inputs
    in
    H.div
        [ HA.css
            [ Css.float Css.left
            ]
        ]
        [ H.span [] [ H.text <| "(" ++ String.fromInt noun ++ ", " ++ String.fromInt verb ++ ") " ]
        , H.span [] [ viewState model.state ]
        , Lazy.lazy viewProcessor ( Array.toList model.tape, viewCell_ )
        ]


makeModel : Int -> Model
makeModel n =
    initialModel (n // 100) (remainderBy 100 n)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 100 Go


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( makeModel 1202, Cmd.none )
        , view = view >> H.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
