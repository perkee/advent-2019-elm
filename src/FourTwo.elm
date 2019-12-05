module FourTwo exposing (main)

import Browser
import Dict exposing (Dict)
import Html.Styled as H exposing (Html)


type alias Digit =
    Int


type alias Solution =
    { partOne : List String
    , partTwo : List String
    }


monotonic : comparable -> List comparable -> Bool
monotonic first rest =
    case rest of
        [] ->
            True

        next :: tail ->
            case compare first next of
                LT ->
                    monotonic next tail

                EQ ->
                    monotonic next tail

                GT ->
                    False


monotonicDigits : Int -> Maybe (List Int)
monotonicDigits l =
    let
        digits =
            intToDigits l
    in
    case digits of
        first :: next ->
            if monotonic first next then
                Just digits

            else
                Nothing

        _ ->
            Nothing


count : comparable -> Dict comparable Int -> Dict comparable Int
count val reduction =
    Dict.insert val
        (case Dict.get val reduction of
            Just n ->
                n + 1

            Nothing ->
                1
        )
        reduction


debugInt : Int -> Digit -> String -> String
debugInt int current label =
    "Failed "
        ++ String.fromInt int
        ++ " on "
        ++ String.fromInt current
        ++ "due to "
        ++ label


digitCounts : List comparable -> List Int
digitCounts digits =
    digits
        |> List.foldl count Dict.empty
        |> Dict.values


accumulateSolutions : Int -> Solution -> Solution
accumulateSolutions num soFar =
    case monotonicDigits num of
        Just digits ->
            let
                counts =
                    digitCounts digits
            in
            { partOne =
                if List.any ((<=) 2) counts then
                    String.fromInt num :: soFar.partOne

                else
                    soFar.partOne
            , partTwo =
                if List.any ((==) 2) counts then
                    String.fromInt num :: soFar.partTwo

                else
                    soFar.partTwo
            }

        Nothing ->
            soFar


intToDigitsRecurse : List Digit -> Int -> List Digit
intToDigitsRecurse soFar rest =
    case ( rest, soFar ) of
        ( 0, [] ) ->
            [ 0 ]

        ( 0, _ ) ->
            soFar

        ( _, _ ) ->
            intToDigitsRecurse
                (remainderBy 10 rest :: soFar)
                (rest // 10)


intToDigits : Int -> List Digit
intToDigits =
    intToDigitsRecurse []


printString : List String -> Html ()
printString s =
    (List.length s |> String.fromInt)
        ++ ": "
        ++ String.join ", " s
        |> H.text


init : Model
init =
    List.range 271973 785961
        -- [ 271977, 288288, 345789, 345677, 345778, 345777 ]
        |> List.foldl accumulateSolutions { partOne = [], partTwo = [] }


type alias Model =
    Solution


view : Model -> Html ()
view model =
    H.div []
        [ H.p []
            [ "part 1: "
                ++ (model.partOne |> List.length |> String.fromInt)
                ++ " and part 2: "
                ++ (model.partTwo |> List.length |> String.fromInt)
                |> H.text
            ]
        , H.p []
            [ model.partOne |> printString ]
        , H.p
            []
            [ model.partTwo |> printString ]
        ]


main =
    Browser.sandbox
        { init = init
        , update = \_ -> identity
        , view = view >> H.toUnstyled
        }
