module Four exposing (main)

import Browser
import Html.Styled as H exposing (Html)


type alias Digit =
    Int


type ParseState
    = PassesMonotonic Int
    | PassesPair Int
    | FailsMonotonic


stepThroughDigits : Digit -> ParseState -> ParseState
stepThroughDigits d state =
    case state of
        FailsMonotonic ->
            state

        PassesMonotonic prev ->
            case compare prev d of
                LT ->
                    PassesMonotonic d

                EQ ->
                    PassesPair d

                GT ->
                    FailsMonotonic

        PassesPair prev ->
            case compare prev d of
                LT ->
                    PassesPair d

                EQ ->
                    PassesPair d

                GT ->
                    FailsMonotonic


passesEverything : ParseState -> Bool
passesEverything state =
    case state of
        PassesPair _ ->
            True

        _ ->
            False


isValid : List Digit -> Bool
isValid digits =
    case digits of
        first :: rest ->
            rest
                |> List.foldl stepThroughDigits (PassesMonotonic first)
                >> passesEverything

        _ ->
            False


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


printString : List String -> String
printString s =
    (List.length s |> String.fromInt)
        ++ ": "
        ++ String.join ", " s


init : Model
init =
    List.range 271973 785961
        -- [ 271977, 288288, 3456789, 3456788 ]
        |> List.filter (intToDigits >> isValid)
        -- |> List.map (intToDigits >> (List.map String.fromInt >> String.join ","))
        |> List.map String.fromInt
        |> printString


type alias Model =
    String


view : Model -> Html ()
view =
    H.text


main =
    Browser.sandbox
        { init = init
        , update = \_ -> identity
        , view = view >> H.toUnstyled
        }
