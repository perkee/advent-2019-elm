module Twelve exposing (main)

import Browser
import Html as H exposing (Html)
import Set exposing (Set)


type alias Vector =
    { x : Int
    , y : Int
    , z : Int
    }


zero : Vector
zero =
    Vector 0 0 0


type alias Moon =
    { position : Vector
    , velocity : Vector
    }


axialGravity : Moon -> (Vector -> Int) -> List Moon -> Int
axialGravity moon getter =
    List.foldl
        (\m g ->
            case
                compare
                    (moon |> .position |> getter)
                    (m |> .position |> getter)
            of
                LT ->
                    g + 1

                EQ ->
                    g

                GT ->
                    g - 1
        )
        0


gravity : Moon -> List Moon -> Vector
gravity moon moons =
    { x = axialGravity moon .x moons
    , y = axialGravity moon .y moons
    , z = axialGravity moon .z moons
    }


addVectors : Vector -> Vector -> Vector
addVectors v1 v2 =
    { x = v1.x + v2.x
    , y = v1.y + v2.y
    , z = v1.z + v2.z
    }


stepMoon : List Moon -> Moon -> Moon
stepMoon moons moon =
    let
        velocity =
            addVectors moon.velocity <| gravity moon moons

        position =
            addVectors moon.position velocity
    in
    { position = addVectors moon.position velocity
    , velocity = velocity
    }


stepMoons : List Moon -> List Moon
stepMoons moons =
    List.map (stepMoon moons) moons


initial : List Moon
initial =
    [ Moon { x = 0, y = 6, z = 1 } zero
    , Moon { x = 4, y = 4, z = 19 } zero
    , Moon { x = -11, y = 1, z = 8 } zero
    , Moon { x = 2, y = 19, z = 15 } zero
    ]


type alias Model =
    List (List Moon)


init : Model
init =
    [ initial ]


energy : Moon -> Int
energy m =
    ([ m.position.x, m.position.y, m.position.z ] |> List.map abs |> List.sum)
        * ([ m.velocity.x, m.velocity.y, m.velocity.z ] |> List.map abs |> List.sum)


generate : Int -> List (List Moon) -> List (List Moon)
generate n states =
    case ( n <= 0, List.head states ) of
        ( True, _ ) ->
            states

        ( False, Just state ) ->
            generate (n - 1) <| stepMoons state :: states

        ( False, Nothing ) ->
            states


partOne : String
partOne =
    generate 1000 init
        |> List.head
        |> Maybe.withDefault []
        |> List.map energy
        |> List.sum
        |> String.fromInt
        |> (++) "1000 step energy: "


type alias Serialized =
    ( ( Int, Int, Int ), ( Int, Int, Int ) )


serialize : Moon -> Serialized
serialize m =
    ( ( m.position.x, m.position.y, m.position.z )
    , ( m.velocity.x, m.velocity.y, m.velocity.z )
    )


repeatTime : Set (List Serialized) -> List Moon -> String
repeatTime states state =
    let
        t =
            List.map serialize state
    in
    if Set.member t states then
        Set.size states + 1 |> String.fromInt |> (++) "repeats after "

    else
        repeatTime (Set.insert t states) <| stepMoons state


main : Html Never
main =
    H.div []
        [ H.div [] [ partOne |> H.text ]

        -- , H.div [] [ repeatTime Set.empty initial |> H.text ]
        ]


view : Model -> Html ()
view model =
    partOne |> H.text


old =
    Browser.sandbox
        { init = init
        , update = \_ -> identity
        , view = view
        }
