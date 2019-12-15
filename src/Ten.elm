module Ten exposing (main)

import Html as H exposing (Html)
import Set exposing (Set)


filterMap : (comparable -> Maybe comparable1) -> Set comparable -> Set comparable1
filterMap fn =
    Set.toList >> List.filterMap fn >> Set.fromList


type CoprimeTree
    = CoprimeTree Int Int CoprimeTree CoprimeTree CoprimeTree
    | Leaf


input : List ( Int, Int )
input =
    """
    ....#...####.#.#...........#........
    #####..#.#.#......#####...#.#...#...
    ##.##..#.#.#.....#.....##.#.#..#....
    ...#..#...#.##........#..#.......#.#
    #...##...###...###..#...#.....#.....
    ##.......#.....#.........#.#....#.#.
    ..#...#.##.##.....#....##..#......#.
    ..###..##..#..#...#......##...#....#
    ##..##.....#...#.#...#......#.#.#..#
    ...###....#..#.#......#...#.......#.
    #....#...##.......#..#.......#..#...
    #...........#.....#.....#.#...#.##.#
    ###..#....####..#.###...#....#..#...
    ##....#.#..#.#......##.......#....#.
    ..#.#....#.#.#..#...#.##.##..#......
    ...#.....#......#.#.#.##.....#..###.
    ..#.#.###.......#..#.#....##.....#..
    .#.#.#...#..#.#..##.#..........#...#
    .....#.#.#...#..#..#...###.#...#.#..
    #..#..#.....#.##..##...##.#.....#...
    ....##....#.##...#..........#.##....
    ...#....###.#...##........##.##..##.
    #..#....#......#......###...........
    ##...#..#.##.##..##....#..#..##..#.#
    .#....#..##.....#.#............##...
    .###.........#....#.##.#..#.#..#.#..
    #...#..#...#.#.#.....#....#......###
    #...........##.#....#.##......#.#..#
    ....#...#..#...#.####...#.#..#.##...
    ......####.....#..#....#....#....#.#
    .##.#..###..####...#.......#.#....#.
    #.###....#....#..........#.....###.#
    ...#......#....##...##..#..#...###..
    ..#...###.###.........#.#..#.#..#...
    .#.#.............#.#....#...........
    ..#...#.###...##....##.#.#.#....#.#.
    """
        |> String.trim
        |> String.split "\n"
        |> List.map (String.trim >> String.split "")
        |> List.indexedMap processRow
        |> List.concat
        |> List.filterMap asteroidToPoint


triplet : a -> b -> c -> ( a, b, c )
triplet a b c =
    ( a, b, c )


asteroidToPoint : ( Int, Int, String ) -> Maybe ( Int, Int )
asteroidToPoint ( y, x, s ) =
    case s of
        "#" ->
            Just ( y, x )

        _ ->
            Nothing


processRow : Int -> List String -> List ( Int, Int, String )
processRow y =
    List.indexedMap (triplet y)


coprimesUnder : Int -> Int -> Int -> CoprimeTree
coprimesUnder max m n =
    if n > m then
        coprimesUnder max n m

    else if m > max then
        Leaf

    else
        CoprimeTree
            m
            n
            (coprimesUnder max (2 * m - n) m)
            (coprimesUnder max (2 * m + n) m)
            (coprimesUnder max (m + 2 * n) n)


toList : CoprimeTree -> List ( Int, Int )
toList t =
    case t of
        CoprimeTree m n left middle right ->
            (( m, n ) :: toList left) ++ toList middle ++ toList right

        Leaf ->
            []


starBurst : ( number, number ) -> List ( number, number )
starBurst ( rise, run ) =
    [ ( rise, run )
    , ( -rise, run )
    , ( -rise, -run )
    , ( rise, -run )
    , ( run, rise )
    , ( -run, rise )
    , ( -run, -rise )
    , ( run, -rise )
    ]


slopes : Set ( Int, Int )
slopes =
    ((coprimesUnder 36 2 1 |> toList) ++ (coprimesUnder 36 3 1 |> toList))
        |> (++) [ ( 1, 1 ), ( 0, 1 ) ]
        |> List.concatMap starBurst
        |> Set.fromList


projectionIsOutOfSpace : ( Int, Int ) -> Bool
projectionIsOutOfSpace ( y, x ) =
    (y < 0) || (y > 35) || (x < 0) || (x > 35)


type alias SlopeToPoint =
    { slope : ( Int, Int )
    , projection : ( Int, Int )
    }


thisSlopeHitsAnyPoint : Set ( Int, Int ) -> ( Int, Int ) -> ( Int, Int ) -> Maybe SlopeToPoint
thisSlopeHitsAnyPoint points ( y, x ) ( rise, run ) =
    let
        slope =
            ( rise, run )

        projection =
            ( y - rise, x + run )
    in
    if projectionIsOutOfSpace projection then
        Nothing

    else if Set.member projection points then
        Just <| SlopeToPoint slope projection

    else
        thisSlopeHitsAnyPoint points projection slope


whichSlopesHitFromHere : Set ( Int, Int ) -> Set ( Int, Int ) -> ( Int, Int ) -> ( Int, Int, List SlopeToPoint )
whichSlopesHitFromHere s points ( y, x ) =
    ( y
    , x
    , s |> Set.toList |> List.sortBy slopeToAngle |> List.filterMap (thisSlopeHitsAnyPoint points ( y, x ))
    )


third : ( a, b, c ) -> c
third ( _, _, c ) =
    c


slopeToAngle : ( Int, Int ) -> Float
slopeToAngle ( rise, run ) =
    atan2 (toFloat run) (toFloat rise) |> normalize


normalize : Float -> Float
normalize n =
    case ( n < 0, n > 2 * pi ) of
        ( True, _ ) ->
            normalize <| n + (2 * pi)

        ( _, True ) ->
            normalize <| n - (2 * pi)

        ( _, _ ) ->
            n


nthSlopeFromPoint : Int -> ( Int, Int, List SlopeToPoint ) -> Maybe SlopeToPoint
nthSlopeFromPoint n ( _, _, s ) =
    s
        |> List.drop (n - 1)
        |> List.head


main : Html Never
main =
    let
        pointsWithSlopes =
            List.map (whichSlopesHitFromHere slopes <| Set.fromList input) input |> List.sortBy (third >> List.length) |> List.reverse

        observationPoint =
            List.head pointsWithSlopes

        twoHundredth =
            observationPoint |> Maybe.andThen (nthSlopeFromPoint 200)
    in
    case ( observationPoint, twoHundredth ) of
        ( Just ( y, x, n ), Just { projection, slope } ) ->
            "From ("
                ++ String.fromInt x
                ++ ", "
                ++ String.fromInt y
                ++ ") you can see "
                ++ (String.fromInt <| List.length n)
                ++ " other asteroids. After blasting 200 times you vaporize ("
                ++ (String.fromInt <|
                        Tuple.second projection
                   )
                ++ ", "
                ++ (String.fromInt <|
                        Tuple.first projection
                   )
                ++ ") by following slope {"
                ++ (String.fromInt <| Tuple.first slope)
                ++ " / "
                ++ (String.fromInt <| Tuple.second slope)
                ++ "}"
                |> H.text

        ( _, _ ) ->
            "There is no good point?" |> H.text
