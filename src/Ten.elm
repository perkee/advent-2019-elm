module Ten exposing (main)

import Browser
import Html as H exposing (Html)
import Set exposing (Set)


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
        -- """
        -- -#--#
        -- -----
        -- #####
        -- ----#
        -- ---##
        -- """
        -- [Log] slopes that hit from(3, 4): Set.fromList [(-4,1),(-2,-3),(-2,-1),(-2,1),(-1,-1),(-1,1)] (Ten.elm, line 523)
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


slopeLeavesSpaceFromThisPoint : ( Int, Int ) -> ( Int, Int ) -> Int -> Bool
slopeLeavesSpaceFromThisPoint ( y, x ) ( rise, run ) n =
    let
        dy =
            rise * n

        dx =
            run * n
    in
    ((y + dy) < 0) || ((y + dy) > 36) || ((x + dx) < 0) || ((x + dx) > 36)


thisSlopeHitsAnyPoint : Int -> Set ( Int, Int ) -> ( Int, Int ) -> ( Int, Int ) -> Bool
thisSlopeHitsAnyPoint n points ( y, x ) ( rise, run ) =
    if slopeLeavesSpaceFromThisPoint ( y, x ) ( rise, run ) n then
        False

    else
        Set.member ( y + rise * n, x + run * n ) points || thisSlopeHitsAnyPoint (n + 1) points ( y, x ) ( rise, run )


howManySlopesHitFromHere : Set ( Int, Int ) -> Set ( Int, Int ) -> ( Int, Int ) -> ( Int, Int, Int )
howManySlopesHitFromHere s points ( y, x ) =
    ( y
    , x
    , Set.filter (thisSlopeHitsAnyPoint 1 points ( y, x )) s
        |> Set.size
    )


third : ( a, b, c ) -> c
third ( _, _, c ) =
    c


main : Html Never
main =
    let
        pointsWithSlopes =
            List.map (howManySlopesHitFromHere slopes <| Set.fromList input) input |> List.sortBy third |> List.reverse
    in
    case List.head pointsWithSlopes of
        Just ( y, x, n ) ->
            "From (" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ") you can see " ++ String.fromInt n ++ " other asteroids." |> H.text

        Nothing ->
            "There is no good point?" |> H.text
