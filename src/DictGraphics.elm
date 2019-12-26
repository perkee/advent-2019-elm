module DictGraphics exposing
    ( Pixels
    , cautiouslyWrite
    , fromList
    , getFrom
    , toMatrix
    , write
    )

import Dict exposing (Dict)


type alias Pixels prePixel =
    Dict ( Int, Int ) prePixel


write : Pixels prePixel -> ( Int, Int, prePixel ) -> Pixels prePixel
write pixels ( x, y, p ) =
    Dict.insert ( x, y ) p pixels


fromList : List ( ( Int, Int ), prePixel ) -> Pixels prePixel
fromList =
    Dict.fromList


cautiouslyWrite : Pixels prePixel -> ( Int, Int, prePixel ) -> Pixels prePixel
cautiouslyWrite pixels ( x, y, p ) =
    if Dict.member ( x, y ) pixels then
        pixels

    else
        Dict.insert ( x, y ) p pixels


getFrom : Pixels prePixel -> ( Int, Int ) -> Maybe prePixel
getFrom pixels loc =
    Dict.get loc pixels


thrush : a -> (a -> b) -> b
thrush a fn =
    fn a


toMatrix : Pixels prePixel -> List (List (Maybe prePixel))
toMatrix pixels =
    let
        mMinx =
            Dict.keys pixels
                |> List.map Tuple.first
                |> List.minimum

        mMaxx =
            Dict.keys pixels
                |> List.map Tuple.first
                |> List.maximum

        mMiny =
            Dict.keys pixels
                |> List.map Tuple.second
                |> List.minimum

        mMaxy =
            Dict.keys pixels
                |> List.map Tuple.second
                |> List.maximum
    in
    case ( ( mMinx, mMaxx ), ( mMiny, mMaxy ) ) of
        ( ( Just minx, Just maxx ), ( Just miny, Just maxy ) ) ->
            let
                xs =
                    List.range minx maxx

                ys =
                    List.range miny maxy
            in
            List.map (Tuple.pair xs) ys
                |> List.map
                    ((Tuple.mapFirst <| List.map Tuple.pair)
                        >> (\( l, y ) -> List.map (thrush y) l)
                    )
                |> List.map (List.map (getFrom pixels))

        ( ( _, _ ), ( _, _ ) ) ->
            []
