module One exposing (main)

import Browser
import Html as H exposing (Html, pre, text)
import Http



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Failure Http.Error
    | Loading
    | Success String


init : () -> ( Model, Cmd Msg )
init _ =
    update fakeMessage Loading


corsProhibitedCommand : Cmd Msg
corsProhibitedCommand =
    Http.get
        { url = "https://adventofcode.com/2019/day/1/input"
        , expect = Http.expectString GotText
        }


fakeMessage : Msg
fakeMessage =
    GotText <| Result.Ok <| """148623
147663
67990
108663
62204
140999
123277
52459
143331
71135
76282
69509
72977
120407
62278
136882
131667
146225
112216
108600
127267
149149
72977
149639
101527
70059
124825
69539
141444
64138
71145
68178
134752
79431
126342
134161
135424
95647
54507
104852
100164
118799
57387
93136
133359
144942
89337
60441
131825
93943
98142
108306
55355
115813
83431
125883
101115
107938
103484
61174
123502
73670
91619
136860
93268
149648
105328
53194
115351
130953
85611
71134
141663
106590
56437
147797
98484
60851
121252
66898
56502
103796
86497
121534
70914
122642
53151
131939
108394
128239
103490
122304
113810
141469
79176
108002
91942
84400
101217
116287"""



-- UPDATE


type Msg
    = GotText (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success fullText, Cmd.none )

                Err err ->
                    ( Failure err, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl s ->
            "BadUrl " ++ s

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus r ->
            "BadStatus " ++ String.fromInt r

        Http.BadBody badBody ->
            "BadBody: " ++ badBody


atLeast : Int -> Int -> Int
atLeast n i =
    clamp n i i


rocketStep : Int -> Int
rocketStep x =
    (x // 3) - 2 |> atLeast 0


rocket : List Int -> List Int
rocket ints =
    case ints of
        [] ->
            [ 0 ]

        first :: rest ->
            case first of
                0 ->
                    rest

                _ ->
                    rocketStep first :: first :: rest |> rocket


sumAndView : List Int -> Html msg
sumAndView =
    List.sum
        >> String.fromInt
        >> H.text
        >> List.singleton
        >> H.dd []


view : Model -> Html Msg
view model =
    case model of
        Failure err ->
            httpErrorToString err
                |> (++) "I was unable to launch the rocket: "
                |> text

        Loading ->
            text "Loading..."

        Success fullText ->
            let
                firstSteps =
                    fullText
                        |> String.split "\n"
                        |> List.filterMap
                            (String.trim
                                >> String.toInt
                                >> Maybe.map rocketStep
                            )
            in
            H.dl []
                [ H.dt [] [ text "first step" ]
                , firstSteps |> sumAndView
                , H.dt [] [ text "recursive step" ]
                , firstSteps |> List.map (List.singleton >> rocket >> List.sum) |> sumAndView
                ]
