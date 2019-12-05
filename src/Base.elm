module Four exposing (main)

import Browser
import Html.Styled as H exposing (Html)


type alias Model =
    List Int


init : Model
init =
    []


view : Model -> Html ()
view model =
    H.text "nothin'"


main =
    Browser.sandbox
        { init = init
        , update = \_ -> identity
        , view = view >> H.toUnstyled
        }
