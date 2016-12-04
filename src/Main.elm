module Main exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Keyboard
import Papa
import Son


type Msg
    = PapaMsgWrap Papa.Msg


type alias Model =
    { activeSonId : Son.Id
    , papaModel : Papa.Model
    }


init : ( Model, Cmd Msg )
init =
    { activeSonId = 1, papaModel = Papa.initModel } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PapaMsgWrap papaMsg ->
            let
                ( papaModel, _ ) =
                    Papa.update model.activeSonId papaMsg model.papaModel
            in
                case papaMsg of
                    Papa.KeyDown code ->
                        { model | papaModel = papaModel } ! []

                    Papa.SonMsgWrap sonMsg ->
                        case sonMsg of
                            Son.ChangeActiveSon id ->
                                { model | activeSonId = id } ! []

                            _ ->
                                model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map PapaMsgWrap <| Keyboard.downs Papa.KeyDown


view : Model -> Html Msg
view model =
    div [] [ Html.map PapaMsgWrap <| Papa.view model.activeSonId model.papaModel ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
