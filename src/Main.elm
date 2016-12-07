module Main exposing (..)

import Html exposing (..)
import Papa


type Msg
    = PapaMsgWrap Papa.Msg
    | ActivateOne Papa.SonId
    | NoOp


type alias Model =
    { papaModel : Papa.Model
    , papaContext : Papa.Context Msg
    }


init : ( Model, Cmd Msg )
init =
    { papaModel = Papa.initModel
    , papaContext = papaContext 1
    } ! []


papaContext : Papa.SonId -> Papa.Context Msg
papaContext id =
    case id of
        1 ->
            [ { msg = ActivateOne 1, isActive = True }
            , { msg = ActivateOne 2, isActive = False }
            ]

        2 ->
            [ { msg = ActivateOne 1, isActive = False }
            , { msg = ActivateOne 2, isActive = True }
            ]

        _ ->
            [ { msg = ActivateOne 1, isActive = False }
            , { msg = ActivateOne 2, isActive = False }
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PapaMsgWrap papaMsg ->
            let
                ( papaModel, _ ) =
                    Papa.update papaMsg model.papaModel
            in
                { model | papaModel = papaModel } ! []

        ActivateOne sonId ->
            { model | papaContext = papaContext sonId } ! []

        _ ->
            model ! []


subscriptions : Model -> Sub Msg
subscriptions { papaModel, papaContext } =
    Papa.subscriptions papaContext papaModel
        |> Sub.map PapaMsgWrap


view : Model -> Html Msg
view model =
    div [] [ Papa.view model.papaContext model.papaModel ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
