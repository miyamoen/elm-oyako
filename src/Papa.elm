module Papa exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Keyboard
import Son


type Msg
    = SonMsgWrap Son.Msg
    | KeyDown Keyboard.KeyCode


type alias Model =
    { isGood : Bool, sonDict : Dict Son.Id Son.Model }


isGood : Dict Son.Id Son.Model -> Bool
isGood sons =
    sons
        |> Dict.toList
        |> List.map (\( id, son ) -> son)
        |> List.all (\son -> son.feeling == Son.Happy)


initModel =
    let
        sons =
            [ Son.initModel 1 "いちろう" Son.Angry
            , Son.initModel 2 "じろう" Son.Crying
            ]

        sonDict =
            sons
                |> List.map (\son -> ( son.id, son ))
                |> Dict.fromList
    in
        { isGood = isGood sonDict
        , sonDict = sonDict
        }


targetSon : Son.Id -> Dict Son.Id Son.Model -> Son.Model
targetSon id sonDict =
    sonDict
        |> Dict.get id
        |> Maybe.withDefault Son.dummySon


update : Son.Id -> Msg -> Model -> ( Model, Cmd Msg )
update id msg model =
    case msg of
        KeyDown code ->
            let
                ( sonModel, _ ) =
                    targetSon id model.sonDict
                        |> Son.update (Son.KeyDown code)

                newSonDict =
                    Dict.insert id sonModel model.sonDict

                newIsGood =
                    isGood newSonDict
            in
                { model | isGood = newIsGood, sonDict = newSonDict } ! []

        SonMsgWrap sonMsg ->
            model ! []


papaImg : List ( String, String )
papaImg =
    [ ( "margin-top", "30px" )
    , ( "margin-left", "50px" )
    , ( "width", "180px" )
    , ( "height", "150px" )
    ]


view : Son.Id -> Model -> Html Msg
view id model =
    let
        sonViews =
            model.sonDict
                |> Dict.toList
                |> List.map (\( id, son ) -> son)
                |> List.map (\son -> Html.map SonMsgWrap <| Son.view id son)

        papaImgSrc =
            if model.isGood then
                "../img/papa-good.png"
            else
                "../img/papa-bad.png"
    in
        div []
            [ img
                [ style papaImg
                , src papaImgSrc
                ]
                []
            , div [] sonViews
            ]
