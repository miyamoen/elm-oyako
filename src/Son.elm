module Son exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard
import Task


type Msg
    = ChangeActiveSon Id
    | KeyDown Keyboard.KeyCode


type alias Id =
    Int


type alias Name =
    String


type Feeling
    = Happy
    | Angry
    | Crying


type alias Model =
    { id : Id
    , name : Name
    , feeling : Feeling
    }


initModel : Id -> Name -> Feeling -> Model
initModel id name feeling =
    { id = id
    , name = name
    , feeling = feeling
    }


dummySon : Model
dummySon =
    initModel 0 "" Happy


message : msg -> Cmd msg
message x =
    Task.perform identity (Task.succeed x)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown code ->
            let
                newFeeling =
                    case code of
                        38 ->
                            backFeeling model.feeling

                        40 ->
                            forwardFeeling model.feeling

                        _ ->
                            model.feeling
            in
                { model | feeling = newFeeling } ! []

        _ ->
            model ! []


backFeeling : Feeling -> Feeling
backFeeling feeling =
    case feeling of
        Happy ->
            Crying

        Angry ->
            Happy

        Crying ->
            Angry


forwardFeeling : Feeling -> Feeling
forwardFeeling feeling =
    case feeling of
        Happy ->
            Angry

        Angry ->
            Crying

        Crying ->
            Happy


sonContainer : List ( String, String )
sonContainer =
    [ ( "margin-left", "20px" )
    , ( "margin-top", "20px" )
    , ( "float", "left" )
    ]


sonImg : List ( String, String )
sonImg =
    [ ( "width", "120px" )
    , ( "height", "100px" )
    ]


sonName : List ( String, String )
sonName =
    [ ( "margin-left", "30px" )
    ]


sonNameColor : List ( String, String )
sonNameColor =
    [ ( "color", "red" ) ]


view : Id -> Model -> Html Msg
view id model =
    let
        sonImgSrc =
            case model.feeling of
                Happy ->
                    "../img/son-happy.png"

                Angry ->
                    "../img/son-angry.png"

                Crying ->
                    "../img/son-crying.png"

        sonNameColorStyle =
            if id == model.id then
                sonNameColor
            else
                []
    in
        div
            [ style sonContainer
            , onClick <| ChangeActiveSon model.id
            ]
            [ img
                [ style sonImg, src sonImgSrc ]
                []
            , div
                [ style <| sonName ++ sonNameColorStyle ]
                [ text model.name ]
            ]
