module Son exposing (init, initModel, Msg, view, update, Model, Id, isHappy, dummySon, getId, Context, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard


type Msg
    = KeyDown Keyboard.KeyCode
    | NoOp


type alias Id =
    Int


type alias Name =
    String


type Feeling
    = Happy
    | Angry
    | Crying


type Model =
    Model
        { id : Id
        , name : Name
        , feeling : Feeling
        }


initModel : Id -> Name -> Model
initModel id name =
    Model
        { id = id
        , name = name
        , feeling = Happy
        }


init : ( Model, Cmd Msg)
init =
    dummySon ! []


dummySon : Model
dummySon =
    initModel 0 "dummy"


isHappy : Model -> Bool
isHappy (Model { feeling }) =
    feeling == Happy


getId : Model -> Id
getId (Model { id }) =
    id


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        KeyDown 38 ->
            Model { model | feeling = backFeeling model.feeling } ! []

        KeyDown 40 ->
            Model { model | feeling = forwardFeeling model.feeling } ! []

        _ ->
            Model model ! []


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


type alias Context msg =
    { msg : msg
    , isActive : Bool
    }


subscriptions : Context msg -> Model -> Sub Msg
subscriptions { isActive } (Model model) =
    if isActive then
        Keyboard.downs <| KeyDown
    else
        Sub.none


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


view : Context msg -> Model -> Html msg
view {msg, isActive} (Model model) =
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
            if isActive then
                sonNameColor
            else
                []
    in
        div
            [ style sonContainer
            , onClick msg
            ]
            [ img
                [ style sonImg, src sonImgSrc ]
                []
            , div
                [ style <| sonName ++ sonNameColorStyle ]
                [ text model.name ]
            ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions { msg = NoOp, isActive = True }
        , view = view { msg = NoOp, isActive = True }
        }
