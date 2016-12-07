module Papa exposing (Model, init, initModel, Msg, isGood, update, Context, subscriptions, view, SonId)

import Html exposing (..)
import Html.Attributes exposing (..)
import Son
import ChildUpdate exposing (updateMany)


type Msg
    = SonMsgWrap SonId Son.Msg
    | NoOp


type alias SonId =
    Son.Id


type Model =
    Model { sons : List Son.Model }


isGood : Model -> Bool
isGood (Model { sons }) =
    sons
        |> List.all Son.isHappy


init : (Model, Cmd Msg)
init =
    initModel ! []


initModel : Model
initModel =
    Model
        { sons =
            [ Son.initModel 1 "いちろう"
            , Son.initModel 2 "じろう"
            ]
        }


setSons : Model -> List Son.Model -> Model
setSons (Model model) sons =
    Model { model | sons = sons }


getSons : Model -> List Son.Model
getSons (Model { sons }) =
    sons


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SonMsgWrap sonId sonMsg ->
            updateMany Son.update Son.getId getSons setSons SonMsgWrap sonId sonMsg model
        
        _ ->
            model ! []


-- Son.Context msg : { msg : msg, isActive : Bool }
type alias Context msg =
    List (Son.Context msg)


subscriptions : Context msg -> Model -> Sub Msg
subscriptions sonContexts (Model { sons }) =
    List.map2 (\context son ->
        Son.subscriptions context son
            |> Sub.map (SonMsgWrap <| Son.getId son)
        ) sonContexts sons
        |> Sub.batch


papaImg : List ( String, String )
papaImg =
    [ ( "margin-top", "30px" )
    , ( "margin-left", "50px" )
    , ( "width", "180px" )
    , ( "height", "150px" )
    ]


view : Context msg -> Model -> Html msg
view sonContexts ((Model { sons }) as m) =
    let
        sonViews : List (Html msg)
        sonViews =
            List.map2 Son.view sonContexts sons

        papaImgSrc =
            if isGood m then
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


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions [{ msg = NoOp, isActive = True }, { msg = NoOp, isActive = False }]
        , view = view [{ msg = NoOp, isActive = True }, { msg = NoOp, isActive = False }]
        }
