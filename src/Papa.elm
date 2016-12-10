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


-- Son.Context : { isActive : Bool }
type alias Context =
    List (Son.Context)


subscriptions : Context -> Model -> Sub Msg
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


view : Context -> Model -> Html SonId
view sonContexts ((Model { sons }) as m) =
    let
        sonViews : List (Html SonId)
        sonViews =
            List.map2 (\context son ->
                Son.view context son
                    |> Html.map (always <| Son.getId son)
                ) sonContexts sons

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
        , subscriptions = subscriptions [{ isActive = True }, { isActive = False }]
        , view = view [{ isActive = True }, { isActive = False }] >> Html.map (always NoOp)
        }
