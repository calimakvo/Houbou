port module Preview exposing (..)

import Browser
import Browser.Events as BE
import Browser.Navigation as Nav
import Random
import Dialog
import Json.Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Http exposing (..)
import Json.Decode.Pipeline as P

type Msg
    = Open
    | Close
    | LoadText (Result Json.Decode.Error PrevParam)
    | GetNewHeight Int
    | RandVal Int
    | HResp (Result Error Upresult)

type alias Upresult =
    { result : Int
    , msg : String
    }

type alias PrevInfo =
    { key : String
    , val : String
    , height: Int
    }

type alias PrevParam =
    { title : String
    , body : String
    , css : String
    , inptype: Int
    , prevtype : String
    }
    
type alias Model =
    { showDialog : Bool
    , frameFlag : Bool
    , prevInfo : PrevInfo
    , height: Int
    , prevtype : String
    , randnum : Int
    , err : Maybe String
    }

-- Out ports
port clickNotice : () -> Cmd msg

-- In ports
port loadText : (E.Value -> msg) -> Sub msg

main : Program PrevInfo Model Msg
main =
    Browser.element
        { init = initialState
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
          loadText (D.decodeValue prevParamDecoder >> LoadText)
        , BE.onResize (\_ h -> GetNewHeight h)
        ]

initialState : PrevInfo -> ( Model, Cmd Msg )
initialState pInfo =
    ( { prevInfo = pInfo 
      , showDialog = False
      , frameFlag = False
      , height = calcHeight pInfo.height
      , prevtype = ""
      , randnum = 1
      , err = Nothing
      }
    , Cmd.none
    )

calcHeight : Int -> Int
calcHeight height =
    if height > 0 then
        round (toFloat(height) * 0.75)
    else
        600

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Open ->
            (model, clickNotice ())

        GetNewHeight h ->
                ( { model | height = calcHeight h
                  }, Cmd.none
                )

        LoadText result ->
            case result of
                Ok param ->
                    ( { model |
                        showDialog = True
                      , prevtype = param.prevtype
                      }
                    , Http.post
                        {
                          url = switchUrl param.prevtype
                        , body = createMultiPart model param
                        , expect = Http.expectJson HResp respDecoder
                        }
                    )
                Err err ->
                    ( { model | err = Just (D.errorToString err)
                      }
                    , Debug.todo (D.errorToString err))

        HResp (Ok res) ->
            if res.result == 1 then
                ( { model | frameFlag = True }, Random.generate RandVal (Random.int 1 1000000) )
            else
                ( model, Nav.load "/hb-admin/error" )

        RandVal num ->
            ( { model | randnum = num}, Cmd.none )

        HResp (Err error) ->
            case error of
                BadUrl url -> ( { model | err = Just ("URLが不正です " ++ url)}
                              , Cmd.none )
                Timeout -> ( { model | err = Just "タイムアウトしました" }
                           , Cmd.none )
                NetworkError -> ( { model | err = Just "ネットワークエラー" }
                                , Cmd.none )
                BadStatus n -> if n == 413 then
                                   ( { model | err = (Just ("転送データ超過")) }, Cmd.none )
                                else
                                   ( { model | err = (Just ("ステータス不正 " ++ String.fromInt n)) }, Cmd.none )
                BadBody e -> ( { model | err = (Just ("エラー " ++ e)) }
                               , Cmd.none )

        Close ->
            ( { model | showDialog = False, frameFlag = False }
            , Cmd.none
            )

view : Model -> Html Msg
view model =
    span [ ]
        [ button
              [ class "btn btn-default btn-sm"
              , onClickNoDefault Open
              ]
              [ text "プレビュー" ]
        , Dialog.view
            (if model.showDialog then
                 Just (dialogConfig model)
             else
                 Nothing
            )
        ]

dialogConfig : Model -> Dialog.Config Msg
dialogConfig model =
    { closeMessage = Just Close
    , containerClass = Nothing
    , header = Just (text "プレビュー")
    , body = showPreview model
    , footer =
          [ button
                [ class "btn btn-default btn-block"
                , onClickNoDefault Close
                ]
                [ text "閉じる" ]
          ]
    }

showPreview : Model -> Maybe (Html msg)
showPreview model =
    if model.frameFlag == True then
        Just (iframe [ id "prevFrameId"
                     , src ((switchUrl model.prevtype) ++ ("?n=") ++ (String.fromInt model.randnum))
                     , attribute "width" "100%"
                     , attribute "height" ((String.fromInt model.height) ++ "px")
                     , style "border" "1px gray solid"
                     ] [])
    else
        Nothing

onClickNoDefault : msg -> Attribute msg
onClickNoDefault message =
    let
        config =
            { stopPropagation = True
            , preventDefault = True
            , message = message
            }
    in
        custom "click" (Json.Decode.succeed config)

createMultiPart : Model -> PrevParam -> Body
createMultiPart model param =
    multipartBody
    [
      stringPart model.prevInfo.key model.prevInfo.val
    , stringPart "preview_title" param.title
    , stringPart "preview_body" param.body
    , stringPart "preview_css" param.css
    , stringPart "input_type" (String.fromInt param.inptype)
    , stringPart "preview_type" param.prevtype
    ] 

respDecoder : D.Decoder Upresult
respDecoder =
    D.succeed Upresult
        |> P.required "result" D.int
        |> P.required "msg" D.string

prevParamDecoder : D.Decoder PrevParam
prevParamDecoder =
    D.map5 PrevParam
      (D.field "title" D.string)
      (D.field "body" D.string)
      (D.field "css" D.string)
      (D.field "inptype" D.int)
      (D.field "prevtype" D.string)

switchUrl : String -> String
switchUrl ptype =
    case ptype of
        "post" -> "/hb-admin/postprev"
        "free" -> "/hb-admin/freeprev"
        _ -> "/hb-admin/error"
