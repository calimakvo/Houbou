module Category exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as D
import Json.Encode as E
import Json.Encode.Extra as E
import Json.Decode.Pipeline as P
import List as L

type alias CateInfo =
    { kw : String
    }

type alias Token =
    { key : String
    , val : String
    }

type Msg
    = Get (Result Http.Error CRes)
    | Send
    | Up Cate
    | Down Cate
    | HResp (Result Error CRes)

type NestCate = NestCate Cate

type alias Cate
    = { unCateId : Int
      , unCatePid : Maybe Int
      , unCateName : String
      , unCateVer : Int
      , unCatePos : Int
      , unCateList : List NestCate
      }

type alias CRes
    = { result : Int
      , cates : List Cate
      , err : Maybe String
      }

type alias Model
    = { result : Int
      , cates : List Cate
      , token : Token
      , successmsg : Maybe String
      , err : Maybe String
      }

type SwapSw =
    SwapUp | SwapDown

init : Token -> (Model, Cmd Msg)
init token =
  ({ result = 0
   , cates = []
   , token = token
   , successmsg = Nothing
   , err = Nothing }
   , Http.get
       { url = "/hb-admin/catelist"
       , expect = Http.expectJson Get respDecoder
       }
  )

encodeCate : Cate -> E.Value
encodeCate c = E.object [ ("type", E.string "Cate")
                        , ("unCateId", E.int c.unCateId)
                        , ("unCatePid", (E.maybe E.int c.unCatePid))
                        , ("unCateName", E.string c.unCateName)
                        , ("unCateVer", E.int c.unCateVer)
                        , ("unCatePos", E.int c.unCatePos)
                        , ("unCateList", encodeCateList (nctoc c.unCateList))
                        ]

encodeCateList : List Cate -> E.Value
encodeCateList cates = E.list encodeCate cates

main = 
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

swapCate : SwapSw -> Cate -> List Cate -> List Cate
swapCate sw c cs =
    case cs of
        c0::c1::xs ->
            if c.unCatePid /= c0.unCatePid then
                ({ c0 | unCateList = ctonc (swapCate sw c (nctoc c0.unCateList))}
                  :: { c1 | unCateList = ctonc (swapCate sw c (nctoc c1.unCateList))}
                  :: swapCate sw c xs)
            else
                case sw of
                    SwapUp ->
                        if c1.unCateId == c.unCateId then
                            c1::c0::xs
                        else
                            c0::(swapCate sw c (c1::xs))
                    SwapDown ->
                        if c0.unCateId == c.unCateId then
                            c1::c0::xs
                        else
                            c0::(swapCate sw c (c1::xs))
        c0::[] ->
            if c.unCatePid /= c0.unCatePid then
                ({ c0 | unCateList = ctonc (swapCate sw c (nctoc c0.unCateList))}::[])
            else
                [c0]
        _ -> cs

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Up c -> ({ model |
                   cates = swapCate SwapUp c model.cates
                 } , Cmd.none)

        Down c -> ({ model |
                   cates = swapCate SwapDown c model.cates
                 } , Cmd.none)

        Get (Ok res) ->
            ({ model |
                   result = res.result
                 , cates = res.cates
                 , err = res.err
             }, Cmd.none)

        Get (Err err) ->
            netErr model err

        Send ->
            ( model
            , Http.post
                { url = "/hb-admin/catelist"
                , body = createMultiPart model
                , expect = Http.expectJson HResp respDecoder
                }
            )

        HResp (Ok res) ->
            if res.result == 1 then
                ( { model |
                        cates = res.cates
                      , successmsg = Just "更新完了" }, Cmd.none)
            else
                ( model, Nav.load "/hb-admin/error" )

        HResp (Err err) ->
            netErr model err

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW
view : Model -> Html Msg
view model =
    case model.cates of
        [] ->
            div [ class "col-lg-8" ]
                [ chead
                , div [ class "alert alert-warning text-center"
                      , attribute "role" "alert"
                      ]
                      [ text "カテゴリの登録はありません"
                      ]
                ]
        _ ->
            div [ class "col-lg-8"
                ]
                [ dispMsg model
                , button [ class "btn btn btn-primary center-block"
                         , onClickNoDefault Send
                         ]
                         [ text "更新"
                         ]
                , chead
                , ul [ class "list-group"
                     ] <|
                    viewCate 1 model.cates -- Top Level
                , dispMsg model
                , button [ class "btn btn btn-primary center-block"
                         , onClickNoDefault Send
                         ]
                         [ text "更新"
                         ]
                ]

chead : Html Msg
chead =
    div [ style "margin-bottom" "5px"
        , class "text-right"
        ]
        [ a [ href "/hb-admin/catenew/0"
            , class "btn btn-primary btn-xs"
            ]
            [ text "追加"
            ]
        ]

dispMsg : Model -> Html Msg
dispMsg model =
    case model.successmsg of
        Just msg ->
            div [ class "alert alert-success text-center"
                , attribute "role" "alert"
                ]
                [ text msg
                ]
        Nothing ->
            case model.err of
                Just err ->
                    div [ class "alert alert-danger text-center"
                        , attribute "role" "alert"
                        ]
                        [ text err
                        ]
                Nothing -> text ""

viewCate : Int -> List Cate -> List (Html Msg)
viewCate depth cs = L.indexedMap (\idx -> innerCate depth idx (L.length cs)) cs

innerCate : Int -> Int -> Int -> Cate -> Html Msg
innerCate depth idx maxLen c =
    li [ class "list-group-item"
       ] <|
    [ div [ class "flex text-justify text-nowrap"
          ]
          [ span [ class "text-truncate"
                 ]
                 [ a [ href ("/catelist/" ++ intToStr c.unCateId)
                     , class "fa fa-external-link"
                     , attribute "target" "blank"
                     -- , attribute "data-toggle" "collapse"
                     ]
                     [ text c.unCateName
                     ]
                ]
          , span [
                 ] <| upMark idx c ++ downMark maxLen idx c ++ operateBtn depth c
          ]
    ] ++ case c.unCateList of
             [] -> []
             _ -> [ recursiveCate (depth + 1) c.unCateList ]

recursiveCate : Int -> List NestCate -> Html Msg
recursiveCate depth xs =
    div [ class "panel-collapse" -- class "panel-collapse collapse"
        , id ("cl" ++ intToStr depth)
        ]
        [ div [ class "panel-body"
              ]
              [ ul [ class "list-group"
                   ]
                   (viewCate depth (nctoc xs))
              ]
        ]

upMark : Int -> Cate -> List (Html Msg)
upMark idx c =
    if idx > 0 then
        [ a [ class "btn btn-primary btn-xs"
            , onClickNoDefault (Up c)
            , href "#"
            ]
            [ text "▲" ]
        , text " " ]
    else
        []

downMark : Int -> Int -> Cate -> List (Html Msg)
downMark maxLen idx c =
    if maxLen /= idx + 1 then
        [ a [ class "btn btn-primary btn-xs"
            , onClickNoDefault (Down c)
            , href "#"
            ]
            [ text "▼"
            ]
        , text " "
        ]
    else
        []

operateBtn : Int -> Cate -> List (Html Msg)
operateBtn depth c =
    [ a [ class "btn btn-info btn-xs"
        , href ("/hb-admin/catemod/" ++ (intToStr c.unCateId))
        ]
        [ text "編集"
        ]
    , text " "
    , if depth < 3 then
        a [ class "btn btn-primary btn-xs"
          , href ("/hb-admin/catenew/" ++ (intToStr c.unCateId))
          ]
          [ text "追加" ]
      else
        text "" -- 3階層
    , text " "
    , a [ class "btn btn-danger btn-xs"
        , href ("/hb-admin/catedel/" ++ (intToStr c.unCateId))
        ]
        [ text "削除"
        ]
    , text " "
    ]

createMultiPart : Model -> Body
createMultiPart model =
    multipartBody [ stringPart model.token.key model.token.val
                  , stringPart "cate_data" <| E.encode 0 <| encodeCateList model.cates
                  ]

onClickNoDefault : msg -> Attribute msg
onClickNoDefault message =
    let
        config =
            { stopPropagation = True
            , preventDefault = True
            , message = message
            }
    in
        custom "click" (D.succeed config)

nctoc : List NestCate -> List Cate
nctoc = L.map (\(NestCate f) -> f)

ctonc : List Cate -> List NestCate
ctonc = L.map NestCate

intToStr : Int -> String
intToStr = String.fromInt

respDecoder : D.Decoder CRes
respDecoder =
    D.map3 CRes
        (D.field "result" D.int)
        (D.field "ary" (D.list inCateDecoder))
        (D.field "err" (D.nullable  D.string))

inCateDecoder : D.Decoder Cate
inCateDecoder =
    D.map6 Cate
        (D.field "unCateId" D.int)
        (D.field "unCatePid" (D.nullable D.int))
        (D.field "unCateName" D.string)
        (D.field "unCateVer" D.int)
        (D.field "unCatePos" D.int)
        (D.field "unCateList" (D.list inNestCateDecoder))

inNestCateDecoder : D.Decoder NestCate
inNestCateDecoder =
    D.map NestCate (D.lazy (\_ -> inCateDecoder))

netErr : Model -> Error -> (Model, Cmd Msg)
netErr model err =
    case err of
        BadUrl url ->
            ({ model | err = Just ("URLが不正です " ++ url) }, Cmd.none )
        Timeout -> ( { model | err = Just "タイムアウトしました" }, Cmd.none )
        NetworkError -> ( { model | err = Just "ネットワークエラー" }, Cmd.none )
        BadStatus n ->
            if n == 413 then
                ( { model | err = (Just ("転送データ超過")) }, Cmd.none )
            else
                ( { model | err = (Just ("ステータス不正 " ++ String.fromInt n)) }, Cmd.none )
        BadBody m -> ( { model | err = (Just ("エラー " ++ m)) }, Cmd.none )
