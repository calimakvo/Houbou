module MediaUpload exposing (..)

import Browser
import Browser.Navigation as Nav
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as D
import Json.Decode.Pipeline as P
import List as L
import List.Extra as L
import Task as T
import Regex as R

-- MAIN
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Token =
    { key : String
    , val : String
    , maxlen : Int
    }

type alias Upresult =
    { result : Int
    , msg : String
    }

-- MODEL
type alias Model =
  { hover : Bool
  , files : List File
  , contents : List String
  , titles : List String
  , token : Token
  , err : Maybe String
  }

init : Token -> (Model, Cmd Msg)
init token =
  (Model False [] [] [] token Nothing, Cmd.none)

-- UPDATE
type Msg
  = Pick
  | DragEnter
  | DragLeave
  | ImageLoaded (List String)
  | GotFiles File (List File)
  | Upload
  | Input Int String 
  | Delete Int
  | HResp (Result Error Upresult)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Pick ->
        ( model
        , Select.files ["*/*"] GotFiles
        )

    DragEnter ->
        ( { model
              | hover = False
          }
        , Cmd.none
        )

    DragLeave ->
        ( { model
              | hover = False
          }
        , Cmd.none
        )

    GotFiles file files ->
        ( { model
              | files = file :: files
              , titles = L.repeat (L.length (file :: files)) ""
              , hover = False
          }
        , T.perform ImageLoaded (T.sequence (L.map (\f -> File.toUrl f) (file::files)))
        )

    ImageLoaded contents ->
        ( { model
              | contents = contents
          }
        , Cmd.none
        )

    Input idx str -> ({ model | titles = updateTitles idx str model.titles }
                     , Cmd.none
                     )

    Delete idx -> (
                   {
                     model | files = removeObject idx model.files
                   , contents = removeObject idx model.contents
                   , titles = removeObject idx model.titles
                   , err = Nothing
                   }
                  , Cmd.none )

    Upload ->
        ( model
        , Http.post
          {
            url = "/admin/medianew"
          , body = createMultiPart model
          , expect = Http.expectJson HResp respDecoder
          }
        )

    HResp (Ok res) ->
        if res.result == 1 then
            ( model, Nav.load "/admin/medialist/1" )
        else
            ( model, Nav.load "/admin/error" )

    HResp (Err error) ->
        case error of
            BadUrl url -> ( { model | err = Just ("URLが不正です " ++ url)}
                          , Cmd.none )
            Timeout -> ( { model | err = Just "タイムアウトしました" }
                       , Cmd.none )
            NetworkError -> ( { model | err = Just "ネットワークエラー" }
                            , Cmd.none )
            BadStatus n -> if n == 413 then
                               ( { model | err = (Just ("アップロードサイズが制限を越えているため削除してください")) }, Cmd.none )
                            else
                               ( { model | err = (Just ("ステータス不正 " ++ String.fromInt n)) }, Cmd.none )
            BadBody e -> ( { model | err = (Just ("エラー " ++ e)) }
                           , Cmd.none )

updateTitles : Int -> String -> List String -> List String
updateTitles idx str titles = 
    L.map (\(_, t) -> t)
        (L.map (\(index, n) -> if idx == index then (index, str) else (index, n))
             (L.indexedMap (\index title -> (index, title)) titles))

removeObject : Int -> List a -> List a
removeObject idx xs =
    L.map (\(_, t) -> t)
        (L.filter (\(index, _) -> if idx /= index then True else False)
             (L.indexedMap (\index o -> (index, o)) xs))

respDecoder : D.Decoder Upresult
respDecoder =
    D.succeed Upresult
        |> P.required "result" D.int
        |> P.required "msg" D.string

createMultiPart : Model -> Body
createMultiPart model =
    multipartBody (
    [
     stringPart model.token.key model.token.val
    ] ++ L.map (filePart "file") model.files
      ++ L.map (stringPart "media_title") model.titles)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW
view : Model -> Html Msg
view model =
  div
    [ class "col-lg-12" ]
    [
     div [ class "well well-lg"
         , hijackOn "dragenter" (D.succeed DragEnter)
         , hijackOn "dragover" (D.succeed DragEnter)
         , hijackOn "dragleave" (D.succeed DragLeave)
         , hijackOn "drop" dropDecoder
         ]
         [
           button [ class "btn btn-default", onClick Pick ] [ text "ファイル選択" ]
         , span [ class "dragandrop" ] [ text "ここへドロップ" ]
         ]
     ,
     case model.err of
         Just msg ->
             div [ class "alert alert-danger alert-dismissible" ]
             [
               button [ class "close"
                      , (attribute "data-dismiss" "alert")
                      , (attribute "aria-hidden" "true") ] [ text "×" ]
             , text msg
             ] 
         Nothing -> div [] []
     , submitBtn model
     , div
         [ class "row row-eq-height" ]
         (L.indexedMap
              (\idx (file, content, title) -> mediaMap (model.token.maxlen) idx file content title)
              (L.zip3 model.files model.contents model.titles))
     , submitBtn model
    ]

submitBtn : Model -> Html Msg
submitBtn model =
    if L.isEmpty model.files == True then
        div [] []
    else
        div [ class "well text-center" ]
            [
             button
                 [
                   class ("btn btn-primary" ++ btnDisabled model)
                 , onClick Upload
                 ] [ text "アップロード" ]
            ]

btnDisabled : Model -> String
btnDisabled model =
    if model.err /= Nothing then
        " disabled"
    else
        ""

mediaMap : Int -> Int -> File -> String -> String -> Html Msg
mediaMap maxlen idx file content title =
    div [ class "col-lg-3"] [
         div [ class "panel panel-info" ]
             [
               div [ class "panel-heading" ] [ text (File.name file) ]
             , div [ class "panel-body" ]
                 [
                   p [ class "inpimg" ]
                       [
                        if R.contains imgMimesRegex (File.mime file) == True then
                            img [
                             src content
                            , class "img-responsive img-thumbnail center-block"
                            , style "max-height" "300px"
                            ] [ ]
                        else
                            img [
                             src "../static/images/noimage.png"
                            , class "img-responsive img-thumbnail center-block"
                            , style "max-height" "300px"
                            ] [ ]
                       ]
                 , p [ ] [ text (adjustSize (File.size file)) ]
                 , input [
                         onInput (Input idx)
                       , class "form-control"
                       , placeholder "画像名"
                       , maxlength maxlen
                       , autofocus True
                       , value title
                       ] [ ]
                 ]
             , div [ class "panel-footer text-center" ]
                 [
                  button
                      [
                        class "btn btn-warning"
                      , onClick (Delete idx)
                      ] [ text "削除" ]
                 ]
             ]

         ]

imgMimesRegex : R.Regex
imgMimesRegex = Maybe.withDefault R.never <| R.fromString "image/*"

adjustSize : Int -> String
adjustSize size =
    if size < (1024 * 1024) then
        (String.fromInt (floor((toFloat size / 1024)))) ++ "KB"
    else if size >= 1024 * 1024 then
        (String.fromInt (floor(toFloat size / (1024 * 1024)))) ++ "MB"
    else
        (String.fromInt size) ++ "B"

dropDecoder : D.Decoder Msg
dropDecoder =
  D.at ["dataTransfer","files"] (D.oneOrMore GotFiles File.decoder)

hijackOn : String -> D.Decoder msg -> Attribute msg
hijackOn event decoder =
  preventDefaultOn event (D.map hijack decoder)

hijack : msg -> (msg, Bool)
hijack msg =
  (msg, True)
