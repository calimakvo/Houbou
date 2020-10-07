port module UrlCpy exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type alias UrlInfo =
    { url : String
    , elmId : String
    }

type Msg = Copy

type alias Model =
    { copy : Bool
    , info : UrlInfo
    }

init : UrlInfo -> (Model, Cmd Msg)
init info =
  (Model False info, Cmd.none)

main = 
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Copy -> (model, copy ())

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

port copy : () -> Cmd msg

-- VIEW
view : Model -> Html Msg
view model = div
             [
              class "form-group input-group"
             ]
             [
                input
                    [
                      type_ "text"
                    , class "form-control"
                    , id (inpId model.info.elmId)
                    , value model.info.url
                    ] []
              , span
                    [
                     class "input-group-btn"
                    ]
                    [
                     button
                         [
                          onClick Copy
                         , class "btn btn-default"
                         ]
                         [
                          i [ class "fa fa-clipboard" ] []
                         ]
                    ]
             ]

inpId : String -> String
inpId elmId = "cpy" ++ elmId
