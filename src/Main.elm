module Main exposing (Model, Msg(..), init, main, update, view)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List
import Platform.Sub
import WebSocket


apiBaseUrl : String
apiBaseUrl =
    "http://localhost:5000/api"


main : Program Never Model Msg
main =
    Html.program
        { subscriptions = subscriptions
        , init = init
        , update = update
        , view = view
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen "ws://echo.websocket.org" Echo


type alias Model =
    { content : String
    , messages : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { content = ""
      , messages = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NewContent String
    | SubmitNewMessage
    | Echo String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewContent newContent ->
            ( { model | content = newContent }, Cmd.none )

        SubmitNewMessage ->
            if model.content /= "" then
                ( { model | messages = ("Sent: " ++ model.content) :: model.messages, content = "" }
                , WebSocket.send "ws://echo.websocket.org" model.content
                )

            else
                ( model, Cmd.none )

        Echo value ->
            ( { model | messages = ("Received: " ++ value) :: model.messages }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , Html.br [] []
        , Form.form [ Html.Events.onSubmit SubmitNewMessage ]
            [ Form.row []
                [ Form.col []
                    [ Input.text
                        [ Input.placeholder "Write a message"
                        , Input.value model.content
                        , Input.onInput NewContent
                        , Input.attrs [ autofocus True ]
                        ]
                    ]
                , Form.col [ Col.smAuto ]
                    [ Button.button [ Button.primary ] [ text "Submit" ]
                    ]
                ]
            ]
        , Html.br [] []
        , div [] <| List.map (\message -> p [] [ text message ]) model.messages
        ]
