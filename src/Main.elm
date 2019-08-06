module Main exposing (Model, Msg(..), init, main, update, view)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Browser
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode
import Json.Encode
import List
import Time


apiBaseUrl =
    "http://localhost:5000/api"


main =
    Browser.element
        { subscriptions = subscriptions
        , init = init
        , update = update
        , view = view
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 500 Tick



-- MODEL


type alias Model =
    { content : String
    , messages : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { content = ""
      , messages = []
      }
    , Http.get
        { url = apiBaseUrl ++ "/values"
        , expect = Http.expectJson GotValues <| Json.Decode.list Json.Decode.string
        }
    )



-- UPDATE


type Msg
    = NewContent String
    | SubmitNewMessage
    | GotValues (Result Http.Error (List String))
    | PostedValue (Result Http.Error ())
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewContent newContent ->
            ( { model | content = newContent }, Cmd.none )

        SubmitNewMessage ->
            if model.content /= "" then
                ( { model | messages = model.content :: model.messages, content = "" }
                , Http.post
                    { url = apiBaseUrl ++ "/values"
                    , body = Http.jsonBody <| Json.Encode.string model.content
                    , expect = Http.expectWhatever PostedValue
                    }
                )

            else
                ( model, Cmd.none )

        GotValues result ->
            case result of
                Ok values ->
                    ( { model | messages = values }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        PostedValue _ ->
            ( model, Cmd.none )

        Tick _ ->
            ( model
            , Http.get
                { url = apiBaseUrl ++ "/values"
                , expect = Http.expectJson GotValues <| Json.Decode.list Json.Decode.string
                }
            )



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
