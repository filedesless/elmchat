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
import Http
import Json.Decode
import Json.Encode
import List
import Platform.Sub
import WebSocket


apiBaseUrl : String
apiBaseUrl =
    "https://localhost:5001/api"


apiAuthUrl : String
apiAuthUrl =
    apiBaseUrl ++ "/auth"


wssBaseUrl : String
wssBaseUrl =
    "wss://localhost:5001/ws"


wssChatUrl : String
wssChatUrl =
    wssBaseUrl ++ "/chat"


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
    WebSocket.listen wssChatUrl ReceivedChatMsg


type alias Model =
    { content : String
    , messages : List String
    , username : String
    , password : String
    }


init : ( Model, Cmd Msg )
init =
    ( { content = ""
      , messages = []
      , username = ""
      , password = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NewContent String
    | NewUsername String
    | SubmitNewMessage
    | SubmitUsername
    | PostedUsername (Result Http.Error String)
    | ReceivedChatMsg String


postUser : Model -> Http.Request String
postUser model =
    let
        body =
            Json.Encode.string model.username
                |> Http.jsonBody
    in
    Http.post (apiBaseUrl ++ "/auth") body Json.Decode.string


postUserCmd : Model -> Cmd Msg
postUserCmd model =
    Http.send PostedUsername (postUser model)


postedUsername : Model -> Result Http.Error String -> ( Model, Cmd Msg )
postedUsername model result =
    case result of
        Ok newToken ->
            ( { model | password = newToken }
            , WebSocket.send wssChatUrl <|
                model.username
                    ++ ":"
                    ++ newToken
            )

        -- TODO: handle error
        Err error ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewContent newContent ->
            ( { model | content = newContent }, Cmd.none )

        NewUsername newUsername ->
            ( { model | username = newUsername }, Cmd.none )

        SubmitNewMessage ->
            if model.content /= "" then
                ( { model | content = "" }
                , WebSocket.send wssChatUrl model.content
                )

            else
                ( model, Cmd.none )

        SubmitUsername ->
            ( model
            , postUserCmd model
            )

        PostedUsername result ->
            postedUsername model result

        ReceivedChatMsg value ->
            ( { model | messages = value :: model.messages }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        loggedIn : Bool
        loggedIn =
            String.length model.password > 0
    in
    Grid.container [] <|
        [ CDN.stylesheet
        , Html.br [] []
        ]
            ++ (if loggedIn then
                    [ Form.form
                        [ Html.Events.onSubmit SubmitNewMessage
                        ]
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

                else
                    [ Form.form
                        [ Html.Events.onSubmit SubmitUsername
                        ]
                        [ Form.row []
                            [ Form.col []
                                [ Input.text
                                    [ Input.placeholder "Enter your nickname"
                                    , Input.value model.username
                                    , Input.onInput NewUsername
                                    , Input.attrs [ autofocus True ]
                                    ]
                                ]
                            , Form.col [ Col.smAuto ]
                                [ Button.button [ Button.primary ] [ text "Login" ]
                                ]
                            ]
                        ]
                    ]
               )
