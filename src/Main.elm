module Main exposing (Model, Msg(..), init, main, update, view)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Modal as Modal
import Bootstrap.Utilities.Flex as Flex
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
    -- "https://localhost:5001/api"
    "https://api.filedesless.dev/api"


apiAuthUrl : String
apiAuthUrl =
    apiBaseUrl ++ "/auth"


wssBaseUrl : String
wssBaseUrl =
    -- "wss://localhost:5001/ws"
    "wss://api.filedesless.dev/ws"


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



-- Utilities


addErrorIfNew : Model -> String -> Model
addErrorIfNew model err =
    { model | errors = ifThenElse (List.member err model.errors) model.errors <| err :: model.errors }


handleHttpError : Http.Error -> String
handleHttpError error =
    case error of
        Http.BadUrl err ->
            "Provided a bad url, " ++ err

        Http.Timeout ->
            "The request timed out"

        Http.NetworkError ->
            "A network level error occured"

        Http.BadStatus { body } ->
            case Json.Decode.decodeString Json.Decode.string body of
                Ok s ->
                    s

                Err _ ->
                    toString body

        Http.BadPayload err response ->
            err ++ ": " ++ toString response


ifThenElse : Bool -> a -> a -> a
ifThenElse predicate x y =
    if predicate then
        x

    else
        y



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen wssChatUrl HandleWebSocketMessage


type alias Model =
    { content : String
    , messages : List String
    , username : String
    , password : String
    , errors : List String
    , users : List String
    }



-- TODO:
--   - get user list from websocket instead, to get dynamic updates
--       requires the server to allow guest connections, at least for reading
--   - fix autofocus on "submit messages" input


getUserList : Http.Request (List String)
getUserList =
    Http.get apiAuthUrl <| Json.Decode.list Json.Decode.string


getUserListCmd : Cmd Msg
getUserListCmd =
    Http.send GotUserList getUserList


gotUserList : Model -> Result Http.Error (List String) -> ( Model, Cmd Msg )
gotUserList model result =
    case result of
        Ok users ->
            ( { model | users = users }, Cmd.none )

        Err error ->
            let
                err =
                    handleHttpError error
            in
            ( addErrorIfNew model err, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( { content = ""
      , messages = []
      , username = ""
      , password = ""
      , errors = []
      , users = []
      }
    , getUserListCmd
    )



-- UPDATE


type Msg
    = NewContent String
    | NewUsername String
    | SubmitNewMessage
    | SubmitUsername
    | PostedUsername (Result Http.Error String)
    | HandleWebSocketMessage String
    | RemoveError Int
    | CloseModal
    | GotUserList (Result Http.Error (List String))


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

        Err error ->
            let
                err =
                    handleHttpError error
            in
            ( addErrorIfNew model err
            , Cmd.none
            )


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
            if String.trim model.username /= "" then
                ( model
                , postUserCmd model
                )

            else
                ( model, Cmd.none )

        PostedUsername result ->
            postedUsername model result

        HandleWebSocketMessage value ->
            handleWebSocketMessage model value

        RemoveError i ->
            let
                removeFromList : List a -> Int -> List a
                removeFromList l i =
                    case ( l, i ) of
                        ( [], _ ) ->
                            []

                        ( _ :: xs, 0 ) ->
                            xs

                        ( x :: xs, _ ) ->
                            x :: removeFromList xs (i - 1)
            in
            ( { model | errors = removeFromList model.errors i }, Cmd.none )

        CloseModal ->
            ( model, Cmd.none )

        GotUserList result ->
            gotUserList model result


handleWebSocketMessage : Model -> String -> ( Model, Cmd Msg )
handleWebSocketMessage model value =
    let
        msg =
            decodeWebSocketMessage value
    in
    case msg of
        WebSocketChatMessage { username, message } ->
            ( { model | messages = (username ++ ": " ++ message) :: model.messages }, Cmd.none )

        WebSocketConnectionMessage { username, left } ->
            ( { model | messages = ("*" ++ username ++ "* " ++ ifThenElse left "left" "joined" ++ " the chat") :: model.messages }, Cmd.none )

        WebSocketUnknownMessage err ->
            ( addErrorIfNew model <| "the websocket sent a response I couldn't understand: " ++ err, Cmd.none )


type WebSocketMessage
    = WebSocketChatMessage { username : String, message : String }
    | WebSocketConnectionMessage { username : String, left : Bool }
    | WebSocketUnknownMessage String


decodeWebSocketMessage : String -> WebSocketMessage
decodeWebSocketMessage value =
    let
        msgDecoder =
            Json.Decode.map WebSocketChatMessage <|
                Json.Decode.map2
                    (\username message -> { username = username, message = message })
                    (Json.Decode.field "username" Json.Decode.string)
                    (Json.Decode.field "message" Json.Decode.string)

        connDecoder =
            Json.Decode.map WebSocketConnectionMessage <|
                Json.Decode.map2 (\username left -> { username = username, left = left })
                    (Json.Decode.field "username" Json.Decode.string)
                    (Json.Decode.field "left" Json.Decode.bool)
    in
    case Json.Decode.decodeString (Json.Decode.oneOf [ msgDecoder, connDecoder ]) value of
        Ok msg ->
            msg

        Err _ ->
            WebSocketUnknownMessage value



-- VIEW


loggedIn : Model -> Bool
loggedIn model =
    String.length model.password > 0


displayErrors : Model -> Html Msg
displayErrors model =
    let
        displayError : Int -> String -> ListGroup.Item Msg
        displayError i s =
            ListGroup.li
                [ ListGroup.attrs [ Flex.block, Flex.justifyBetween, Flex.alignItemsCenter ]
                , ListGroup.danger
                ]
                [ text s
                , Html.span
                    [ class "close"
                    , property "innerHTML" (Json.Encode.string "&times;")
                    , onClick (RemoveError i)
                    ]
                    []
                ]
    in
    ListGroup.ul <|
        List.indexedMap displayError model.errors


displayUserList : Model -> Html Msg
displayUserList model =
    ListGroup.ul <|
        [ ListGroup.li [ ListGroup.primary ] [ Html.h4 [] [ text "Connected users" ] ] ]
            ++ List.map (\s -> ListGroup.li [] [ text s ]) model.users


displayLoginForm : Model -> Html Msg
displayLoginForm model =
    let
        validationError : Bool
        validationError =
            not (List.isEmpty model.errors) && not (loggedIn model)
    in
    Modal.config CloseModal
        |> Modal.large
        --|> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [ text "Please login" ]
        |> Modal.body []
            [ Form.form [ Html.Events.onSubmit SubmitUsername ]
                [ Form.row []
                    [ Form.col []
                        [ Input.text
                            [ Input.placeholder "Enter your nickname"
                            , Input.value model.username
                            , Input.onInput NewUsername
                            , Input.attrs [ autofocus (not (loggedIn model)) ]
                            ]
                        ]
                    , Form.col [ Col.smAuto ]
                        [ Button.button [ Button.primary ] [ text "Login" ] ]
                    ]
                , Form.row [] [ Form.col [] [ displayErrors model ] ]
                , Form.row []
                    [ Form.col []
                        [ ifThenElse (loggedIn model) (Html.span [] []) (displayUserList model) ]
                    ]
                ]
            ]
        |> Modal.view (ifThenElse (loggedIn model) Modal.hidden Modal.shown)


displayBody : Model -> Html Msg
displayBody model =
    div []
        [ ListGroup.ul
            ([ ListGroup.li [ ListGroup.primary ]
                [ Form.form [ Html.Events.onSubmit SubmitNewMessage ]
                    [ Form.row []
                        [ Form.col []
                            [ Input.text
                                [ Input.placeholder "Write a message"
                                , Input.value model.content
                                , Input.onInput NewContent
                                , Input.attrs [ autofocus (loggedIn model) ]
                                ]
                            ]
                        , Form.col [ Col.smAuto ]
                            [ Button.button [ Button.primary ] [ text "Submit" ] ]
                        ]
                    ]
                ]
             ]
                ++ List.map (\message -> ListGroup.li [] [ text message ]) model.messages
            )
        , Html.br [] []
        , ifThenElse (loggedIn model) (displayUserList model) (Html.span [] [])
        ]


view : Model -> Html Msg
view model =
    Grid.container [] <|
        [ CDN.stylesheet
        , Html.br [] []
        , ifThenElse (loggedIn model) (displayErrors model) <| Html.span [] []
        , Html.br [] []
        , displayLoginForm model
        , displayBody model
        ]
