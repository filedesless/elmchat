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
    , errors : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { content = ""
      , messages = []
      , username = ""
      , password = ""
      , errors = []
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
    | RemoveError Int
    | CloseModal


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
            ( { model | errors = ifThenElse (List.member err model.errors) model.errors <| err :: model.errors }
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

        ReceivedChatMsg value ->
            ( { model | messages = value :: model.messages }, Cmd.none )

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



-- VIEW


ifThenElse : Bool -> a -> a -> a
ifThenElse predicate x y =
    if predicate then
        x

    else
        y


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
                ]
            ]
        |> Modal.view (ifThenElse (loggedIn model) Modal.hidden Modal.shown)


displayBody : Model -> Html Msg
displayBody model =
    div []
        [ Form.form
            [ Html.Events.onSubmit SubmitNewMessage ]
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
                    [ Button.button [ Button.primary ] [ text "Submit" ]
                    ]
                ]
            ]
        , Html.br [] []
        , div [] <| List.map (\message -> p [] [ text message ]) model.messages
        ]


view : Model -> Html Msg
view model =
    Grid.container [] <|
        [ CDN.stylesheet
        , Html.br [] []
        , ifThenElse (loggedIn model) (displayErrors model) <| Html.span [] []
        , displayLoginForm model
        , displayBody model
        ]
