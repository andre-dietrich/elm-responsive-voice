module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Responsive


type Msg
    = Update String
    | Speak
    | Cancel
    | ChangeVoice String
    | TTS (Result String String)


type alias Model =
    { text : String
    , voice : Maybe String
    , voice_list : List String
    , msg : String
    , speaking : Bool
    }


main : Program Never Model Msg
main =
    Html.program
        { update = update
        , init = init
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : ( Model, Cmd msg )
init =
    ( init_voices
        { text = "Enter some text in here ..."
        , voice = Nothing
        , voice_list = []
        , msg = "status: ok"
        , speaking = False
        }
    , Cmd.none
    )


init_voices : Model -> Model
init_voices model =
    case Responsive.getVoices () of
        Ok voice_list ->
            { model
                | voice_list = voice_list
                , voice = List.head voice_list
            }

        Err msg ->
            { model | msg = "error: " ++ msg }


view : Model -> Html Msg
view model =
    Html.div [ Attr.style [ ( "width", "400px" ) ] ]
        [ if model.speaking then
            Html.button
                [ Attr.style [ ( "width", "50%" ) ], onClick Cancel ]
                [ Html.text "Cancel!" ]
          else
            Html.button
                [ Attr.style [ ( "width", "50%" ) ], onClick Speak ]
                [ Html.text "Say It!" ]
        , Html.select
            [ Attr.style [ ( "width", "50%" ) ], onInput ChangeVoice ]
            (List.map
                (\v ->
                    Html.option [ Attr.value v ] [ Html.text v ]
                )
                model.voice_list
            )
        , Html.br [] []
        , Html.textarea
            [ Attr.style [ ( "width", "99%" ), ( "height", "100px" ) ]
            , Attr.value model.text
            , onInput Update
            ]
            []
        , Html.text model.msg
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Speak ->
            case model.voice of
                Just name ->
                    ( { model
                        | msg = "status: started speaking"
                        , speaking = True
                      }
                    , Responsive.speak TTS name model.text
                    )

                Nothing ->
                    ( { model
                        | msg = "error: no voice slected"
                        , speaking = False
                      }
                    , Cmd.none
                    )

        Cancel ->
            let
                rslt =
                    Responsive.cancel ()
            in
            update (TTS (Ok "")) model

        Update text ->
            ( { model | text = text }, Cmd.none )

        TTS (Result.Ok m) ->
            ( { model | msg = "status: ok", speaking = False }, Cmd.none )

        TTS (Result.Err m) ->
            ( { model | msg = "error: " ++ m, speaking = False }, Cmd.none )

        ChangeVoice name ->
            ( { model | voice = Just name }, Cmd.none )
