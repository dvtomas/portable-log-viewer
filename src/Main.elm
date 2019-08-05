import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, p, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Task
import Log exposing (Severity(..), Log, LogEntry, stringToLog, toString)

-- MAIN

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { log: Log, fileName: String }

init : () -> (Model, Cmd Msg)
init _ =
  ( {log = [], fileName = ""}, Cmd.none )

-- UPDATE

type Msg
  = LogRequested
  | LogSelected File
  | LogLoaded {fileName: String, content: String}


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LogRequested ->
      ( model
      , Select.file ["text"] LogSelected
      )

    LogSelected file ->
      ( model
      , Task.perform (\content -> LogLoaded {fileName = File.name file, content = content}) (File.toString file)
      )

    LogLoaded info ->
      ( { model | log = stringToLog info.content, fileName = info.fileName }
      , Cmd.none
      )

-- VIEW

toColor severity = case severity of
    Trace -> "Trace"
    Debug -> "Debug"
    Info -> "Info"
    Warn -> "Warn"
    Error -> "Error"

view : Model -> Html Msg
view model =
    let
        logRow entry = Html.tr [Html.Attributes.class (toString entry.severity |> String.toLower)] [
            (Html.td [] [text entry.timeString]),
            (Html.td [] [text (String.fromInt entry.time)]),
            (Html.td [] [text (String.fromInt entry.deltaTime)]),
            (Html.td [] [text (toString entry.severity)]),
            (Html.td [] [text entry.text]) ]
    in
        p [] [
            button [ onClick LogRequested ] [ text "Load Log" ],
            Html.h1 [] [text model.fileName],
            Html.table [] (List.map logRow model.log)
        ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none