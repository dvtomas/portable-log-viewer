import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Task
import Log
import Filter
import Keyboard
import Keyboard exposing (Key(..))

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

type alias Model = {
    log: Log.Log,
    controlPanelMinimized: Bool,
    fileName: String,
    filters: Filter.Model
    }

init : () -> (Model, Cmd Msg)
init _ = (
        {
            log = [],
            controlPanelMinimized = False,
            fileName = "",
            filters = Filter.emptyModel
        },
        Cmd.none
    )

-- UPDATE

type Msg
  = LogRequested
  | LogSelected File
  | LogLoaded {fileName: String, content: String}
  | KeyDown Keyboard.RawKey
  | MinimizeControlPanel
  | MaximizeControlPanel
  | FilterMsg Filter.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LogRequested -> (
            model,
            Select.file ["text"] LogSelected
        )
    KeyDown rawKey ->
        case Keyboard.anyKeyOriginal rawKey of
            Just key ->
                if key == F2 then
                    ({model | controlPanelMinimized = not model.controlPanelMinimized}, Cmd.none)
                else if key == F4 then
                    (model, Select.file ["text"] LogSelected)
                else
                    (model, Cmd.none)
            Nothing -> (model, Cmd.none)
    LogSelected file -> (
            model,
            Task.perform (\content -> LogLoaded {fileName = File.name file, content = content}) (File.toString file)
        )
    LogLoaded info -> (
        { model | log = Log.stringToLog info.content, fileName = info.fileName },
        Cmd.none
      )
    MinimizeControlPanel -> ({model | controlPanelMinimized = True}, Cmd.none)
    MaximizeControlPanel -> ({model | controlPanelMinimized = False}, Cmd.none)
    FilterMsg filterMsg ->
        let
            (newModel, cmd) = Filter.update filterMsg model.filters
        in
            ({model | filters = newModel}, Cmd.map FilterMsg cmd)


-- VIEW

view : Model -> Html Msg
view model =
    let
        td s = Html.td [] [text s]
        logTableHeader = Html.thead [] [Html.tr [] [td "Time", td "Cumul.", td "Delta", td "Level", td "Message"]]
        logTableRow entry =
            Html.tr [class (Log.toString entry.severity |> String.toLower)] [
                td entry.timeString,
                td (String.fromInt entry.cumulativeTime),
                td (String.fromInt entry.deltaTime),
                td (Log.toString entry.severity),
                td entry.text
            ]
        logTableBody = Html.tbody [] (List.map logTableRow (Filter.filterLog model.filters model.log))

        controlPanel = if model.controlPanelMinimized
            then
                button [ class "controlPanelButton", onClick MaximizeControlPanel ] [ text "▼F2" ]
            else
                Html.div [class "controlPanel"] [
                    button [ onClick MinimizeControlPanel ] [ text "▲F2" ],
                    text " ",
                    button [ onClick LogRequested ] [ text "Load Log (F4)" ], text " ", text model.fileName,
                    Html.map FilterMsg (Filter.view model.filters)
                ]
    in
        div [] [
            controlPanel,
            Html.table [] [logTableHeader, logTableBody]
        ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [
        Keyboard.downs KeyDown
    ]
