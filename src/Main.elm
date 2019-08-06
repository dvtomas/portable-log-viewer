import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, p, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Task
import Log
import Filter

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
    fileName: String,
    filters: Filter.Model
    }

init : () -> (Model, Cmd Msg)
init _ = (
        {
            log = [],
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
  | FilterMsg Filter.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LogRequested -> (
            model,
            Select.file ["text"] LogSelected
        )
    LogSelected file -> (
            model,
            Task.perform (\content -> LogLoaded {fileName = File.name file, content = content}) (File.toString file)
        )
    LogLoaded info -> (
        { model | log = Log.stringToLog info.content, fileName = info.fileName },
        Cmd.none
      )
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
            Html.tr [Html.Attributes.class (Log.toString entry.severity |> String.toLower)] [
                td entry.timeString,
                td (String.fromInt entry.cumulativeTime),
                td (String.fromInt entry.deltaTime),
                td (Log.toString entry.severity),
                td entry.text
            ]
        logTableBody = Html.tbody [] (List.map logTableRow (Filter.filterLog model.filters model.log))

    in
        p [] [
            button [ onClick LogRequested ] [ text "Load Log" ],
            Html.h1 [] [text model.fileName],
            Html.map FilterMsg (Filter.view model.filters),
            Html.table [] [logTableHeader, logTableBody]
        ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none