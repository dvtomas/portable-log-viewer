import Browser
import File
import File.Select
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Task
import Log
import Log exposing (Severity(..))
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

helpTexts = [
    (Warn, "INTRO Welcome to the Portable Log viewer, TODO homepage"),
    (Info, "INTRO This is a simple application to parse and browse modest-sized text logs. You can filter log messages and highlight particular substrings."),
    (Info, "INTRO Right now the application can only parse logs in a very specific format produced by https://docs.rs/slog-term/2.4.1/slog_term/. Log entries look like this:"),
    (Debug, "Apr 17 12:19:24.597 DEBG Listening, port: 12345, module: ProtoServer"),
    (Info, "INTRO However, if you like the application, contact me, we can try to make it more generic. Support for more/customizable formats would be some work, but it's feasible for a motivated person. We could add support for JSON logs."),
    (Warn, "FEATURES Simple, robust, portable, zero-install app running locally in your browser. No data is ever sent to any server. Save this page and open it later for an internet-less operation"),
    (Info, "FEATURES Written exclusively in the Elm programming language for fun development experience"),
    (Info, "FEATURES Various filters, optionally operating only in given context"),
    (Info, "FEATURES Columns for cumulative time and delta time (difference to previous message time) for easy performance problems spotting"),
    (Info, "FEATURES Highlight searched terms with different colors "),
    (Warn, "USAGE Let's take a closer look on how to use the Portable Log Viewer"),
    (Debug, "USAGE Use the control panel (F2) to load your own log file (try a real world example here: TODO) and to set-up filters."),
    (Debug, "USAGE FILTERS There are currently three filters implemented: "),
    (Debug, "USAGE FILTERS  - Accept Matching: a filter that only accepts entries containing a substring and rejects everything else"),
    (Debug, "USAGE FILTERS  - Reject Matching: a filter that only accepts entries NOT containing a substring"),
    (Debug, "USAGE FILTERS  - Level filter: Rejects everything with severity lower than selected severity"),
    (Debug, "USAGE FILTERS Unique property of the filtering system is the filtering Context. When non-empty, the filters will only operate on entries from the given Context, and leave the rest untouched. Right now Context only means that the entry contains the context substring (try entering e.g. a context FILTERS or TODO to see how it works), but in the future context could be the class name for Java logs, key-value pairs for structured logs etc."),
    (Debug, "USAGE FILTERS Filters can be also muted, i.e. disabled. That can be useful for temporarily inspecting hidden entries, or for using the Accept filter to highlight certain substrings."),
    (Warn, "TODO Right now Portable Log Viewer is just a proof of concept. It works for me, but there's probably some work to be done should it be useful for others as well."),
    (Info, "TODO ESSENTIAL Support for other log formats. The way to go would probably be JSON logs."),
    (Info, "TODO ESSENTIAL Paging. The app chokes on logs even several thousand entries long. I think the bottleneck is simply the browser rendering the huge table."),
    (Debug, "TODO HIGH_PRIORITY Case insensitive matching."),
    (Debug, "TODO HIGH_PRIORITY Toggle highlighting for Accept filter. Add a 'Highlight only' filter type."),
    (Debug, "TODO HIGH_PRIORITY Remember position and retain it when filter changes"),
    (Trace, "TODO IDEA Unfolding filtered items"),
    (Trace, "TODO IDEA User-selectable columns"),
    (Trace, "TODO IDEA Regex support"),
    (Trace, "TODO IDEA Fuzzy matching"),
    (Trace, "TODO IDEA Remember recent logs, save them to local storage"),
    (Trace, "TODO IDEA Remember filter settings, save them to local storage"),
    (Warn, "BUGS Known bugs"),
    (Error, "BUGS Delta time column displays delta to previous message, not previous DISPLAYED message"),
    (Error, "BUGS Only parses time component for computing time offsets and deltas. Those wIll wrap negative on midnight")
    ]

init : () -> (Model, Cmd Msg)
init _ = (
        {
            log = List.map (\(severity, text) ->{ dateString = "now", timeString = "now", time = 0, cumulativeTime = 0, deltaTime = 0, severity = severity, text = text }) helpTexts,
            controlPanelMinimized = True,
            fileName = "",
            filters = Filter.emptyModel
        },
        Cmd.none
    )

-- UPDATE

type Msg
  = LogRequested
  | LogSelected File.File
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
            File.Select.file ["text"] LogSelected
        )
    KeyDown rawKey ->
        case Keyboard.anyKeyOriginal rawKey of
            Just key ->
                if key == F2 then
                    ({model | controlPanelMinimized = not model.controlPanelMinimized}, Cmd.none)
                else if key == F4 then
                    (model, File.Select.file ["text"] LogSelected)
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

type HighlightStringNode =
    Text String |
    Highlight {text: String, highlight: Int}

type alias StringWithHighlights = List HighlightStringNode

addHighlight (needle, highlight) stringWithHighlights =
    let
        highlightString string =
            String.split needle string |> List.map Text |> List.intersperse (Highlight {text = needle, highlight = highlight})
        highlightNode node =
            case node of
                Text string -> highlightString string
                Highlight _ -> [node]
    in
        List.concatMap highlightNode stringWithHighlights

stringWithHighlightsToHtml stringWithHighlights =
    let
        nodeToHtml node =
            case node of
                Text string -> String.split "\n" string |> List.map text |> List.intersperse (Html.br [] [])
                Highlight {text, highlight} -> [Html.span [class ("highlight-" ++ String.fromInt highlight)] [Html.text text]]
    in
        List.concatMap nodeToHtml stringWithHighlights

view : Model -> Html Msg
view model =
    let
        getHighlightString filter =
            case filter.specific of
                Filter.AcceptMatching string -> [(string, 1 + modBy 6 filter.id)]
                Filter.RejectMatching _ -> []
                Filter.LevelFilter _ -> []

        highlightSpecs = List.concatMap getHighlightString model.filters.filters
        highlightedText entry = List.foldl addHighlight [Text entry.text] highlightSpecs

        td s = Html.td [] [text s]
        logTableHeader = Html.thead [] [Html.tr [] [td "Time", td "Cumul.", td "Delta", td "Level", td "Message"]]
        logTableRow entry =
            Html.tr [class (Log.toString entry.severity |> String.toLower)] [
                td entry.timeString,
                td (String.fromInt entry.cumulativeTime),
                td (String.fromInt entry.deltaTime),
                td (Log.toString entry.severity),
                Html.td [] (stringWithHighlightsToHtml (highlightedText entry))
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
