module Filter exposing (Filters, Filter, SpecificFilter, FilterContext, filterLog, Model, emptyModel, Msg, update, view )

import Log exposing (Severity(..), toInt, Log, LogEntry)
import Html exposing (div, Html, button, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)

type SpecificFilter =
    AcceptMatching String |
    RejectMatching String |
    LevelFilter Severity

type alias FilterContext = String

type alias Filter = {id: Int, specific: SpecificFilter, context: FilterContext, mute: Bool}

type alias Filters = { uid: Int, filters: List Filter }

acceptEntry filter entry =
    let
       passSpecificFilter =
           case filter.specific of
               AcceptMatching needle ->
                   String.contains needle entry.text
               RejectMatching needle ->
                   String.isEmpty needle || (not <| String.contains needle entry.text)
               LevelFilter severity ->
                   toInt entry.severity >= toInt severity
    in
        if String.isEmpty filter.context then
            passSpecificFilter
        else
            if (String.contains filter.context entry.text) then
                passSpecificFilter
            else
                True

singleFilterLog: Filter -> Log -> Log
singleFilterLog filter log = List.filter (acceptEntry filter) log

filterLog: Filters -> Log -> Log
filterLog filters log = List.foldl singleFilterLog log (List.filter (\filter -> not filter.mute) filters.filters)

-- MODEL

type alias Model = Filters

emptyModel = { uid = 0, filters = [] }

-- UPDATE

type Msg =
    AddAcceptMatching |
    AddRejectMatching |
    AddLevelFilter |
    UpdateSeverity Int Severity |
    UpdateFilterText Int String |
    UpdateFilterContextText Int String |
    ToggleMute Int |
    Delete Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        doUpdateFilter id updateFunc =
            let
                updateFilter filter =
                    if filter.id == id then
                        updateFunc filter
                    else
                        filter
            in
                (
                    {model | filters = List.map updateFilter model.filters},
                    Cmd.none
                )

        addFilter specific =
            (
                { model | filters = model.filters ++ [{id = model.uid, specific = specific, context = "", mute = False}], uid = model.uid + 1 },
                Cmd.none
            )
    in
        case msg of
            AddAcceptMatching -> addFilter (AcceptMatching "")
            AddRejectMatching -> addFilter (RejectMatching "")
            AddLevelFilter ->  addFilter (LevelFilter Debug)
            UpdateFilterContextText id newContext ->
                doUpdateFilter id (\filter -> {filter | context = newContext})
            UpdateSeverity id newSeverity ->
                doUpdateFilter id (\filter ->
                    case filter.specific of
                        LevelFilter _ -> {filter | specific = LevelFilter newSeverity}
                        _ -> filter
                    )
            UpdateFilterText id newText ->
                doUpdateFilter id (\filter ->
                    case filter.specific of
                        AcceptMatching _ -> {filter | specific = AcceptMatching newText}
                        RejectMatching _ -> {filter | specific = RejectMatching newText}
                        LevelFilter _ -> filter
                    )
            ToggleMute id ->
                doUpdateFilter id (\filter -> {filter | mute = not filter.mute})
            Delete id ->
                (
                    {model | filters = List.filter (\filter -> filter.id /= id) model.filters},
                    Cmd.none
                )

-- VIEW

view : Model -> Html Msg
view model =
    let
        spacer = text " "

        toggleButtonClass bool = class (if bool then "button" else "button-outline")

        row filter name customComponent = div [] [
                spacer,
                button [ class "button-outline", onClick (Delete filter.id) ] [ text "✕" ],
                spacer,
                button [ toggleButtonClass filter.mute, onClick (ToggleMute filter.id) ] [ text "Mute" ],
                spacer,
                text name,
                spacer,
                customComponent,
                spacer,
                text "Context ",
                spacer,
                Html.input [ value filter.context, onInput (UpdateFilterContextText filter.id) ] [],
                spacer
            ]

        textFilterRow filter name filterText =
            row filter name (Html.input [ value filterText, onInput (UpdateFilterText filter.id) ] [])

        selectSeverityButton id selectedSeverity severity =
            button [ toggleButtonClass (selectedSeverity == severity), onClick (UpdateSeverity id severity) ] [ text (Log.toString severity) ]

        levelFilterRow filter severity =
            row filter "Level must be at least" (Html.span [] (List.map (\def -> selectSeverityButton filter.id severity def.severity) Log.severityDef))

        filterRow filter =
            case filter.specific of
                AcceptMatching filterText -> textFilterRow filter "Only accept matching" filterText
                RejectMatching filterText -> textFilterRow filter "Reject those matching" filterText
                LevelFilter severity -> levelFilterRow filter severity

        filterRows = div [] (List.map filterRow model.filters)
    in
        div [] [
                filterRows,
                button [ onClick AddAcceptMatching, class "button-outline" ] [ text "+ Accept Matching" ],
                spacer,
                button [ onClick AddRejectMatching, class "button-outline" ] [ text "+ Reject Matching" ],
                spacer,
                button [ onClick AddLevelFilter, class "button-outline" ] [ text "+ Level" ]
            ]