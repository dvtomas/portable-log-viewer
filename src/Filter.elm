module Filter exposing (Filters, Filter, SpecificFilter, FilterContext, filterLog, Model, emptyModel, Msg, update, view )

import Log exposing (Severity(..), Log, LogEntry)
import Html exposing (Html, button, p, text)
import Html.Attributes exposing (class, name, value)
import Html.Events exposing (onClick, onInput)

type SpecificFilter = AcceptMatching String

type alias FilterContext = String

type alias Filter = {id: Int, specific: SpecificFilter, context: FilterContext}

type alias Filters = { uid: Int, filters: List Filter }

acceptEntry filter entry =
    case filter.specific of
        AcceptMatching needle ->
            if String.isEmpty filter.context then
                String.contains needle entry.text
            else
                if (String.contains filter.context entry.text) then
                    (String.contains needle entry.text)
                else
                    True

singleFilterLog: Filter -> Log -> Log
singleFilterLog filter log = List.filter (acceptEntry filter) log

filterLog: Filters -> Log -> Log
filterLog filters log = List.foldl singleFilterLog log filters.filters

-- MODEL

type alias Model = Filters

emptyModel = { uid = 0, filters = [] }

-- UPDATE

type Msg =
    AddAcceptMatching |
    UpdateFilterText Int String |
    UpdateFilterContextText Int String |
    Delete Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddAcceptMatching ->
            (
                { model | filters = model.filters ++ [{id = model.uid, specific = AcceptMatching "", context = ""}], uid = model.uid + 1 },
                Cmd.none
            )
        UpdateFilterContextText id newContext ->
            let
                updateFilter filter =
                    if filter.id == id then
                        {filter | context = newContext}
                    else
                        filter
            in
                (
                    {model | filters = List.map updateFilter model.filters},
                    Cmd.none
                )
        UpdateFilterText id newText ->
            let
                updateFilter filter =
                    if filter.id == id then
                        case filter.specific of
                            AcceptMatching _ -> {filter | specific = AcceptMatching newText}
                    else
                        filter
            in
                (
                    {model | filters = List.map updateFilter model.filters},
                    Cmd.none
                )
        Delete id ->
            (
                {model | filters = List.filter (\filter -> filter.id /= id) model.filters},
                Cmd.none
            )

-- VIEW

view : Model -> Html Msg
view model =
    let
        textFilterRow id filterText contextText =
            Html.tr [] [
                (Html.td [] [text "Filter ", Html.input [ value filterText, name "Filter", onInput (UpdateFilterText id) ] []]),
                (Html.td [] [text "Context ", Html.input [ value contextText, name "Context", onInput (UpdateFilterContextText id) ] []]),
                (Html.td [] [button [ class "button-outline", onClick (Delete id) ] [ text "X" ]])
            ]

        filterRow filter =
            case filter.specific of
                AcceptMatching filterText -> textFilterRow filter.id filterText filter.context

        tableBody = Html.tbody [] (List.map filterRow model.filters)
    in
        p [] [
                Html.table [] [tableBody],
                button [ onClick AddAcceptMatching ] [ text "Add Accept" ]
            ]