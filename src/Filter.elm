module Filter exposing (Filters, Filter, SpecificFilter, FilterContext, filterLog, Model, emptyModel, Msg, update, view )

import Log exposing (Severity(..), Log, LogEntry)
import Html exposing (div, Html, button, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)

type SpecificFilter =
    AcceptMatching String |
    RejectMatching String

type alias FilterContext = String

type alias Filter = {id: Int, specific: SpecificFilter, context: FilterContext}

type alias Filters = { uid: Int, filters: List Filter }


acceptEntry filter entry =
    let
       passSpecificFilter =
           case filter.specific of
               AcceptMatching needle ->
                   String.contains needle entry.text
               RejectMatching needle ->
                   String.isEmpty needle || (not <| String.contains needle entry.text)
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
filterLog filters log = List.foldl singleFilterLog log filters.filters

-- MODEL

type alias Model = Filters

emptyModel = { uid = 0, filters = [] }

-- UPDATE

type Msg =
    AddAcceptMatching |
    AddRejectMatching |
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
        AddRejectMatching ->
            (
                { model | filters = model.filters ++ [{id = model.uid, specific = RejectMatching "", context = ""}], uid = model.uid + 1 },
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
                            RejectMatching _ -> {filter | specific = RejectMatching newText}
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
        spacer = text " "
        textFilterRow id name filterText contextText =
            div [] [
                text name,
                spacer,
                Html.input [ value filterText, onInput (UpdateFilterText id) ] [],
                spacer,
                text "Context ",
                spacer,
                Html.input [ value contextText, onInput (UpdateFilterContextText id) ] [],
                spacer,
                button [ class "button-outline", onClick (Delete id) ] [ text "✕" ]
            ]

        filterRow filter =
            case filter.specific of
                AcceptMatching filterText -> textFilterRow filter.id "Only accept matching" filterText filter.context
                RejectMatching filterText -> textFilterRow filter.id "Reject those matching" filterText filter.context

        filterRows = div [] (List.map filterRow model.filters)
    in
        div [] [
                filterRows,
                button [ onClick AddAcceptMatching, class "button-outline" ] [ text "+ Accept Matching" ],
                spacer,
                button [ onClick AddRejectMatching, class "button-outline" ] [ text "+ Reject Matching" ]
            ]