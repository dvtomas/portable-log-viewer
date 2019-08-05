module Log exposing (Severity(..), toString, LogEntry, Log, stringToLog)

type alias Millis = Int

type Severity = Trace | Debug | Info | Warn | Error
toString severity = case severity of
    Trace -> "Trace"
    Debug -> "Debug"
    Info -> "Info"
    Warn -> "Warn"
    Error -> "Error"

type alias LogEntry =
    { dateString: String
    , timeString: String
    , time: Millis
    , offsetTime: Millis
    , deltaTime: Millis
    , severity: Severity
    , text: String
    }

type alias Log = List LogEntry

parseSecs: String -> Maybe Millis
parseSecs s = case String.split "." s of
    secsString :: millisString :: [] -> Maybe.map2 (\secs millis -> secs * 1000 + millis) (String.toInt secsString) (String.toInt millisString)
    _ -> Nothing

parseTime: String -> Maybe Millis
parseTime s = case String.split ":" s of
    hoursString :: minutestString :: secsString :: [] ->
        Maybe.map3
            (\hours minutes millis -> hours * 3600000 + minutes * 60000 + millis)
            (String.toInt hoursString)
            (String.toInt minutestString)
            (parseSecs secsString)
    _ -> Nothing

-- Parses line like "Aug 04 12:58:24.599 INFO Creating directory and files, module: Savers, module: KmsMeasuringContext, core: KmsCore"
parseLine: String -> Maybe LogEntry
parseLine s =
    case String.split " " s of
        month :: day :: timeString :: severityString :: text ->
            let
                severity = case severityString of
                    "TRCE" -> Just Trace
                    "DEBG" -> Just Debug
                    "INFO" -> Just Info
                    "WARN" -> Just Warn
                    "ERRO" -> Just Error
                    _ -> Nothing
                maybeTime = parseTime timeString
            in
                Maybe.map2
                    (\sev time -> {
                        dateString = (month ++ day),
                        timeString =timeString,
                        time = time,
                        offsetTime = 0,
                        deltaTime = 0,
                        severity = sev,
                        text = (String.join " " text)
                    })
                    severity
                    maybeTime
        _ -> Nothing

accumLine: String -> (List LogEntry) -> (List LogEntry)
accumLine line logEntries =
    case parseLine line of
        Just entry -> entry :: logEntries
        Nothing -> case logEntries of
            head :: tail -> {head | text = head.text ++ line} :: tail
            [] -> logEntries {-First line is not a valid log line, discard it-}

stringToLog: String -> Log
stringToLog s  =
    let
        lines = String.split("\n") s
        entries = List.foldl accumLine [] lines
    in
        List.reverse entries

