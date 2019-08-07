# Slog Logs viewer

Single-page JavaScript webapp for user-friendly viewing and filtering logs 
in the [slog](https://crates.io/crates/slog) format. 
Just open the page in your browser and you are ready to go.

Written in the [Elm programming language](https://elm-lang.org/).

Currently it expects the log lines to look like this:

`Aug 04 12:58:20.509 INFO Connecting to simulated camera device, device: CAM01, DeviceManager: CAM01, module: KmsCompoundDevices`

that is `MMM DD HH:MM:SS.MMM Severity Message`

Multiline log messages are supported.

Could probably be easily modified to support other formats.

## TODO

 - **Parse arbitrary logs with customizable format**
 - User-definable severity levels
 - Improve performance for big logs
 - Remember filter settings

## Known bugs

 - Only parses time component for computing time offsets and deltas. Those wIll wrap negative on midnight
