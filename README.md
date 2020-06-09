# echo-bot

This is a simple chat bot with a tiny feature set of sending user-entered text
messages back for a configured amount of times. Additional chat commands are
supported for getting help and configuring the bot at runtime.

## Building

`stack build` is an only command needed.

## Running

    echo-bot [--config PATH_TO_CONFIG]

If no configuration file is provided, default values will be used. A sample
configuration file with documentation is available in `echo-bot.config.default`.
It is ready for use without modifications.

## Usage

Any user input that does not match to one of the following patterns is
considered as a commentary and is repeated for the current repetition amount of
times and echoed back.

Additional commands:

- `/help` outputs short, general information.
- `/repeat` prints the current repetition count and propose to change it.
