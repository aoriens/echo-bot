# echo-bot

This is a simple chat bot - with a tiny feature set of sending user-entered text
messages back for a configured amount of times. Additional chat commands are
supported for getting help and configuring the bot at runtime.

## Building

`stack build` is an only command needed.

## Running

    echo-bot [--config PATH_TO_CONFIG]

If no configuration file is provided, default values will be used. A sample
configuration file is available in `echo-bot.config.default`.

## Usage

Any user input that does not match to one of the following patterns is
considered as a commentary, is repeated for the current repetition amount of
times, and echoed back.

Additional commands:

- `/help` outputs short, general information.
- `/repeat` prints the current repetition count and propose to change it.

## Testing

The following example describes testing as a Telegram bot, but it may be
accomodated accordingly to a different front-end supported. Create a custom
configuration file ending with `.private`, e.g. `my.conf.private`, to avoid
committing it to git accidentally, and fill it with the following content:

    import "echo-bot.conf.default"

    core {
      FrontEnd = "telegram"
    }

    telegram {
      ApiToken = "<Insert your API token here>"
    }

    log {
      # Optionally increases the logging verbosity level. The debug level will
      # dump all traffic into the logs.
      Verbosity = "debug"
    }
