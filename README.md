<div align="center">
    <div><img src="./rsrc/banner.png" /></div>
    <div>
        <h3>
        <i><b><u>J</u></b>am
        (isn't)
        <b><u>A</u></b>nother
        <b><u>M</u></b>ake</i>
        </h3>
    </div>
</div>

## Demo

![demo movie](./rsrc/demo.gif)

## Overview

Jam is a TUI frontend and command runner. It's core concept is to allow a user to run common commands in the quickest way possible by mapping them to key chords, similar to `emacs` keybinds. It uses a YAML based configuration and supports dynamic imports (including from other command runners such as `make` or `cargo`). There is plenty of work still left to do, but it should be at least usable now.

## Usage

```
Jam (isn't) Another Make. A task runner.

Usage: jam [OPTIONS] [EXEC_ARG] [SHORTCUT]...

Arguments:
  [EXEC_ARG]     First execution argument. If using a shortcut, this is just the first character. Otherwise, it's the name of the target to execute
  [SHORTCUT]...  Individual keys that together (with EXEC_ARG) give a shortcut, uniquely identifying a jam command to execute

Options:
  -d, --dry-run                    Show what jam _would_ do, but don't actually do it
  -l, --log-level <LOG_LEVEL>      Adjusts the logging level [possible values: critical, error, warning, info, debug, trace, disabled]
      --dump-mappings
  -c, --config-file <CONFIG_FILE>
  -h, --help                       Print help
```

## Configuration

```yaml
# General options for jam.
options:
  # Determines how to handle conflicts. Only firstnonmatch supported currently.
  reconciliation_strategy: firstnonmatch
  # Controls the log level; emitted to stderr so this requires redirection if enabled.
  log_level: disabled
# A series of scripts to execute. Each script must return a JSON document with a
# single field, "targets", containing all targets that should be merged with
# this jamfile.
imports:
  - script: "cat ./rsrc/configs/fake_import_output.json"
  - script: "python3 my_cool_importer.py"
# Define targets, the actual executable units for jam.
targets:
  # Targets have names.
  - name: "foo"
    # And can have a command they execute.
    cmd: "echo 'foo'"
  - name: "alpha"
    cmd: "echo 'foo'"
    # Specify children commands. These commands get their names prefixed with
    # their parent's name.
    targets:
      - name: "baz"
        help: "runs the baz command, duh"
        cmd: "echo 'just ran alpha-baz! with '$SHELL"
      - name: "qux"
        cmd: "echo 'bam'"
        # You can specify dependencies as well. This lets you create links
        # between different targets even if they aren't hierarchically
        # connected.
        deps:
          - "foo"
  - name: "ambiguous"
    cmd: "echo 'foo'"
    targets:
      - name: "delta"
        cmd: "echo 'ambiguous-delta'"
      - name: "bark"
        cmd: "echo 'ambiguous-bark'"
  - name: "bar"
    cmd: "echo 'bar'"
    # This is an example of a target that has no command to execute, which is
    # allowed if you have children.
  - name: "noexeccmd"
    targets:
      - name: "corge"
        help: "runs the baz command, duh"
        cmd: "printf '\n\n!!!!\n\nran the n-c command!\n\n!!!!\n\n'"
      - name: "delta"
        cmd: "echo 'bam'"
```
