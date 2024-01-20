# desmoctl

A tool for controlling a desmofylakas cluster written in Chicken Scheme.

## Installation

Install [Chicken Scheme](https://wiki.call-cc.org/platforms).

Run the tool in the interpreter:

    INLINE_TESTS=1 csi -s run.scm  # with tests
    csi -s run.scm                 # without tests

Compile a statically linked binary for production:

    ./build.sh

## Usage

See `desmoctl help`.

## Development

When adding new modules, add the module names to `modules.list`. The defined module name must match the filename.

## Copyright

© Desmofylakas Core Team 2024