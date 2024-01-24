# desmoctl

A tool for controlling a desmofylakas cluster written in Chicken Scheme.

## Installation

Install [Chicken Scheme](https://wiki.call-cc.org/platforms).

Install dependencies:

    chicken-install -from-list requirements.list

## Building and running

Run the tool in the interpreter:

    # Set DEBUG=1 to show debug prints
    # Set INLINE_TESTS=1 to run unit tests before execution
    csi -s run.scm

Compile a statically linked binary for production:

    ./build.sh

## Usage

See `desmoctl help`.

## Development

When adding new modules, add the module names to `modules.list`. The defined module name must match the filename.

## Copyright

© Desmofylakas Core Team 2024
