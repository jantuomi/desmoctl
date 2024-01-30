# desmoctl

A tool for controlling a desmofylakas cluster written in Chicken Scheme.

## Installation

Install [Chicken Scheme](https://wiki.call-cc.org/platforms).

You also need to have these tools or libraries:

    tar
    make
	openssl (with dev headers)
	pkg-config
	
Note: on some systems you need to set `PKG_CONFIG_PATH` to a path that contains `openssl.pc`. On MacOS w/ Homebrew this would be:

    export PKG_CONFIG_PATH="$(brew --prefix openssl)/lib/pkgconfig"

Install dependencies, build and install to your preferred location:

    make deps
    make dynamic # or make static
    PREFIX=$HOME/.local/bin/ make install

## Usage

See `desmoctl help`.

## Development

Run the tool in the interpreter:

    # Set DEBUG=1 to show debug prints
    # Set INLINE_TESTS=1 to run unit tests before execution (not available when compiled)
    csi -s run.scm

## Copyright

© Desmofylakas Core Team 2024
