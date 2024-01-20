# desmoctl

A tool for controlling a desmofylakas cluster written in Chicken Scheme.

## Installation

Install [Chicken Scheme](https://wiki.call-cc.org/platforms).

Run the tool in the interpreter with tests:

    INLINE_TESTS=1 csi -s desmoctl.scm

Compile the tool for production.

    csc -O3 -o desmoctl desmoctl.scm

## Usage

See `desmoctl help`.
