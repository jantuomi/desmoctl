#!/bin/bash

# if the first flag is -h or --help, then show usage
if [[ $1 == "-h" || $1 == "--help" ]]; then
    echo "Usage: build.sh [OPTION]"
    echo "Builds the desmoctl binary."
    echo ""
    echo "Options:"
    echo "  -h, --help    Show this help message"
    echo "  -d, --debug   Enable debugging"
    exit 0
fi

BUILD_DIR=build

# if the first flag is --debug or -d, then set CHICKEN_DEBUGGER=localhost:9999 and add -d3 to CSC_FLAGS
if [[ $1 == "--debug" || $1 == "-d" ]]; then
    export CHICKEN_DEBUGGER=localhost:9999
    export CSC_FLAGS="-d3"
else
    export CHICKEN_DEBUGGER=
    export CSC_FLAGS=-O3
fi

set -euxo pipefail

mkdir -p ${BUILD_DIR}
cd ${BUILD_DIR}

csc ${CSC_FLAGS} -o desmoctl -static ../desmoctl.scm
chmod +x desmoctl
