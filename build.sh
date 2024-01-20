#!/bin/bash

set -euxo pipefail

CSC_FLAGS=-O3
BUILD_DIR=build

mkdir -p ${BUILD_DIR}
cd ${BUILD_DIR}

modules=$(cat ../modules.list)

for mod in $modules; do
    csc ${CSC_FLAGS} -c -static -J ../${mod}.scm -unit ${mod} -o ${mod}.o
done

csc ${CSC_FLAGS} -o desmoctl -static -uses desmo-apply ../desmoctl.scm
chmod +x desmoctl
