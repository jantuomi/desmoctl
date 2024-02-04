#!/usr/bin/env csi -s

(import (chicken process-context))
(set-environment-variable! "RUN_TESTS" "0")
(load "desmoctl.scm")
(run-desmoctl)
