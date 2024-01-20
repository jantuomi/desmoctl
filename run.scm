#!/usr/bin/env csi -s

(import (chicken io))

(let ((modules (read-lines (open-input-file "modules.list"))))
  (for-each load modules))

(load "desmoctl.scm")

