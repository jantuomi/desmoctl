;;; This file is intended to be loaded into csi

(import (chicken io))

(let ((modules (read-lines (open-input-file "modules.list"))))
  (for-each load modules))
