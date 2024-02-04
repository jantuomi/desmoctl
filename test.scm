(import test
	(chicken process-context))

(set-environment-variable! "RUN_TESTS" "1")

(test-begin "desmoctl")
(load "desmoctl.scm")
(test-end "desmoctl")

(test-exit)
