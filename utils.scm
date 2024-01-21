(module utils (*should-run-inline-tests?* inline-tests)
	(import (scheme)
		(chicken process-context))

	(define *should-run-inline-tests?*
	  (let ((v (get-environment-variable "INLINE_TESTS")))
	    (and (string? v)
		 (string=? v "1"))))

	(define-syntax inline-tests
	  (syntax-rules ()
	    ((_ expr ...)
	     (if *should-run-inline-tests?*
		 (begin expr ...))))))

