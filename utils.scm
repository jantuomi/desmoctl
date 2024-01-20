(module utils (inline-tests)
  (import scheme
          (chicken base)
          (chicken process-context))

  (define-syntax inline-tests
    (syntax-rules ()
      ((_ expr ...)
       (let* ((v (get-environment-variable "INLINE_TESTS"))
              (should-run-inline-tests? (and (string? v)
                                             (string=? v "1"))))
         (if should-run-inline-tests?
             (begin expr ...)))))))

