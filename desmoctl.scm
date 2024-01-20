(import (chicken process-context)
        (chicken format)
        matchable
        test)

(define should-run-inline-tests?
  (let ((v (get-environment-variable "INLINE_TESTS")))
    (and (string? v)
         (string=? v "1"))))

(define-syntax inline-tests
  (syntax-rules ()
    ((_ expr ...)
     (if should-run-inline-tests?
       (begin
          expr ...)))))

; CLI arg parser

(define usage-string (format (string-append
  "Usage: desmoctl [-f cfg-path] SUBCOMMAND~%~%"
  "Subcommands:~%"
  "    apply          Apply the cluster config~%"
  "    status         Show cluster status~%"
  "    logs           Show cluster logs~%"
  "    help           Show this help text~%~%"
  "Top-level flags:~%"
  "    -f cfg-path    Path to the cluster config file (default: desmo.scm)")))

(define initial-opts '(
  (cfg-path "desmo.scm")
))

(define (is-flag-like? args)
  (let ([first-char (string-ref (car args) 0)])
    (eq? #\- first-char)))

(inline-tests
  (test-group "is-flag-like?"
    (test "returns true for valid list" #t (is-flag-like? '("-f" "desmo.scm")))
    (test "returns false for invalid list" #f (is-flag-like? '("apply")))))

;;; Returns a list of parsed options and the remaining arguments.
(define (parse-top-level-flags opts args)
  (match args
    [()
      `(parse-ok ,opts '())]
    [("-f" cfg-path . rest)
      (parse-top-level-flags (cons `(cfg-path ,cfg-path) opts)
                             rest)]
    [(? is-flag-like?)
        `(parse-error ,usage-string)]
    [_
        `(parse-ok ,opts ,args)]))

(inline-tests
  (test-group "parse-top-level-flags"
    (test "returns parse-error for invalid flag" 'parse-error
      (car (parse-top-level-flags initial-opts '("--invalid-flag"))))
    (test "returns parse-ok for empty args" 'parse-ok
      (car (parse-top-level-flags initial-opts '())))
    (test "returns parse-ok for valid flag" 'parse-ok
      (car (parse-top-level-flags initial-opts '("-f" "foobar.scm" "status"))))
    (test "returns parse-ok for valid flag and subcommand" 'parse-ok
      (car (parse-top-level-flags initial-opts '("-f" "desmo.scm" "apply"))))))

(define (parse-subcommand args)
  (match args
    [("apply")
      '(parse-ok cmd-apply ())]
    [("status" . rest)
      (print "TODO status subcommand")
      (exit 0)]
    [("logs" . rest)
      (print "TODO logs subcommand")
      (exit 0)]
    [("help" . rest)
      (print usage-string)
      (exit 0)]
    [_ `(parse-error ,usage-string)]))

(define top-level-flags-parse-result (parse-top-level-flags initial-opts (command-line-arguments)))
(cond
  [(eq? (car top-level-flags-parse-result) 'parse-error)
   (print (cadr top-level-flags-parse-result))
   (exit 1)])

(define cli-opts (cadr top-level-flags-parse-result))
(define cli-subcommand-args (caddr top-level-flags-parse-result))

(print (format "debug: cli-opts: ~A" cli-opts))
(print (format "debug: cli-subcommand-args: ~A" cli-subcommand-args))

(define subcommand-parse-result (parse-subcommand cli-subcommand-args))

(cond
  [(eq? (car subcommand-parse-result) 'parse-error)
   (print (cadr subcommand-parse-result))
   (exit 1)])

(define cli-subcommand (cadr subcommand-parse-result))
(define cli-opts (append (caddr subcommand-parse-result) cli-opts))

(print (format "debug: cli-subcommand: ~A" cli-subcommand))
(print (format "debug: cli-opts: ~A" cli-opts))

; evaluate parsed command

(define (eval-apply opts)
  'done)

(define (eval-subcommand subcmd opts)
  (match subcmd
    ['cmd-apply (eval-apply opts)]
    [other (print `(eval-error ,(format "invalid subcommand: ~A" other)))]))

(display (format "debug: eval-cmd: ~A ~%" (eval-subcommand cli-subcommand cli-opts)))
