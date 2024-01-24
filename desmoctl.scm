(import scheme
        (chicken base)
        (chicken process-context)
        (chicken format)
        (chicken pretty-print)
	(chicken condition)
        matchable
        test
        )

;;;;;;;;;;;;;;;
;; Utilities ;;
;;;;;;;;;;;;;;;

(define *should-run-inline-tests?*
  (let ((v (get-environment-variable "INLINE_TESTS")))
    (and (string? v)
	 (string=? v "1"))))

(define-syntax inline-tests
  (syntax-rules ()
    ((_ expr ...)
     (if *should-run-inline-tests?*
	 (begin expr ...)))))

(define (try-catch catcher fn)
  "Tries (fn) and calls (catcher exn) if fn throws"
  (handle-exceptions exn (catcher exn) (fn)))

(define default-cfg
  `((user-cfg-path
     ,(string-append (get-environment-variable "HOME") "/" ".desmorc"))
    (mgmt-api-url
     "http://localhost:9939")))

(define (is-flag-like? args)
  (let ([first-char (string-ref (car args) 0)])
    (eq? #\- first-char)))

(define (assert-parse-or-exit result)
  (if (eq? (car result) 'parse-error)
      (begin (print (cadr result))
	     (exit 1))
      #f))

(inline-tests
 (test-group "is-flag-like?"
   (test "returns true for valid list" #t (is-flag-like? '("-c" "desmo.scm")))
   (test "returns false for invalid list" #f (is-flag-like? '("apply")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse flags and subcommand ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define usage-string
  (format (string-append
           "Usage: desmoctl [-c cfg-path] SUBCOMMAND~%"
           "~%"
           "Flags:~%"
           "    -c cfg-path    Path to your user config file (default: $HOME/.desmorc)~%"
           "~%"
           "Workload subcommands:~%"
           "    build          Build a workload archive~%"
           "    push           Push a workload archive~%"
           "~%"
           "Orchestration subcommands:~%"
           "    apply          Apply prison config~%"
           "    status         Show prison status~%"
           "    logs           Show logs~%"
           "    help           Show this help text~%"
           "~%"
           "Run \"desmoctl SUBCOMMAND help\" to see help for a subcommand."
           )))


(define (parse-top-level-flags opts args)
  "Returns a list of parsed options and the remaining arguments."
  (match args
    [()
     `(parse-ok ,opts '())]
    [("-c" user-cfg-path . rest)
     (parse-top-level-flags (cons `(user-cfg-path ,user-cfg-path) opts)
                            rest)]
    [(? is-flag-like?)
     `(parse-error ,usage-string)]
    [_
     `(parse-ok ,opts ,args)]))

(inline-tests
 (test-group "parse-top-level-flags"
   (test "returns parse-error for invalid flag" 'parse-error
         (car (parse-top-level-flags default-cfg '("--invalid-flag"))))
   (test "returns parse-ok for empty args" 'parse-ok
         (car (parse-top-level-flags default-cfg '())))
   (test "returns parse-ok for valid flag" 'parse-ok
         (car (parse-top-level-flags default-cfg '("-c" "foobar.scm" "status"))))
   (test "returns parse-ok for valid flag and subcommand" 'parse-ok
         (car (parse-top-level-flags default-cfg '("-c" "desmo.scm" "apply"))))))

(define (parse-subcommand args)
  (match args
    [("build")
     '(parse-ok subcmd-build ())]
    [("push")
     '(parse-ok subcmd-push ())]
    [("apply" . rest)
     (parse-subcommand-apply rest)]
    [("status")
     '(parse-ok subcmd-status ())]
    [("logs")
     '(parse-ok subcmd-logs ())]
    [("help")
     '(parse-ok subcmd-help ())]
    [_
     `(parse-error ,usage-string)]))

(inline-tests
 (test-group "parse-subcommand"
   (test "returns parse-error for invalid subcommand" 'parse-error
         (car (parse-subcommand '("invalid-subcommand"))))
   (test "returns parse-ok for valid subcommand" 'parse-ok
         (car (parse-subcommand '("apply" "foo.conf"))))))

;; Evaluate subcommand

(define (eval-subcommand subcmd cfg)
  (match subcmd
    ['subcmd-apply (run-apply cfg)]
    ['subcmd-status (print "TODO status")
		    (exit 1)]
    ['subcmd-logs (print "TODO logs")
		  (exit 1)]
    ['subcmd-help
     (print usage-string) 'done]
    ['subcmd-apply-help
     (print apply-usage-string) 'done]
    [other
     (print `(eval-error ,(format "invalid subcommand: ~A" other)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse & eval "apply" subcommand ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define apply-usage-string
  (format (string-append
           "Usage: desmoctl apply [-f path] SUBCOMMAND~%"
           "~%"
           "Apply the config at PATH.~%"
	   "~%"
	   "Subcommands:~%"
	   "    help           Show this help text"
           )))

(define (parse-apply-flags args)
  (match args
    [("-f" path)
     `(parse-ok ((apply-config-path ,path)))]
    [("help")
     `(parse-ok subcmd-apply-help)]
    [_
     `(parse-error ,apply-usage-string)]))

(inline-tests
 (test-group "parse-apply-flags"
   (test "returns parse-ok for valid args (-f some-path)" 'parse-ok
         (car (parse-apply-flags '("-f"  "some-path"))))
   (test "returns parse-ok for valid args (help)" 'parse-ok
         (car (parse-apply-flags '("help"))))
   (test "returns parse-error for invalid args" 'parse-error
	 (car (parse-apply-flags '("foo" "bar"))))
   (test "returns parse-error for empty args" 'parse-error
	 (car (parse-apply-flags '())))))

(define (parse-subcommand-apply args)
  (match (parse-apply-flags args)
    [('parse-ok 'subcmd-apply-help)
     `(parse-ok subcmd-apply-help ())]
    [('parse-ok . rest)
     `(parse-ok subcmd-apply ,(car rest))]
    [other
     other]))

(inline-tests
 (test-group "parse-subcommand-apply"
   (test "returns parse-ok for help command" '(parse-ok subcmd-apply-help ())
	 (parse-subcommand-apply '("help")))
   (test "returns parse-ok for -f foo.txt" '(parse-ok subcmd-apply ((apply-config-path "foo.txt")))
	 (parse-subcommand-apply '("-f" "foo.txt")))))

(define (run-apply cfg)
  (define apply-config-content
    (let* ((path (cadr (assoc 'apply-config-path cfg)))
	   (catcher (lambda (e)
		      (print (format "Error: no user config found at ~A" path))
		      (exit 1)))
	   (open-apply-cfg (lambda () (read (open-input-file path)))))
      (try-catch catcher open-apply-cfg)))

  (print "apply cfg:")
  (pretty-print apply-config-content)

  (print "TODO run-apply"))

;;;;;;;;;;;;;;;;;
;; Run the CLI ;;
;;;;;;;;;;;;;;;;;

(define (run-desmoctl)
  (define top-level-flags-parse-result
    (parse-top-level-flags default-cfg
			   (command-line-arguments)))

  (assert-parse-or-exit top-level-flags-parse-result)

  (define cfg-with-flags
    (cadr top-level-flags-parse-result))

  (define user-cfg
    (let* ((path (cadr (assoc 'user-cfg-path cfg-with-flags)))
	   (catcher (lambda (e)
		      (print (format "Note: no user config found at ~A" path))
		      '()))
	   (open-user-cfg (lambda () (read (open-input-file path)))))
      (try-catch catcher open-user-cfg)))

  (print "debug: user-cfg")
  (pretty-print user-cfg)

  (define subcommand-parse-result
    (parse-subcommand (caddr top-level-flags-parse-result)))

  (assert-parse-or-exit subcommand-parse-result)

  (define subcommand
    (cadr subcommand-parse-result))
  (define subcommand-cfg
    (caddr subcommand-parse-result))

  (print "debug: subcommand-cfg")
  (pretty-print subcommand-cfg)

  (define cfg (append subcommand-cfg
		      user-cfg
		      default-cfg))

  (print "debug: cfg")
  (pretty-print cfg)

  (eval-subcommand subcommand cfg))

(cond-expand
  (compiling (run-desmoctl))
  (else))
