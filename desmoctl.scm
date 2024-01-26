(import scheme
        (chicken base)
        (chicken process-context)
        (chicken format)
        (chicken pretty-print)
	(chicken condition)
	(chicken port)
	(chicken io)
	srfi-13
	medea
        matchable
        test
	http-client
	intarweb
	uri-common
        )

;;;;;;;;;;;;;;;
;; Utilities ;;
;;;;;;;;;;;;;;;

(define *should-run-inline-tests?*
  (let ((v (get-environment-variable "INLINE_TESTS")))
    (and (string? v)
	 (string=? v "1"))))

(define *debug?*
  (let ((v (get-environment-variable "DEBUG")))
    (and (string? v)
	 (string=? v "1"))))

(define-syntax inline-tests
  (syntax-rules ()
    ((_ expr ...)
     (if *should-run-inline-tests?*
	 (begin expr ...)))))

(define (debug-print text)
  (if *debug?*
      (print "debug: " text)
      (void)))

(define (try-catch catcher fn)
  "Tries (fn) and calls (catcher exn) if fn throws"
  (handle-exceptions exn (catcher exn) (fn)))

(define default-cfg
  `((user-cfg-path
     ,(string-append (get-environment-variable "HOME") "/" ".desmorc"))
    (mgmt-api-url
     "http://localhost:9939")
    (mgmt-api-key "")))

(define (is-flag-like? args)
  (let ([first-char (string-ref (car args) 0)])
    (eq? #\- first-char)))

(define (assert-parse-or-exit result)
  (if (eq? (car result) 'parse-error)
      (begin (print (cadr result))
	     (exit 1))
      #f))

(define (to-json-string datum)
  (with-output-to-string (lambda () (write-json datum))))

(define (from-json-string json)
  (read-json json))

(define (alist-keys alist)
  (map (lambda (elem) (symbol->string (car elem))) alist))

(define (string-repeat s n)
  (if (<= n 0)
      ""
      (string-append s (repeat-string s (- n 1)))))

(define (pretty-print-alists alists)
  (define (pp alist)
    (define (pp-pair pair)
      (print (format "~A: ~A" (symbol->string (car pair)) (cdr pair))))

    (print "")
    (for-each pp-pair alist))

  (if (null? alists)
      "No data"
      (for-each pp alists))
  )

(inline-tests
 (test-group "is-flag-like?"
   (test "returns true for valid list" #t
	 (is-flag-like?'("-c" "desmo.scm")))
   (test "returns false for invalid list" #f
	 (is-flag-like? '("apply"))))
 
 (test-group "to-json-string"
   (test "transforms alist to json object" "{\"a\":123}"
	 (to-json-string '((a . 123))))
   (test "transforms vector to json array" "[1,2,3]"
	 (to-json-string #(1 2 3)))
   (test "transforms bool to json bool" "true"
	 (to-json-string #t)))
 
 (test-group "from-json-string"
   (test "transforms json object to alist" '((a . 123))
	 (from-json-string "{\"a\":123}"))
   (test "transforms json array to vector" #(1 2 3)
	 (from-json-string "[1,2,3]"))
   (test "transforms json bool to bool" #(#t)
	 (from-json-string "[true]"))))

;;;;;;;;;;;;;;;;;
;; API adapter ;;
;;;;;;;;;;;;;;;;;

(define (mock-post-fn . rest)
  ;; (print (format "~%mock-post-fn called with: ~A" rest))
  rest)

(define (post-fn url api-key json)
  (with-input-from-request
   (make-request method: 'POST
                 uri: (uri-reference url)
		 headers: (headers `((x-api-key ,api-key))))
   json read-string))

(define (mock-get-fn . rest)
  ;; (print (format "~%mock-get-fn called with: ~A" rest))
  rest)

(define (get-fn url api-key)
  (with-input-from-request
   (make-request method: 'GET
		 uri: (uri-reference url)
		 headers: (headers `((x-api-key ,api-key))))
   #f read-string))

(define (post-prison post-fn cfg prison-json)
  (let* ((api-url (cadr (assoc 'mgmt-api-url cfg)))
	 (api-key (cadr (assoc 'mgmt-api-key cfg)))
	 (req-url (string-append api-url "/prisons")))
    (post-fn req-url api-key prison-json)))

(define (get-prisons get-fn cfg)
  (let* ((api-url (cadr (assoc 'mgmt-api-url cfg)))
	 (api-key (cadr (assoc 'mgmt-api-key cfg)))
	 (req-url (string-append api-url "/prisons")))
    (get-fn req-url api-key)))

(inline-tests
 (test-group "get-prisons"
   (test "should create correct query" '("http://example.com/prisons" "bar")
	 (get-prisons mock-get-fn
		      '((mgmt-api-url "http://example.com") (mgmt-api-key "bar")))))
 
 (test-group "post-prison"
   (test "should create correct query" '("http://example.com/prisons" "bar" "{\"a\":\"b\"}")
	 (post-prison mock-post-fn
		      '((mgmt-api-url "http://example.com") (mgmt-api-key "bar"))
		      "{\"a\":\"b\"}"))))

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

(define (parse-subcommand-apply args)
  (match (parse-apply-flags args)
    [('parse-ok 'subcmd-apply-help)
     `(parse-ok subcmd-apply-help ())]
    [('parse-ok . rest)
     `(parse-ok subcmd-apply ,(car rest))]
    [other
     other]))

(inline-tests
 (test-group "parse-apply-flags"
   (test "returns parse-ok for valid args (-f some-path)" 'parse-ok
         (car (parse-apply-flags '("-f"  "some-path"))))
   (test "returns parse-ok for valid args (help)" 'parse-ok
         (car (parse-apply-flags '("help"))))
   (test "returns parse-error for invalid args" 'parse-error
	 (car (parse-apply-flags '("foo" "bar"))))
   (test "returns parse-error for empty args" 'parse-error
	 (car (parse-apply-flags '()))))
 
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

  (if *debug?*
      (begin 
	(debug-print "apply cfg:")
	(pretty-print apply-config-content)))

  (define apply-lst (vector->list apply-config-content))

  (print (format "Applying all defined prisons (~A)..." (length apply-lst)))
  
  (define (apply-prison prison)
    (print (format "Applying prison \"~A\"..." (cdr (assoc 'name prison))))
    
    (define apply-json (to-json-string prison))
    
    (debug-print "apply json:")
    (debug-print apply-json)

    (define api-response
      (post-prison post-fn cfg apply-json))

    (debug-print "api-response:")
    (debug-print (format "~A" api-response)))

  (for-each apply-prison apply-lst)

  (print "Apply complete"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse and eval "status" subcommand ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-status cfg)
  (print "Fetching status of all prisons...")

  (define api-response
    (get-prisons get-fn cfg))

  (define prisons (vector->list (from-json-string api-response)))

  (define (debug-print-prison prison)
    ;; TODO improve this
    (debug-print (format "~a" prison)))
  
  (for-each debug-print-prison prisons)
  (pretty-print-alists prisons))

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
 (test-group "parse-top-level-flags"
   (test "returns parse-error for invalid flag" 'parse-error
         (car (parse-top-level-flags default-cfg '("--invalid-flag"))))
   (test "returns parse-ok for empty args" 'parse-ok
         (car (parse-top-level-flags default-cfg '())))
   (test "returns parse-ok for valid flag" 'parse-ok
         (car (parse-top-level-flags default-cfg '("-c" "foobar.scm" "status"))))
   (test "returns parse-ok for valid flag and subcommand" 'parse-ok
         (car (parse-top-level-flags default-cfg '("-c" "desmo.scm" "apply")))))
 
 (test-group "parse-subcommand"
   (test "returns parse-error for invalid subcommand" 'parse-error
         (car (parse-subcommand '("invalid-subcommand"))))
   (test "returns parse-ok for valid subcommand" 'parse-ok
         (car (parse-subcommand '("apply" "-f" "foo.conf"))))))

;; Evaluate subcommand

(define (eval-subcommand subcmd cfg)
  (match subcmd
    ['subcmd-apply (run-apply cfg)]
    ['subcmd-status (run-status cfg)]
    ['subcmd-logs (print "TODO logs")
		  (exit 1)]
    ['subcmd-help
     (print usage-string) 'done]
    ['subcmd-apply-help
     (print apply-usage-string) 'done]
    [other
     (print `(eval-error ,(format "invalid subcommand: ~A" other)))]))

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

  (if *debug?*
      (begin
	(debug-print "user-cfg:")
	(pretty-print user-cfg)))

  (define subcommand-parse-result
    (parse-subcommand (caddr top-level-flags-parse-result)))

  (assert-parse-or-exit subcommand-parse-result)

  (define subcommand
    (cadr subcommand-parse-result))
  (define subcommand-cfg
    (caddr subcommand-parse-result))

  (if *debug?*
      (begin 
	(debug-print "subcommand-cfg:")
	(pretty-print subcommand-cfg)))

  (define cfg (append subcommand-cfg
		      user-cfg
		      default-cfg))

  (if *debug?*
      (begin
	(debug-print "cfg:")
	(pretty-print cfg)))

  (eval-subcommand subcommand cfg))

;; When compiled, run the CLI when executable is run
;; Interpreter should load ./run.scm
(cond-expand
  (compiling (run-desmoctl))
  (else))
