(import (chicken base)
        (only (chicken process-context)
	      get-environment-variable
	      command-line-arguments)
        (chicken format)
        (chicken pretty-print)
	(chicken condition)
	(chicken port)
	(chicken io)
	(chicken file)
	(only (chicken file posix)
	      create-symbolic-link)
	(only (chicken string)
	      string-split)
	matchable
	openssl
	(only medea read-json write-json)
	(only http-client with-input-from-request)
	(only intarweb make-request headers)
	(only uri-common uri-reference)
	(only shell capture)
	(only filepath filepath:combine filepath:take-directory)
	(only srfi-13 string-contains)
	srfi-133
        )

;; Retain macro symbols in runtime when compiled
(declare (compile-syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditional execution ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *run-tests?*
  (equal? "1"
	  (or (get-environment-variable "RUN_TESTS") "1")))

(define *debug?*
  (equal? "1"
	  (or (get-environment-variable "DEBUG") "0")))

(cond-expand
  ((not compiling)
   ;; when interpreted or loaded

   (import test)

   (define-syntax inline-tests
     (syntax-rules ()
       ((_ expr ...)
	(if *run-tests?*
	    (begin expr ...))))))

  (else
   ;; when compiled

   (define-syntax inline-tests
     (syntax-rules ()
       ((_ expr ...)
	(void))))))

;;;;;;;;;;;;;;;
;; Utilities ;;
;;;;;;;;;;;;;;;

(define debug-print
  (match-lambda*
    [(msg obj) (if *debug?* (begin (print "debug: " msg)
				   (pretty-print obj)))]
    [(msg) (if *debug?* (print "debug: " msg))]))

(define (try-catch catcher fn)
  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler (lambda (e) (k (catcher e)))
       fn))))

(inline-tests
 (test-group "try-catch"
   (test "returns fn return value when no exn thrown" 'done
	 (try-catch (lambda (e) 'caught) (lambda () 'done)))
   (test "returns catcher return value when exn thrown" 'caught
	 (try-catch (lambda (e) 'caught) (lambda () (car '()))))))

(define (id . args) (apply values args))

(define (assocar key alist)
  (match (assoc key alist)
    [#f #f]
    [pair (car pair)]))

(define (assocdr key alist)
  (match (assoc key alist)
    [#f #f]
    [pair (cdr pair)]))

(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (cons (car lst1) (car lst2))
            (zip (cdr lst1) (cdr lst2)))))

(inline-tests
 (test-group "zip"
   (test "returns correct list for even length input"
	 '((1 . 3) (2 . 4))
	 (zip '(1 2) '(3 4)))))

(define-syntax @
  (syntax-rules ()
    ((_ fn-body expr ...)
     (lambda (x) (fn-body expr ... x)))))

(inline-tests
 (test-group "@"
   (test "expands to a lambda of one argument" 3
	 ((@ + 1) 2))))

(define (all predicate lst)
  (cond ((null? lst) #t)
        ((predicate (car lst)) (all predicate (cdr lst)))
        (else #f)))

(inline-tests
 (test-group "all"
   (test-assert "returns true when all satisfy pred"
     (all (@ equal? 1) (list 1 1 1)))
   (test "returns false when some do not satisfy pred" #f
	 (all (@ equal? 1) (list 2 1 4)))))

(define (any predicate lst)
  (cond ((null? lst) #f)
        ((predicate (car lst)) #t)
        (else (any predicate (cdr lst)))))

(inline-tests
 (test-group "any"
   (test-assert "returns true when some satisfy pred"
     (any (@ equal? 1) (list 1 1 2)))
   (test "returns false when none satisfy pred" #f
	 (any (@ equal? 1) (list 2 3 4)))))

(define (alist? val)
  (and (list? val) (all pair? val)))

(define (pipe x . fns)
  (match fns
    [() x]
    [(fn . rest) (apply pipe (fn x) rest)]))

(define (flow . fns)
  (lambda (x) (apply pipe x fns)))

(define (alist-join2 a b)
  "Join alist1 and alist2 such that alist1 takes priority."
  (define appended (append a b))
  (define (join pairs)
    (match pairs
      [() '()]
      [(head . tail) (alist-update (car head) (cdr head) (join tail))]))
  (join appended))

(define (alist-join . alists)
  "Joins alist1 alist2 ... alistN such that alist1 takes priority."
  (match alists
    [() '()]
    [(head) head]
    [(head . tail)
     (let ((joined-tail (apply alist-join tail)))
       (alist-join2 head joined-tail)
       )]))

(define (is-flag-like? args)
  (let ([first-char (string-ref (car args) 0)])
    (eq? #\- first-char)))

(inline-tests
 (test-group "is-flag-like?"
   (test "returns true for valid list" #t
	 (is-flag-like?'("-c" "desmo.scm")))
   (test "returns false for invalid list" #f
	 (is-flag-like? '("apply")))))

(define (assert-parse-or-exit result)
  (if (eq? (car result) 'parse-error)
      (begin (print (cadr result))
	     (exit 1))
      #f))

(define (to-json-string datum)
  (with-output-to-string (lambda () (write-json datum))))

(inline-tests
 (test-group "to-json-string"
   (test "transforms alist to json object" "{\"a\":123}"
	 (to-json-string '((a . 123))))
   (test "transforms vector to json array" "[1,2,3]"
	 (to-json-string #(1 2 3)))
   (test "transforms bool to json bool" "true"
	 (to-json-string #t))))

(define (from-json-string json)
  (read-json json))

(inline-tests
 (test-group "from-json-string"
   (test "transforms json object to alist" '((a . 123))
	 (from-json-string "{\"a\":123}"))
   (test "transforms json array to vector" #(1 2 3)
	 (from-json-string "[1,2,3]"))
   (test "transforms json bool to bool" #(#t)
	 (from-json-string "[true]"))))

(define (string-repeat s n)
  (if (<= n 0)
      ""
      (string-append s (string-repeat s (- n 1)))))

(inline-tests
 (test-group "string-repeat"
   (test "should return repeated string" "foofoofoo"
	 (string-repeat "foo" 3))))

(define (pretty-print-alists alists)
  (define (pp alist)
    (define (pp-pair pair)
      (print (format "~A: ~A" (symbol->string (car pair)) (cdr pair))))

    (print "")
    (for-each pp-pair alist))

  (if (null? alists)
      "No data"
      (for-each pp alists)))

(define (string-empty? s)
  (= (string-length s) 0))

(inline-tests
 (test-group "string-empty?"
   (test "should return true for empty string" #t
	 (string-empty? ""))
   (test "should return false for non-empty string" #f
	 (string-empty? "foobar"))))

(define (shell-command-capture cmd)
  (match (capture ,cmd)
    [#!eof '(shell-err eof)]
    [(? string-empty?) '(shell-err empty)]
    [other `(shell-ok ,other)]))

(inline-tests
 (test-group "shell-command-capture"
   (test "successful command should return shell-ok" '(shell-ok "123\n")
	 (shell-command-capture "echo 123"))
   (test "failing command should return shell-err" 'shell-err
	 (car (shell-command-capture "cmd-does-not-exist")))))

(define-syntax config-eval-env-symbols
  (syntax-rules ()
    ((_ id ...)
     (begin
       (define *retained-imports* '())
       (define *retained-imports* (cons (cons (quote id) id)
					*retained-imports*))
       ...))))

(config-eval-env-symbols glob format)

(define (config-eval code)
  (define keys (map car *retained-imports*))
  (define (to-binding key)
    `(,key (assocdr (quote ,key) *retained-imports*)))
  (define bindings
    (map to-binding keys))
  (define let-expr
    `(let ,bindings ,code))
  (eval let-expr))

;;;;;;;;;;;;;;;;;
;; API adapter ;;
;;;;;;;;;;;;;;;;;

(define (mock-post-fn . rest)
  ;; (print (format "~%mock-post-fn called with: ~A" rest))
  rest)

(define (mock-get-fn . rest)
  ;; (print (format "~%mock-get-fn called with: ~A" rest))
  rest)

(define http-client (make-parameter with-input-from-request))

(define (post-prison cfg prison-json)
  (let* ((api-url (assocdr 'mgmt-api-url cfg))
	 (api-key (assocdr 'mgmt-api-key cfg))
	 (req-url (string-append api-url "/prisons")))

    ((http-client)
     (make-request method: 'POST
                   uri: (uri-reference req-url)
		   headers: (headers `((x-api-key ,api-key))))
     prison-json read-string)))

(define (get-prisons cfg)
  (let* ((api-url (assocdr 'mgmt-api-url cfg))
	 (api-key (assocdr 'mgmt-api-key cfg))
	 (req-url (string-append api-url "/prisons")))

    ((http-client)
     (make-request method: 'GET
		   uri: (uri-reference req-url)
		   headers: (headers `((x-api-key ,api-key))))
     #f read-string)))

(define (post-archive cfg archive-path)
  (let* ((api-url (assocdr 'mgmt-api-url cfg))
	 (api-key (assocdr 'mgmt-api-key cfg))
	 (req-url (string-append api-url "/prisons")))

    ((http-client)
     (make-request method: 'POST
                   uri: (uri-reference req-url)
		   headers: (headers `((x-api-key ,api-key))))
     `((image file: ,archive-path
	      filename: "desmo_archive.txz"))
     read-string)))

(inline-tests
 (test-group "API adapter"
   (parameterize ((http-client id))
     (test-assert "get-prisons should construct query successfully"
       (get-prisons '((mgmt-api-url . "http://example.com") (mgmt-api-key . "bar"))))

     (test-assert "post-prison should construct query successfully"
       (post-prison '((mgmt-api-url . "http://example.com") (mgmt-api-key . "bar"))
		    "{\"a\":\"b\"}"))

     (test-assert "post-archive should construct query successfully"
       (post-prison '((mgmt-api-url . "http://example.com") (mgmt-api-key . "bar"))
		    "desmo_archive.txz")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse & eval "apply" subcommand ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define apply-usage-string
  (format (string-append
           "Usage: desmoctl apply [-f|-F path] SUBCOMMAND~%"
           "~%"
           "Apply the config at PATH.~%"
	   "Flags:~%"
	   "    -f path        Path to the prison config~%"
	   "    -F path        Path to a Scheme program that evaluates to the prison config-%."
	   "~%"
	   "Subcommands:~%"
	   "    help           Show this help text"
           )))

(define default-apply-cfg
  '((apply-config-eval
     . #f)))

(define (parse-apply-flags opts args)
  "Returns an alist of opts"
  (match args
    [()
     (if (null? opts)
	 `(parse-error ,apply-usage-string)
	 `(parse-ok ,opts))]
    [("-f" path . rest)
     (parse-apply-flags (pipe opts
			      (@ alist-update 'apply-config-path path))
			rest)]
    [("-F" path . rest)
     (parse-apply-flags (pipe opts
			      (@ alist-update 'apply-config-path path)
			      (@ alist-update 'apply-config-eval #t))
			rest)]
    [(? is-flag-like?)
     `(parse-error ,apply-usage-string)]
    [("help")
     `(parse-ok subcmd-apply-help)]
    [_
     `(parse-error ,apply-usage-string)]))

(define (parse-subcommand-apply apply-cfg args)
  (match (parse-apply-flags apply-cfg args)
    [('parse-ok 'subcmd-apply-help)
     `(parse-ok subcmd-apply-help ())]
    [('parse-ok opts)
     `(parse-ok subcmd-apply ,opts)]
    [other
     other]))

(inline-tests
 (test-group "apply"
   (test-group "parse-apply-flags"
     (test "returns parse-ok for valid args (-f some-path)" 'parse-ok
           (car (parse-apply-flags '() '("-f"  "some-path"))))
     (test "returns parse-ok for valid args (help)" 'parse-ok
           (car (parse-apply-flags '() '("help"))))
     (test "returns parse-error for invalid args" 'parse-error
	   (car (parse-apply-flags '() '("foo" "bar"))))
     (test "returns parse-error for empty args" 'parse-error
	   (car (parse-apply-flags '() '()))))

   (test-group "parse-subcommand-apply"
     (test "returns parse-ok for help command"
	   '(parse-ok subcmd-apply-help ())
	   (parse-subcommand-apply '() '("help")))
     (test "returns parse-ok for -f foo.txt"
	   '(parse-ok subcmd-apply ((apply-config-path . "foo.txt")
				    (apply-config-eval . #t)))
	   (parse-subcommand-apply '() '("-F" "foo.txt"))))))

(define (run-apply cfg)
  (define content-raw
    (let* ((path (or (assocdr 'apply-config-path cfg)
		     (begin (print "Error: no path to apply config supplied")
			    (exit 1))))
	   (catcher (lambda (e)
		      (print (format "Error: no apply config found at ~A" path))
		      (exit 1)))
	   (open-apply-cfg (lambda () (read (open-input-file path)))))
      (try-catch catcher open-apply-cfg)))

  (define content (if (assocdr 'apply-config-eval cfg)
		      (config-eval content-raw)
		      content-raw))

  (debug-print "apply cfg:" content)

  (define apply-lst (vector->list content))

  (print (format "Applying all defined prisons (~A)..." (length apply-lst)))

  (define (apply-prison prison)
    (print (format "Applying prison \"~A\"..." (assocdr 'name prison)))

    (define apply-json (to-json-string prison))

    (debug-print "apply json:" apply-json)

    (define api-response
      (post-prison post-fn cfg apply-json))

    (debug-print "api-response:" api-response))

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

  (for-each debug-print prisons)

  (pretty-print-alists prisons))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse and eval "build" subcommand ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-build-cfg
  '((build-manifest-path
     . "desmo.manifest")
    (build-manifest-eval
     . #f)
    (build-archive-path
     . "desmo_archive.txz")))

(define build-usage-string
  (format (string-append
           "Usage: desmoctl build [-f|-F path] [-o path] SUBCOMMAND~%"
           "~%"
           "Build a desmo workload archive according to the build manifest.~%"
	   "~%"
	   "Flags:~%"
	   "    -f path        Path to the build manifest~%"
	   "    -F path        Path to a Scheme program that evaluates to the build manifest~%"
	   "    -o path        Output archive path~%"
	   "~%"
	   "Subcommands:~%"
	   "    help           Show this help text"
           )))

(define (parse-build-flags opts args)
  "Returns an alist of opts"
  (match args
    [()
     `(parse-ok ,opts)]
    [("-f" path . rest)
     (parse-build-flags (pipe opts
			      (@ alist-update 'build-manifest-path path))
			rest)]
    [("-F" path . rest)
     (parse-build-flags (pipe opts
			      (@ alist-update 'build-manifest-path path)
			      (@ alist-update 'build-manifest-eval #t))
			rest)]
    [("-o" path . rest)
     (parse-build-flags (pipe opts
			      (@ alist-update 'build-archive-path path))
			rest)]
    [(? is-flag-like?)
     `(parse-error ,build-usage-string)]
    [("help")
     `(parse-ok subcmd-build-help)]
    [_
     `(parse-error ,build-usage-string)]))

(inline-tests
 (test-group "parse-build-flags"
   (test "returns defaults for empty input" `(parse-ok ,default-build-cfg)
	 (parse-build-flags default-build-cfg '()))
   (test "parses path when -f supplied" `(build-manifest-path . "manifest")
	 (let ((result (parse-build-flags default-build-cfg '("-f" "manifest"))))
	   (assoc 'build-manifest-path (cadr result))))
   (test "parses path when -F supplied" `(build-manifest-path . "manifest")
	 (let ((result (parse-build-flags default-build-cfg '("-F" "manifest"))))
	   (assoc 'build-manifest-path (cadr result))))
   (test "parses eval=true when -F supplied" `(build-manifest-eval . #t)
	 (let ((result (parse-build-flags default-build-cfg '("-F" "manifest"))))
	   (assoc 'build-manifest-eval (cadr result))))
   (test "parses output path when -o supplied" `(build-archive-path . "outfile")
	 (let ((result (parse-build-flags default-build-cfg '("-o" "outfile"))))
	   (assoc 'build-archive-path (cadr result))))
   (test "parses help command" `(parse-ok subcmd-build-help)
	 (parse-build-flags default-build-cfg '("help")))
   ))

(define (parse-subcommand-build build-cfg args)
  (match (parse-build-flags build-cfg args)
    [('parse-ok 'subcmd-build-help)
     `(parse-ok subcmd-build-help ())]
    [('parse-ok opts)
     `(parse-ok subcmd-build ,opts)]
    [other
     other]))

(define (filespec-alist-to-vec alist)
  (define (transform pair)
    (assert (pair? pair))
    (format "~A:~A" (car pair) (cdr pair)))
  (list->vector (map transform alist)))

(define (run-build cfg)
  (define (run-build*)
    (print "Reading build manifest...")

    (define manifest-path
      (or (assocdr 'build-manifest-path cfg)
	  (begin (print "Error: no path to build manifest supplied")
		 (exit 1))))

    (define content-raw
      (let* ((catcher (lambda (e)
			(print (format "Error: no build manifest found at ~A" manifest-path))
			(exit 1)))
	     (open-build-manifest (lambda () (read (open-input-file manifest-path)))))
	(try-catch catcher open-build-manifest)))
    (debug-print "manifest content raw: " content-raw)

    (define content (if (assocdr 'build-manifest-eval cfg)
			(config-eval content-raw)
			content-raw))
    (debug-print "manifest content: " content)
    (assert (alist? content))

    (define files
      (match (assocdr 'files content)
	[#f '()]
	[other other]))

    (if (alist? files)
	(set! files (filespec-alist-to-vec files)))

    (alist-update! 'files files content)
    (debug-print "manifest content after files vec transform: " content)

    (define work-area-path "__desmowork")
    (define desmometa-path (filepath:combine work-area-path "__desmometa"))

    (create-directory work-area-path)
    (create-directory desmometa-path)

    (define pwd (get-environment-variable "PWD"))

    (define (symlink-to-work-area filespec)
      (debug-print "filespec: " filespec)
      (assert (string-contains filespec ":"))

      (match-let (((from to-jail-abs) (string-split filespec ":")))
	(define to (string-append work-area-path to-jail-abs))
	(define to-dir (filepath:take-directory to))

	(create-directory to-dir #t)

	(define symlink-from (filepath:combine pwd from))
	(define symlink-to (filepath:combine pwd to))

	(debug-print "symlink from:" symlink-from)
	(debug-print "symlink to:" symlink-to)
	(create-symbolic-link symlink-from symlink-to)))

    (vector-for-each symlink-to-work-area files)

    (define manifest-json
      (to-json-string content))
    (define manifest-scm
      (format "~s" content))

    (debug-print "manifest-json:" manifest-json)

    (define meta-json-path
      (filepath:combine desmometa-path "manifest.json"))
    (define meta-scm-path
      (filepath:combine desmometa-path "manifest.scm"))
    (define meta-version-path
      (filepath:combine desmometa-path "version"))

    ;; https://stackoverflow.com/a/10441464
    (define (write-to-a-file path txt)
      (call-with-output-file path
	(lambda (output-port)
	  (display txt output-port))
	#:text))

    (write-to-a-file meta-json-path manifest-json)
    (write-to-a-file meta-scm-path manifest-scm)
    (write-to-a-file meta-version-path "1")

    (define archive-path (assocdr 'build-archive-path cfg))

    (define tar-cmd
      (format "2>&1 tar cvhf ~A -C ~A ." archive-path work-area-path))

    (debug-print "tar-cmd:" tar-cmd)

    (print (format "Building archive ~A..." archive-path))

    (define tar-output (shell-command-capture tar-cmd))

    (match (car tar-output)
      ['shell-ok (print (format "Archive ~A built." archive-path))]
      [other (print (format "Failed to build archive ~A." archive-path))
	     (exit 1)])

    (delete-directory work-area-path #t))

  (try-catch (lambda (e) (begin (print "Error: build failed")
				(print-error-message e)
				(exit 1)))
	     run-build*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse and eval "push" subcommand ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-push-cfg
  '((push-archive-path
     . "desmo_archive.txz")))

(define push-usage-string
  (format (string-append
           "Usage: desmoctl push [-a archive-path] SUBCOMMAND~%"
           "~%"
           "Push a desmo workload archive to a Desmofylakas server.~%"
	   "~%"
	   "Flags:~%"
	   "    -a path        Path to the build manifest~%"
	   "~%"
	   "Subcommands:~%"
	   "    help           Show this help text"
           )))

(define (parse-push-flags opts args)
  "Returns an alist of opts"
  (match args
    [()
     `(parse-ok ,opts)]
    [("-a" path . rest)
     (parse-push-flags (pipe opts
			     (@ alist-update 'push-archive-path path))
		       rest)]
    [(? is-flag-like?)
     `(parse-error ,push-usage-string)]
    [("help")
     `(parse-ok subcmd-push-help)]
    [_
     `(parse-error ,push-usage-string)]))

(define (parse-subcommand-push push-cfg args)
  (match (parse-push-flags push-cfg args)
    [('parse-ok 'subcmd-push-help)
     `(parse-ok subcmd-push-help ())]
    [('parse-ok opts)
     `(parse-ok subcmd-push ,opts)]
    [other
     other]))

(define (run-push cfg)
  (define (run-push*)
    (print "Pushing prison archive...")
    (define archive-path (or (assocdr 'push-archive-path cfg)
			     (begin (print "Error: no path to archive supplied")
				    (exit 1))))

    (debug-print "archive-path: " archive-path)
    (post-archive cfg archive-path)
    (print "Push complete."))

  (try-catch (lambda (e) (begin (print "Error: push failed")
				(print-error-message e)
				(exit 1)))
	     run-push*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse flags and subcommand ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-cfg
  `((user-cfg-path
     . ,(filepath:combine (get-environment-variable "HOME") ".desmorc"))
    (mgmt-api-url
     . "http://localhost:9939")
    (mgmt-api-key
     . "")
    ))

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
     (parse-top-level-flags (alist-update 'user-cfg-path user-cfg-path opts)
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
    [("build" . rest)
     (parse-subcommand-build default-build-cfg rest)]
    [("push" . rest)
     (parse-subcommand-push default-push-cfg rest)]
    [("apply" . rest)
     (parse-subcommand-apply default-apply-cfg rest)]
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
         (car (parse-subcommand '("apply" "-f" "foo.conf"))))))

;; Evaluate subcommand

(define (eval-subcommand subcmd cfg)
  (match subcmd
    ['subcmd-build (run-build cfg)]
    ['subcmd-push (run-push cfg)]
    ['subcmd-apply (run-apply cfg)]
    ['subcmd-status (run-status cfg)]
    ['subcmd-logs (print "TODO logs")
		  (exit 1)]
    ['subcmd-help
     (print usage-string) 'done]
    ['subcmd-apply-help
     (print apply-usage-string) 'done]
    ['subcmd-build-help
     (print build-usage-string) 'done]
    ['subcmd-push-help
     (print push-usage-string) 'done]
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
    (let* ((path (assocdr 'user-cfg-path cfg-with-flags))
	   (catcher (lambda (e)
		      (print (format "Note: no user config found at ~A" path))
		      '()))
	   (open-user-cfg (lambda () (read (open-input-file path)))))
      (try-catch catcher open-user-cfg)))

  (debug-print "user-cfg:" user-cfg)

  (define subcommand-parse-result
    (parse-subcommand (caddr top-level-flags-parse-result)))

  (assert-parse-or-exit subcommand-parse-result)

  (define subcommand
    (cadr subcommand-parse-result))
  (define subcommand-cfg
    (caddr subcommand-parse-result))

  (debug-print "subcommand-cfg:" subcommand-cfg)

  (define cfg (alist-join subcommand-cfg
			  user-cfg
			  default-cfg))

  (debug-print "cfg:" cfg)

  (eval-subcommand subcommand cfg))

;; When compiled, run the CLI when executable is run
;; Interpreter should load ./run.scm
(cond-expand
  (compiling (run-desmoctl))
  (else))
