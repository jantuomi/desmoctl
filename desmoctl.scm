(import (chicken base)
        (only (chicken process-context)
	      get-environment-variable
	      command-line-arguments)
        (chicken format)
        (chicken pretty-print)
	(chicken condition)
	(chicken port)
	(chicken io)
	(only (chicken file)
	      create-directory
	      delete-directory)
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
        )

;;;;;;;;;;;;;;;
;; Utilities ;;
;;;;;;;;;;;;;;;

(define (id x) x)
(define nil '())

(cond-expand
  ((not compiling)
   ;; when interpreted or loaded
   
   (import (only test test-group test))

   (define *should-run-inline-tests?*
     (let ((v (get-environment-variable "INLINE_TESTS")))
       (and (string? v)
	    (string=? v "1"))))

   (define *cumulative-test-cases* '())

   (define-syntax inline-tests
     (syntax-rules ()
       ((_ expr ...)
	(if *should-run-inline-tests?*
	    (set! *cumulative-test-cases* (append (quote (expr ...))
						  *cumulative-test-cases*)))))))

  (else
   ;; when compiled
   
   (define-syntax inline-tests
     (syntax-rules ()
       ((_ expr ...)
	(void))))))

(define *debug?*
  (let ((v (get-environment-variable "DEBUG")))
    (and (string? v)
	 (string=? v "1"))))

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

(define default-cfg
  `((user-cfg-path
     . ,(filepath:combine (get-environment-variable "HOME") ".desmorc"))
    (mgmt-api-url
     . "http://localhost:9939")
    (mgmt-api-key
     . "")
    ))

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

(define (alist-keys alist)
  (map (lambda (elem) (symbol->string (car elem))) alist))

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
  (let* ((api-url (cdr (assoc 'mgmt-api-url cfg)))
	 (api-key (cdr (assoc 'mgmt-api-key cfg)))
	 (req-url (string-append api-url "/prisons")))
    (post-fn req-url api-key prison-json)))

(define (get-prisons get-fn cfg)
  (let* ((api-url (cdr (assoc 'mgmt-api-url cfg)))
	 (api-key (cdr (assoc 'mgmt-api-key cfg)))
	 (req-url (string-append api-url "/prisons")))
    (get-fn req-url api-key)))

(inline-tests
 (test-group "API adapter"
   (test-group "get-prisons"
     (test "should create correct query" '("http://example.com/prisons" "bar")
	   (get-prisons mock-get-fn
			'((mgmt-api-url . "http://example.com") (mgmt-api-key . "bar")))))
   
   (test-group "post-prison"
     (test "should create correct query" '("http://example.com/prisons" "bar" "{\"a\":\"b\"}")
	   (post-prison mock-post-fn
			'((mgmt-api-url . "http://example.com") (mgmt-api-key . "bar"))
			"{\"a\":\"b\"}")))))

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
     (parse-apply-flags (cons `(apply-config-path . ,path) opts)
			rest)]
    [("-F" path . rest)
     (parse-apply-flags (append `((apply-config-path . ,path)
				  (apply-config-eval . #t)) opts)
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
    (let* ((path (or (cdr (assoc 'apply-config-path cfg))
		     (begin (print "Error: no path to apply config supplied")
			    (exit 1))))
	   (catcher (lambda (e)
		      (print (format "Error: no apply config found at ~A" path))
		      (exit 1)))
	   (open-apply-cfg (lambda () (read (open-input-file path)))))
      (try-catch catcher open-apply-cfg)))

  (define content (if (cdr (assoc 'apply-config-eval cfg))
		      (eval content-raw (null-environment 5))
		      content-raw))

  (debug-print "apply cfg:" content)

  (define apply-lst (vector->list content))

  (print (format "Applying all defined prisons (~A)..." (length apply-lst)))
  
  (define (apply-prison prison)
    (print (format "Applying prison \"~A\"..." (cdr (assoc 'name prison))))
    
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

  (define (debug-print-prison prison)
    (debug-print (format "~a" prison)))
  
  (for-each debug-print-prison prisons)

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
	   "    -F path        Path to a Scheme program that evaluates to the build manifest-%"
	   "    -o path        Output archive path"
	   "~%"
	   "Subcommands:~%"
	   "    help           Show this help text"
           )))

(define (parse-build-flags opts args)
  "Returns an alist of opts"
  (match args
    [()
     (if (null? opts)
	 `(parse-error ,build-usage-string)
	 `(parse-ok ,opts))]
    [("-f" path . rest)
     (parse-build-flags (cons `(build-manifest-path . ,path) opts)
			rest)]
    [("-F" path . rest)
     (parse-build-flags (append `((build-manifest-eval . ,path)
				  (build-manifest-eval . #t)) opts)
			rest)]
    [("-o" path . rest)
     (parse-build-flags (cons `(build-archive-path . ,path) opts)
			rest)]
    [(? is-flag-like?)
     `(parse-error ,build-usage-string)]
    [("help")
     `(parse-ok subcmd-build-help)]
    [_
     `(parse-error ,build-usage-string)]))

(define (parse-subcommand-build build-cfg args)
  (match (parse-build-flags build-cfg args)
    [('parse-ok 'subcmd-build-help)
     `(parse-ok subcmd-build-help ())]
    [('parse-ok opts)
     `(parse-ok subcmd-build ,opts)]
    [other
     other]))

(define (run-build cfg)
  (print "Reading build manifest...")

  (define manifest-path
    (cdr (or (assoc 'build-manifest-path cfg)
	     (begin (print "Error: no path to build manifest supplied")
		    (exit 1)))))
  
  (define content-raw
    (let* ((catcher (lambda (e)
		      (print (format "Error: no build manifest found at ~A" manifest-path))
		      (exit 1)))
	   (open-build-manifest (lambda () (read (open-input-file manifest-path)))))
      (try-catch catcher open-build-manifest)))

  (define content (if (cdr (assoc 'build-manifest-eval cfg))
		      (eval content-raw (null-environment 5))
		      content-raw))

  (if *debug?*
      (begin (debug-print "manifest content:")
	     (pretty-print content)))

  (define filespec-list
    (match (assoc 'files content)
      [#f '()]
      [pair (vector->list (cdr pair))]))

  (define work-area-path "__desmowork")
  (define desmometa-path (filepath:combine work-area-path "__desmometa"))

  (create-directory work-area-path)
  (create-directory desmometa-path)

  (define pwd (get-environment-variable "PWD"))
  
  (define (symlink-to-work-area filespec)
    (match-let (((from to-jail-abs) (string-split filespec ":")))
      (define to (string-append work-area-path to-jail-abs))
      (define to-dir (filepath:take-directory to))
      
      (create-directory to-dir #t)

      (define symlink-from (filepath:combine pwd from))
      (define symlink-to (filepath:combine pwd to))

      (debug-print "from:" symlink-from)
      (debug-print "to:" symlink-to)
      (create-symbolic-link symlink-from symlink-to)))
  
  (for-each symlink-to-work-area filespec-list)
  
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

  (define archive-path (cdr (assoc 'build-archive-path cfg)))

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
    [("build" . rest)
     (parse-subcommand-build default-build-cfg rest)]
    [("push")
     '(parse-ok subcmd-push ())]
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
 (test-group "top level parser"
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
           (car (parse-subcommand '("apply" "-f" "foo.conf")))))))

;; Evaluate subcommand

(define (eval-subcommand subcmd cfg)
  (match subcmd
    ['subcmd-build (run-build cfg)]
    ['subcmd-push (print "TODO push")
		  (exit 1)]
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
    (let* ((path (cdr (assoc 'user-cfg-path cfg-with-flags)))
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

  (define cfg (append subcommand-cfg
		      user-cfg
		      default-cfg))

  (debug-print "cfg:" cfg)

  (eval-subcommand subcommand cfg))

;; Only run inline tests when interpreted and setting enabled
(cond-expand
  ((not compiling)
   (if *should-run-inline-tests?*
       (test-group "desmoctl" (for-each eval *cumulative-test-cases*))))
  (else))

;; When compiled, run the CLI when executable is run
;; Interpreter should load ./run.scm
(cond-expand
  (compiling (run-desmoctl))
  (else))
