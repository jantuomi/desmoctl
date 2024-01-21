(module desmo-apply (run-apply)
	(import scheme
		(chicken base)
		shell
		test
		utils)

	(define (run-apply cfg)
	  (let* ((cluster (assoc 'cluster cfg))
		 (nodes (cdr cluster)))

	    (map gather-node-facts nodes)

	    (print "TODO")
	    'todo))

	(define (get-ssh-command node)
	  (let* ((alist (cdr node))
		 (ssh-user-pair (assoc 'ssh-user alist))
		 (ssh-hostname-pair (assoc 'ssh-hostname alist))
		 (ssh-command-pair (assoc 'ssh-command alist)))
	    (cond ((not ssh-user-pair)
		   '(cfg-error "ssh-user not specified"))
		  ((not ssh-hostname-pair)
		   '(cfg-error "ssh-hostname not specified"))
		  ((not ssh-command-pair)
		   '(cfg-error "ssh-command not specified"))
		  (else
		   (string-append (cadr ssh-command-pair)
				  " "
				  (cadr ssh-user-pair)
				  "@"
				  (cadr ssh-hostname-pair))))))

	(inline-tests
	 (test-group
	  "get-ssh-command"
	  (test "returns error if ssh-user not specified" 'cfg-error
		(car (get-ssh-command '(node (ssh-hostname "foo") (ssh-command "bar")))))
	  (test "returns error if ssh-hostname not specified" 'cfg-error
		(car (get-ssh-command '(node (ssh-user "foo") (ssh-command "bar")))))
	  (test "returns error if ssh-command not specified" 'cfg-error
		(car (get-ssh-command '(node (ssh-user "foo") (ssh-hostname "bar")))))
	  (test "returns ssh command if all specified" "ssh foo@bar"
		(get-ssh-command '(node (ssh-user "foo") (ssh-hostname "bar") (ssh-command "ssh"))))))

	(define (gather-node-facts node)
	  (let* ((ssh-command (get-ssh-command node))
		 (jls-command "jls")
		 (combined (string-append ssh-command " " jls-command))
		 (result (capture ,combined)))
					; TODO parse result
	    (not (equal? result #!eof)))))

