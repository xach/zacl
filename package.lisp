;;;; package.lisp

(defpackage #:zacl
  (:use #:cl)
  (:import-from #:cl-ppcre
                #:create-scanner
                #:scan)
  (:import-from #:flexi-streams
                #:with-output-to-sequence)
  (:import-from #:bordeaux-threads
                #:current-thread))

(defpackage #:zacl-reader
  (:use #:cl)
  (:export #:*allegro-rewriting-readtable*))

(defpackage #:zacl-if-star
  (:use #:cl)
  (:export #:if*
           #:then
           #:thenret
           #:else))

(defpackage #:excl
  (:use #:zacl-if-star)
  (:export #:if*
           #:then
           #:thenret
           #:else)
  (:export #:named-function
           #:featurep
           #:*initial-terminal-io*
           #:*cl-default-special-bindings*)
  (:export #:match-regexp
           #:match-re
           #:replace-regexp)
  (:export #:with-output-to-buffer)
  (:export #:sm))

(defpackage #:ff
  (:use)
  (:export #:def-foreign-call))

(defpackage #:socket
  (:use)
  (:export #:make-socket
           #:accept-connection
           #:local-port
           #:local-host
           #:set-socket-options
           #:ipaddr-to-dotted
           #:ipaddr-to-hostname
           #:dotted-to-ipaddr
           #:lookup-hostname
           #:remote-host
           #:with-pending-connect))

(defpackage #:user
  (:use #:cl
        #:excl))

(defpackage #:sys
  (:use)
  (:export #:reap-os-subprocess
           #:with-timeout
           #:*current-process*
           #:command-line-arguments
           #:*tilde-expand-namestrings*))

(defpackage #:util.zip
  (:use)
  (:export #:inflate-stream))

(defpackage #:net.uri
  (:use)
  (:export #:uri-path
           #:copy-uri
           #:render-uri
           #:merge-uris
           #:uri-host
           #:uri-scheme
           #:uri-port
           #:parse-uri
           #:uri-query
           #:uri-userinfo
           #:uri-fragment
           #:uri))

(defpackage #:net.html.generator
  (:use))

(defpackage #:mp
  (:use)
  (:export #:with-timeout
           #:without-scheduling)
  (:export #:make-gate
           #:open-gate
           #:close-gate
           #:gate-open-p)
  (:export #:make-process-lock
           #:with-process-lock
           #:process-lock)
  (:export #:wait-for-input-available)
  (:export #:make-process
           #:process-wait
           #:process-wait-with-timeout
           #:process-reset
           #:process-allow-schedule
           #:process-kill
           #:process-run-function
           #:process-keeps-lisp-alive-p
           #:process-preset
           #:process-property-list
           #:process-run-reasons
           #:process-add-run-reason
           #:process-revoke-run-reason
           #:process-name
           #:*current-process*))

(defpackage #:si
  (:use)
  (:export #:without-scheduling
           #:global-symbol-value))

(defpackage #:excl.osi
  (:use)
  (:export #:stat
           #:stat-mtime))
