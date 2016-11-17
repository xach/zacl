;;;; zacl.asd

(asdf:defsystem #:zacl
  :description "A layer for loading and running some Allegro CL
  projects."
  :author "Zach Beane"
  :license "BSD"
  :depends-on (#:uiop
               #:usocket
               #:bordeaux-threads
               #:cl-ppcre
               #:flexi-streams
               #:quri
               #:trivial-garbage
               #:trivial-backtrace
               #:split-sequence
               #:queues.simple-cqueue
               #:cl-store
               #:alexandria)
  :serial t
  :components ((:file "package")
               #+clozure
               (:file "clozure-specific")
               (:file "defpackage-hack")
               (:file "reader")
               (:file "fake-slots")
               (:file "if-star")
               (:file "package-excl")
               (:file "package-sys")
               (:file "package-mp")
               (:file "package-socket")
               (:file "package-si")
               (:file "package-ff")
               (:file "package-net.uri")
               (:file "package-toplevel.debug")
               (:file "package-util.zip")
               (:file "utils")
               (:file "provides")))

