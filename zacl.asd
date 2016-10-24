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
               #:trivial-garbage)
  :serial t
  :components ((:file "package")
               (:file "defpackage-hack")
               (:file "reader")
               (:file "fake-slots")
               (:file "if-star")
               (:file "package-mp")
               (:file "package-si")
               (:file "package-excl")
               (:file "package-ff")
               (:file "package-sys")
               (:file "package-net.uri")
               (:file "utils")
               (:file "provides")))

