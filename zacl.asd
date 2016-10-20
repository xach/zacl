;;;; zacl.asd

(asdf:defsystem #:zacl
  :description "A layer for loading and running some Allegro CL
  projects."
  :author "Zach Beane"
  :license "BSD"
  :depends-on (#:uiop
               #:usocket
               #:bordeaux-threads
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "reader")
               (:file "if-star")
               (:file "package-mp")
               (:file "package-si")
               (:file "package-excl")
               (:file "package-ff")
               (:file "utils")
               (:file "provides")))

