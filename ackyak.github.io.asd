;;;; ackyak.github.io.asd

(asdf:defsystem #:ackyak.github.io
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-who)
  :components ((:file "package")
               (:file "ackyak.github.io")))
