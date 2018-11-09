;;;; ackyak.github.io.asd

(asdf:defsystem #:ackyak.github.io
  :license  "LLGPL"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-who #:parenscript)
  :components ((:file "package")
               (:file "ackyak.github.io")))
