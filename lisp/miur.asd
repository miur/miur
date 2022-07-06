;;;; miur.asd

(asdf:defsystem #:miur
  :description "Semantic graph file manager"
  :author "Dmytro Kolomoiets <amerlyq+code@gmail.com>"
  :license "GPL-3.0-only"
  :version "0.0.1"
  :serial t
  :depends-on (:cl-ppcre)
  :components ((:file "package")
               (:file "miur")))
