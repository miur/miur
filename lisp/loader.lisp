;; USAGE: $ rlwrap sbcl --noinform --load ./loader.lisp

(load "~/quicklisp/setup.lisp")
(require "asdf")

(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload '(:croatoan :swank)))

; FIXME: directly load single system inof searching whole project tree
(setf ql:*local-project-directories* (list (uiop/os:getcwd)))
(ql:quickload :miur)

;; OR:(w/o ql):
; (require :asdf)
; (push '*default-pathname-defaults* asdf:*central-registry*)
; (asdf:load-system :miur)

(in-package :miur)

*package*

(require :swank)
(swank:create-server :port 4005 :dont-close t)


; (eval-when (:execute)
;   (main))
