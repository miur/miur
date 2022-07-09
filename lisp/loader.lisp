;; USAGE: $ rlwrap sbcl --noinform --load ./loader.lisp

(load "~/quicklisp/setup.lisp")
(require "asdf")

(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload '(:croatoan)))

; FIXME: directly load single system inof searching whole project tree
(setf ql:*local-project-directories* (list (uiop/os:getcwd)))
(ql:quickload :miur)

;; OR:(w/o ql):
; (require :asdf)
; (push '*default-pathname-defaults* asdf:*central-registry*)
; (asdf:load-system :miur)

; (in-package :miur)
; *package*

;;; Vanilla SWANK
; (ql:quickload '(:croatoan :swank)))
; (require :swank)
; (swank:create-server :port 4005 :dont-close t)


;;; OR:NEED: /@/plugins/nvim/all/vlime/lisp -> ~/quicklisp/local-projects/lisp
; (ql:quickload :vlime)
; (vlime:main)
; (vlime:main :interface #(127 0 0 1)
;             :port 7002
;             :backend :vlime-usocket)
; BAD: blocking, single-thread NEED: multithread
(load "/@/plugins/nvim/all/vlime/lisp/start-vlime.lisp")


; (eval-when (:execute)
;   (main))
