;; USAGE: $ rlwrap sbcl --noinform --load ./loader.lisp

(load "~/quicklisp/setup.lisp")
(require "asdf")

(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload '(:croatoan :croatoan-ncurses)))

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


;; BAD: blocking, single-thread NEED: multithread for !ncurses
; (load "/@/plugins/nvim/all/vlime/lisp/start-vlime.lisp")
;;; OR:NEED: /@/plugins/nvim/all/vlime/lisp -> ~/quicklisp/local-projects/lisp
(push #P"/@/plugins/nvim/all/vlime/lisp" ql:*local-project-directories*)
(ql:quickload :vlime :silent t)
(vlime:main :interface #(127 0 0 1)
            :port 7002
            :backend :vlime-usocket)


(in-package :miur)
(eval-when (:execute)
  (main))
