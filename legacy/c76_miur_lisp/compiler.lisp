;; USAGE: $ sbcl --noinform --non-interactive --disable-debugger --load ./compiler.lisp

;; OR
; --eval "(require :asdf)" \
; --eval "(push '*default-pathname-defaults* asdf:*central-registry*)" \
; --eval "(asdf:load-system :miur)" \

(load "~/quicklisp/setup.lisp")
(require "asdf")

(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload '(:croatoan :croatoan-ncurses)))

; FIXME: directly load single system inof searching whole project tree
(setf ql:*local-project-directories* (list (uiop/os:getcwd)))
(ql:quickload :miur)

(sb-ext:save-lisp-and-die
  "miur.bin"
  :toplevel 'miur:main
  :executable t
  :purify t)
