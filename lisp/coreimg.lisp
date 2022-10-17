
;; REF: https://github.com/marcoheisig/common-lisp-tweaks
;; Stop Lisp from SCREAMING AT YOU.
(setf *print-case* :downcase)
;; Replace very deeply nested structures by '#' when printing.
; (setf *print-level* 50)
;; Replace elements of very long sequences by '...' when printing.
; (setf *print-length* 200)

;; PERF:WARN: don't forget to disable for release/benchmark
#+sbcl
(progn
  (sb-ext:restrict-compiler-policy 'safety 3)
  (sb-ext:restrict-compiler-policy 'debug 3))


;; TODO:BET:(bootstrap):  https://matteolandi.net/plan.html#day-2021-11-05
(load "~/quicklisp/setup.lisp")
(require "asdf")

;; Ensure that Quicklisp is up to date.
(ql:update-client)
(ql:update-all-dists)

;; Load libraries that you care about
(dolist (system '(; "alexandria"
                  ; "babel"
                  ; "bordeaux-threads"
                  ; "cffi"
                  ; "closer-mop"
                  "cl-ppcre"
                  ; "cl-fad"
                  "croatoan"
                  "croatoan-ncurses"
                  ; "flexi-streams"
                  ; "nibbles"
                  ; "named-readtables"
                  ; "split-sequence"
                  "str"
                  "swank"
                  ; "trivial-backtrace"
                  ; "trivial-features"
                  ; "trivial-garbage"
                  ; "trivial-macroexpand-all"
                  ; "trivial-package-local-nicknames"
                  ;; ...
                  ))
  (ql:quickload system))


; FIXME: directly load single system inof searching whole project tree
; (setf ql:*local-project-directories* (list (uiop/os:getcwd)))
; (ql:quickload :miur)


; (sb-ext:save-lisp-and-die
;   "miur.bin"
;   :toplevel 'miur:main
;   :executable t
;   :purify t)

(uiop:dump-image "/cache/miur.core")
