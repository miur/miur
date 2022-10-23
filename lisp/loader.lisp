;; USAGE: $ rlwrap sbcl --noinform --load ./loader.lisp

;; ALT:SRC: /cache/plugins/nvim/all/vlime/lisp/start-vlime.lisp:7:
(defparameter *pj-dir*
  (pathname-directory (parse-namestring *load-truename*)))

;; [_] TODO:PERF: Loading Swank faster (SLIME User Manual, version 2.24) ⌇⡣⡌⠹⠯
;;   https://slime.common-lisp.dev/doc/html/Loading-Swank-faster.html
;; [_] ALSO:IMPL:(lazy swank): EmacsWiki: Stump WM ⌇⡣⡍⢵⡝
;;   https://www.emacswiki.org/emacs/StumpWM

; (load "~/quicklisp/setup.lisp")
; (require "asdf")

;; DISABLED: already present in ./miur.asd DEPs
; (eval-when (:compile-toplevel :execute :load-toplevel)
;   (ql:quickload '(:croatoan :croatoan-ncurses)))

; FIXME: directly load single system inof searching whole project tree
;   TRY: /cache/plugins/nvim/all/vlime/lisp/load-vlime.lisp:22: (asdf:initialize-source-registry)
;        /cache/plugins/nvim/all/vlime/lisp/load-vlime.lisp:34: (try-to-load) fallback to (ql:*)
; (setf ql:*local-project-directories* (list (uiop/os:getcwd)))
; (ql:quickload :miur)

;; OR:(w/o ql):
(require :asdf)
(push '*default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system :miur)

(in-package :miur)
; *package*

;;; Vanilla SWANK
; (ql:quickload :swank)
; (require :swank)
; (swank:create-server :port 4005 :dont-close t)  ; ALSO: :style :fd-handler
; (loop (sleep 2147483647)


;; HACK: redefine *debug-hook* in SWANK for !ncurses cleanup
; SRC: https://stackoverflow.com/questions/34523512/how-do-i-prevent-slime-from-starting-sldb-on-certain-errors
; (in-package swank)
; (setq swank-debugger-hook-orig #'swank-debugger-hook)
; (defun swank-debugger-hook (condition hook)
;   (etypecase condition
;     (sb-int:simple-stream-error
;       (progn
;         (princ "*** Stream error" *error-output*)
;         (abort)))
;     (t (funcall swank-debugger-hook-orig condition hook))))
; (in-package cl-user)
; (swank:create-server :port 4008 :dont-close t)
;;
;; OR: no-swank runtime
;;
; (defun my-debug (condition hook)
;   ; (declare (ignore hook))
;   ; (print condition)
;   ; (abort)
;   (ncurses:def-prog-mode)  ; save current tty modes
;   (ncurses:endwin)  ; restore original tty modes
;   (unwind-protect
;     (if hook (funcall hook condition nil))
;     (nc:refresh *scr*)))  ; restore save modes, repaint screen
; (setf *debugger-hook* #'my-debug)


;; BAD: blocking, single-thread NEED: multithread for !ncurses
; (load "/@/plugins/nvim/all/vlime/lisp/start-vlime.lisp")
;;; OR:NEED: /@/plugins/nvim/all/vlime/lisp -> ~/quicklisp/local-projects/lisp
; (push #P"/@/plugins/nvim/all/vlime/lisp" ql:*local-project-directories*)
; (ql:quickload :vlime :silent t)
(vlime:main :interface #(127 0 0 1)
            :port 7002
            :backend :vlime-usocket)

(miur:main)
