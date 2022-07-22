; Packages ⌇⡢⣇⡘⢮
;   https://lispcookbook.github.io/cl-cookbook/packages.html
(defpackage #:miur
  (:use #:cl)
  ; NEED:<local-nicknames> (asdf:asdf-version) > 3.3.4.1 (April 2020)
  ;   >> upgrade ASDF in SBCL (OR: quicklisp)
  (:local-nicknames (:re :cl-ppcre)
                    (:nc :croatoan)
                    ; (:alex :alexandria)
                    )
  ; (:import-from #:cl-ppcre #:split) ; single symbol
  (:export #:main))
