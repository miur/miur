;;
;; WORKFLOW:
;;   * run slime/swank in separate terminal $ ./ctl s
;;   * add/modify sexpr
;;   * <, e f> to eval top-level sexpr under cursor
;;
(in-package #:miur)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Launch

(defun ps ()
  (let ((output (uiop:run-program '("ls" "-l" "/data") :output :string)))
    (loop for line in (rest (cl-ppcre:split "(\\n+)" output))
          collect (cl-ppcre:split "(\\s\\s+)" line))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main

(defun main ()
  (print "=main")
  (finish-output nil)
  (unwind-protect
       (progn
         (print "=loop")
         (print (ps))
         )
    (print "=cleanup"))
  (finish-output nil))

; (main)
