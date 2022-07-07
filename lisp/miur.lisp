;;
;; WORKFLOW:
;;   * run slime/swank in separate terminal $ ./ctl s
;;   * add/modify sexpr
;;   * <, e f> to eval top-level sexpr under cursor
;;
(in-package :miur)

(defparameter *scr* nil) ;; global main screen to access from slime
(defparameter *swank-output* *standard-output*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Launch

(defun ps ()
  (let ((output (uiop:run-program '("ls" "-l" "/data") :output :string)))
    (loop for line in (rest (cl-ppcre:split "(\\n+)" output))
          collect (cl-ppcre:split "(\\s+)" line))))


; (defun nctest ()
;   "Minimal example: init, output, refresh, end."
;   (initscr)
;   (move 1 1)
;   (mvaddstr 0 0 "hello there")
;   (mvaddstr 7 7 "hello there")
;   (mvaddstr 15 15 "hello there")
;   (refresh)
;   (getch)
;   (endwin))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main

; (defun main ()
;   (nc:with-screen (scr :input-echoing nil :input-blocking t :enable-colors t)
;     (nc:clear scr)
;     (nc:move scr 2 0)
;     (format scr "Type chars. Type q to quit.~%~%")
;     (nc:refresh scr)
;     ; (setf (nc:color-pair scr) '(:yellow :red)
;     ;       (nc:attributes scr) '(:bold))
;     ; (event-case (scr event)
;     ;   (#\q (return-from event-case))
;     ;   (otherwise (princ event scr)
;     ;              (nc:refresh scr)))
;     ))

; (defun main ()
;   (print "=main")
;   (finish-output nil)
;   (unwind-protect
;        (progn
;          (print "=loop")
;          (print (ps))
;          )
;     (print "=cleanup"))
;   (finish-output nil))

(defun main ()
  (nc:with-screen (scr :input-blocking 100 :bind-debugger-hook nil)
    (nc:bind scr #\q 'nc:exit-event-loop)
    (nc:run-event-loop (setf *scr* scr))))

; (eval-when (:execute)
;   (progn
;     (nc:submit (croatoan:add-string miur::*scr* "Hey!"))
;     (nc:submit (format *swank-output* "Hellou!~%"))
;     (nc:submit (nc:bind *scr* #\c (lambda (win event) (nc:clear *scr*))))
;     (nc:submit (nc:bind *scr* :resize
;       (lambda (win event)
;         (format *swank-output* "Terminal resized to width: ~a, height: ~a~%"
;                 (nc:width win) (nc:height win)))))
;   ))
