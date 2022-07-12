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
;;; Input

; (defun ps ()
;   (let ((output (uiop:run-program '("ls" "-l" "/data") :output :string)))
;     (loop for line in (rest (cl-ppcre:split "(\\n+)" output))
;           collect (cl-ppcre:split "(\\s+)" line))))

;; DEBUG: (myinput)
(defun myinput ()
  (with-open-file (file #P"miur.asd")
    (loop for i from 0
          for line = (read-line file nil nil)
          while line
          collect line
          ; collect (write-to-string i)
          ; collect (format nil "~d: ~a~%" i line)
          ; collect (str:join ": " '((write-to-string i) line))
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; View

;; DEBUG: (nc:submit (draw *scr*))
(defun draw (scr)
  ; BAD:(not implemented): (nc:window-size *scr*)
  (let ((ww (nc:width scr)) (hh (nc:height scr)))
    (nc:clear scr)
    (nc:move scr 0 2)
    (loop for i from 0 to (+ hh -1)
          for v in (myinput)
          do (let* ((beg 0) (cur 3) (attr (if (= i cur) '(:reverse :bold) '(:normal)))
                    (pfx (format nil "~02a| ~03a:" i (+ beg i))))
               ;; DEBUG:
               ; (nc:submit (croatoan:add-string miur::*scr* "Hey!"))
               ; (nc:submit (format *swank-output* "Hellou!~%"))
               (nc:add-string scr pfx :x 0 :y i :fgcolor :lime :bgcolor :terminal)
               (nc:add-string scr v :x 7 :y i :attributes attr :fgcolor :terminal :bgcolor :terminal)
               ))
    (nc:refresh scr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UIX

;; DEBUG: (nc:submit (keybinds *scr*))
(defun keybinds (scr)
  (nc:bind scr " " (lambda (w ev) (draw scr) (nc:refresh scr)))
  (nc:bind scr #\c (lambda (w ev) (nc:clear scr) (nc:refresh scr)))
  (nc:bind scr #\q 'nc:exit-event-loop)
  (nc:bind scr "^D" 'sb-ext:quit)
  (nc:bind scr :resize (lambda (w ev)
      (nc:add-string scr (format nil "Resize: WxH=~a ~a    "
                                 (nc:width w) (nc:height w)) :x 80 :y 1)
      (nc:refresh scr)))
  (nc:bind scr t (lambda (w ev) (nc:add-string scr (write-to-string ev) :x 80 :y 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main

;; DEBUG: (sb-ext:quit)
(defun main ()
  (print "=main")
  (finish-output nil)
  (nc:with-screen (scr :input-echoing nil
                       :enable-colors t
                       :use-terminal-colors t
                       :input-blocking 100
                       :bind-debugger-hook nil)
    (setf *scr* scr)
    ; (setf (nc:attributes scr) '(:bold))
    ; (setf (nc:color-pair scr) '(:yellow :red))
    (nc:clear scr)
    (keybinds scr)
    ; (event-case (scr event)
    ;   (#\q (return-from event-case))
    ;   (otherwise (princ event scr)
    ;              (nc:refresh scr)))
    (nc:run-event-loop scr))
  (print "=cleanup")
  (finish-output nil))


;; DEBUG: (if (fboundp '_live) (_live))
(defun _live ()
  (nc:submit (draw *scr*)))
