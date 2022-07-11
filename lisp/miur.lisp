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

; (defun ps ()
;   (let ((output (uiop:run-program '("ls" "-l" "/data") :output :string)))
;     (loop for line in (rest (cl-ppcre:split "(\\n+)" output))
;           collect (cl-ppcre:split "(\\s+)" line))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOM / Database

;; DEBUG: (mapcar #'sb-mop:slot-definition-name (sb-mop:class-direct-slots (class-of item)))
;; NOTE:(nesting): "graph-id" :has "triple-id"
(defclass fact ()
  ((ts :initform (get-internal-real-time)) ; ALT: simply increment index
   (subj :initarg :subj :reader fact-subj)
   (pred :initarg :pred :reader fact-pred)
   (obj :initarg :obj :reader fact-obj)))

(defun make-fact (s p o)
  (make-instance 'fact :subj s :pred p :obj o))

;; DEBUG: (slot-value item 'ts)
; (fact-subj item)
; (setf (fact-subj item) 10)
(setf item (make-fact 1 2 3))


(defvar *db* (make-instance 'db))

(defun db-get (s p o &optional (db *db*))
  (db-fact db (make-fact/v s p o)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input

;; DEBUG: (myinput)
(defun myinput ()
  (with-open-file (file #P"miur.asd")
    (loop for i from 0
          for line = (read-line file nil nil)
          while line
          ; collect line
          ; collect (write-to-string i)
          ; collect (format nil "~d: ~a~%" i line)
          ; collect (str:join ": " '((write-to-string i) line))
          )))

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

;; DEBUG: (nc:submit (draw))
(defun draw ()
  ; BAD:(not implemented): (nc:window-size *scr*)
  (let ((scr *scr*) (hh 8) (ww 80))
    (nc:clear scr)
    (nc:move scr 0 2)
    (loop for i from 0 to (+ hh -1)
          for v in (myinput)
          do (nc:add-string scr v :x 0 :y i))
    (nc:refresh scr)))

    ; def draw_list(self) -> None:
    ;     i = 0
    ;     hh, _ww = self._scr.getmaxyx()
    ;     # BAD: unable to print "part" of last item
    ;     items = self._wg[i : i + ((hh - 1) // 2)]
    ;     beg, _end = self._wg._scroll.range(i)
    ;     for i, x in enumerate(items, start=i):
    ;         self._scr.addstr(i * 2, 0, f"{i:02d}| {beg + i:03d}:", C.color_pair(2))
    ;         attr = (C.A_REVERSE | C.A_BOLD) if i == self._wg.pos else C.color_pair(1)
    ;         self._scr.addstr(f" {x}", attr)
    ;         self._scr.addstr(
    ;             i * 2 + 1, 8, f"{x.strsize()} | {x.strdepsnum()}", C.color_pair(3)
    ;         )

;; DEBUG: (sb-ext:quit)
(defun main ()
  (nc:with-screen (scr :input-blocking 100 :bind-debugger-hook nil)
    (setf *scr* scr)
    (nc:bind scr #\c (lambda (win event) (nc:clear scr)))
    (nc:bind scr #\q 'nc:exit-event-loop)
    (nc:bind scr "^D" 'sb-ext:quit)
    (nc:bind scr " " 'draw)
    (nc:run-event-loop scr)))


;; DEBUG: (if (fboundp '_live) (_live))
(defun _live ()
  (nc:submit (draw)))

; (eval-when (:execute)
;   (progn
;     ; (nc:submit (croatoan:add-string miur::*scr* "Hey!"))
;     (nc:submit (nc:add-string *scr* "Hey!"))
;     (nc:submit (format *swank-output* "Hellou!~%"))
;     (nc:submit (nc:bind *scr* #\c (lambda (win event) (nc:clear *scr*))))
;     (nc:submit (nc:bind *scr* :resize
;       (lambda (win event)
;         (format *swank-output* "Terminal resized to width: ~a, height: ~a~%"
;                 (nc:width win) (nc:height win)))))
;   ))
