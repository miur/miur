;;
;; WORKFLOW:
;;   * run slime/swank in separate terminal $ ./ctl s
;;   * add/modify sexpr
;;   * <, e f> to eval top-level sexpr under cursor
;;
(in-package :miur)

(defvar *scr* nil) ;; global main screen to access from slime
(defparameter *swank-input* *standard-input*)
(defparameter *swank-output* *standard-output*)

(defparameter *editor* (uiop:getenv "EDITOR"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input

; (defun ps ()
;   (let ((output (uiop:run-program '("ls" "-l" "/data") :output :string)))
;     (loop for line in (rest (cl-ppcre:split "(\\n+)" output))
;           collect (cl-ppcre:split "(\\s+)" line))))

(defun runhere (&rest args)
  (uiop:run-program args :force-shell nil :directory (uiop:getcwd) :output :string))

(defun grep (x)
  (sort (str:lines (runhere "re" x)) 'string-lessp))

;; DEP: vim-fetch -- to be able to directly open spec "/path/to/file:15:20:"
;;   OR:  nvim -c "call setpos('.',[0,line,col,0])"
(defun filespec (grepline)
  ; FUTURE: support optional linenum/columns
  (nth-value 0 (re:scan-to-strings "^.*:\\d+:\\d+(?::|$)" grepline)))

(defun runshell ()
  (shell-out *scr* nil (uiop:getenv "SHELL")))

(defun runeditor ()
  (shell-out *scr* nil *editor* "--"
             (filespec (item-text (undercursor)))))


;; FAIL: stdin redir total trash -- try using temp files in /run
;;   << most likely redir is impossible w/o bkgr thread to supply data
; (with-input-from-string (text "some\nelse")
;   (shell-out *scr* :stream "ifne" *editor*
;             "-" "-c" "setl bt=nofile nowrap efm=%f:%l:%c:%m | cbuffer"
;             ))
(defun runquickfix ()
  (let ((tmp "/run/user/1000/miur/tmp"))
    (uiop:ensure-pathname tmp :ensure-directories-exist t)
    (with-open-file (fd tmp
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (dolist (line (mapcar 'item-text (dbslice)))
      (write-line line fd)))
    (shell-out *scr* nil *editor* "-c" "setl bt=nofile nowrap efm=%f:%l:%c:%m | cbuffer"
               "--" tmp)))


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
;;; DOM

(defclass item ()
  ((text :initarg :text :reader item-text)
   ))

(defun make-item (text)
  (make-instance 'item :text text))

(defmethod item-type ((x item))
  (if (search ":" (item-text x))
      :key :raw))


; (defparameter *dom* (mapcar 'make-item (myinput)))
(defparameter *dom* (mapcar 'make-item (grep "")))

; TEMP: caching database for filesystem -- preload requested info only on-demand
(defun lazydatabase ()
  *dom*)

(defun dbslice ()
  "Horizontal query to get current 'folder' view of chosen elements"
  (lazydatabase))

; HACK: both dbsclice and viewport can be combined into single direct query to DB
;   << this would allow us to work with infinite lists like "chats", etc.
(defun viewport (beg end)
  "Vertical query -- to preview necessary part of the list
    BUT: only after all filters were applied"
  (subseq (dbslice) beg end))

(defun scrollwidget ()
  "Scroll widget"
  (viewport 0 8))

(defun undercursor ()
  (nth 3 (scrollwidget)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; View

;; DEBUG: (nc:submit (draw *scr*))
(defun draw (scr)
  ; BAD:(not implemented): (nc:window-size *scr*)
  (let ((ww (nc:width scr))
        (hh (nc:height scr))
        (beg 0)
        (cur 3))
    (nc:clear scr)
    (nc:move scr 0 2)
    (loop for i from 0 to (+ hh -1)
          for x in (scrollwidget)
          do (let* ((attr (if (= i cur) '(:reverse :bold) '(:normal)))
                    (clr (if (eql :key (item-type x)) :terminal :teal))
                    (pfx (format nil "~02d| ~02d" i (+ beg i))))
               ;; DEBUG:
               ; (nc:submit (croatoan:add-string miur::*scr* "Hey!"))
               ; (nc:submit (format *swank-output* "Hellou!~%"))
               (nc:add-string scr pfx :x 0 :y i :fgcolor :lime :bgcolor :terminal)
               (nc:add-string scr (item-text x) :x 7 :y i :attributes attr :fgcolor clr :bgcolor :terminal)
               ))
    (nc:move scr cur 0)
    (nc:refresh scr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UIX

;; DEBUG: (nc:submit (shell-out *scr*))
;; FAIL: can't
; (defun shell-out (scr &rest cmdline &key input &allow-other-keys)
(defun shell-out (scr input &rest cmdline)
  ; cmd = args or [os.environ.get("SHELL", "sh")]
  (ncurses:def-prog-mode)  ; save current tty modes
  (ncurses:endwin)  ; restore original tty modes
  (format *swank-output* "Shelling out...~%")
  (unwind-protect
    ; (grep "xxx")
    ; ALT:(:interactive): *swank-output* | uiop/stream:*output*
    (uiop:run-program cmdline
                      :force-shell nil
                      :directory (uiop:getcwd)
                      :input (or input :interactive)
                      :output :interactive)
    (format *swank-output* "...Restored~%")
    (nc:refresh scr)  ; restore save modes, repaint screen
    (nc:add-string scr "Returned" :x 80 :y 4)
    )
  )


(defun drawkeyinfo (w ev)
  (nc:add-string *scr* (write-to-string
                       (list (nc:event-key ev)
                             (nc:event-code ev)))
                 :x 80 :y 0))


;; DEBUG: (nc:submit (keybinds *scr*))
(defun keybinds (scr)
  (nc:bind scr " " (lambda (w ev) (draw scr) (nc:refresh scr)))
  (nc:bind scr #\c (lambda (w ev) (nc:clear scr) (nc:refresh scr)))
  (nc:bind scr #\s (lambda (w ev) (runshell)))
  (nc:bind scr #\Newline (lambda (w ev) (runeditor)))
  (nc:bind scr #\o (lambda (w ev) (runquickfix)))
  (nc:bind scr #\q 'nc:exit-event-loop)
  (nc:bind scr "^D" 'sb-ext:quit)
  (nc:bind scr :resize (lambda (w ev)
      (nc:add-string scr (format nil "Resize: WxH=~a ~a    "
                                 (nc:width w) (nc:height w)) :x 80 :y 1)
      (nc:refresh scr)))
  (nc:bind scr t #'drawkeyinfo))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main

;; DEBUG: (sb-ext:quit)
(defun main ()
  (print "=main")
  (setf (uiop:getenv "MIUR_LVL") (write-to-string 1))
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
