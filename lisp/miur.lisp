;;
;; WORKFLOW:
;;   * run slime/swank in separate terminal $ ./ctl s
;;   * add/modify sexpr
;;   * <, e f> to eval top-level sexpr under cursor
;;
(in-package :miur)

(defparameter *swank-input* *standard-input*)
(defparameter *swank-output* *standard-output*)
(defparameter *editor* (uiop:getenv "EDITOR"))

(defvar *scr* nil) ;; global main screen to access from slime
(defparameter *quit* nil)
(defparameter *menu* 'menu-navi)
(defparameter *textinput* "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input

; (defun ps ()
;   (let ((output (uiop:run-program '("ls" "-l" "/data") :output :string)))
;     (loop for line in (rest (cl-ppcre:split "(\\n+)" output))
;           collect (cl-ppcre:split "(\\s+)" line))))

(defun runhere (&rest args)
  (uiop:run-program args :force-shell nil :directory (uiop:getcwd) :output :string))

(defun rungrep (x)
  (sort (str:lines (runhere "re" x)) 'string-lessp))

;; DEP: vim-fetch -- to be able to directly open spec "/path/to/file:15:20:"
;;   OR:  nvim -c "call setpos('.',[0,line,col,0])"
(defun filespec (grepline)
  ; FUTURE: support optional linenum/columns
  (nth-value 0 (re:scan-to-strings "^.*:\\d+:\\d+(?::|$)" grepline)))

(defun runshell ()
  (shell-out *scr* nil (uiop:getenv "SHELL")))

;; HACK: #miur※⡢⣯⢁⢏ pre-fill prompt (inof running) by specified cmdline on ZSH startup
;; RQ:(~/.zshrc): if [[ -n ${ZSH_BUFFER-} ]]; then print -z "$ZSH_BUFFER" && unset ZSH_BUFFER; fi
(defun runzshprompt (&optional cmdline)
  ; SBCL※⡢⣱⡒⡄ (non-portable) :environment (list "KEY=value1 and value2")
  ;; HACK: set app env before shell-out to inherit ENV by child
  (if cmdline (setf (uiop:getenv "ZSH_BUFFER") cmdline))
  (shell-out *scr* nil (uiop:getenv "SHELL")))


;; DEBUG: (myinput)
#+(or)
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

(defun myinput ()
  (rungrep ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOM

; TODO: lazy access/query to all [other] metadata on-demand
(defclass item ()
  ((text :reader item-text
         :initarg :text
         :type string
         :documentation "Text to show in widgets")
   ) (:documentation "Single item with all related metadata combined"))

(defun make-item (text)
  (make-instance 'item :text text))

(defmethod item-type ((x item))
  (if (search ":" (item-text x))
      :key :raw))


(defclass snapshot ()
  ((items :reader snapshot-items
          :initarg :items
          ; :type '(array item)
          :documentation "Persistent copies of transient items")
   ) (:documentation "Cached Snapshot of query results"))

; TODO: apply all sort/filter actions first -- only then slice it"
(defmethod snapshot-slice ((x snapshot) &optional (beg 0) (end nil))
  "Vertical query -- to preview necessary part of the list"
  (subseq (snapshot-items x) beg end))


; TEMP: lazy caching database for filesystem
; FIXME: preload requested info only on-demand
; CHG: data -> triples
(defclass db ()
  ((data :reader db-all
         :initarg :data
         ; :initform nil
         ;; BAD:(SBCL ignores :type)
         ;;   https://stackoverflow.com/questions/23060868/defclass-type-information-for-performance
         ; :type '(array item)
         :documentation "Convert all db data to item list")
   ) (:documentation "Cached Knowledge Database"))

(defun make-db (lines)
  (let* ((xs (mapcar 'make-item lines))
         (arr (make-array (length xs)
                          :element-type 'item
                          :initial-contents xs
                          :adjustable t)))
    (make-instance 'db :data arr)))

; HACK: both "query" and "slice" can be combined into single direct query to DB
;   << this would allow us to work with infinite lists like "chats", etc.
; TODO: accept params to filter db list e.g. folder-cwd or grep-prefix
; SRC: https://stackoverflow.com/questions/9549568/common-lisp-convert-between-lists-and-arrays
(defmethod db-query ((x db))
  "Horizontal query to get current 'folder' snapshot view of chosen elements"
  (let ((xs (db-all x)))
    (make-array (length xs)
                :element-type 'item
                ; OR: :displaced-to xs
                :initial-contents xs
                :adjustable nil)))


(defclass scrollwidget ()
  ((provider :reader sw-all
             :initarg :provider
             ; :type snapshot
             :documentation "Snapshot items")
  (offset :reader sw-offset
          :initarg :offset
          :initform 0
          :type integer
          :documentation "Viewport window beginning")
  (limit :reader sw-limit
         :initarg :limit
         :type integer
         :documentation "Viewport window length")
  (curabs :reader sw-curabs
          :initarg :curabs
          :initform 0
          :type integer
          :documentation "Cursor current absolute position")
   ) (:documentation "Scroll widget"))

(defun make-scrollwidget (snap height)
  (make-instance 'scrollwidget :provider snap :limit height))

(defmethod sw-currel ((x scrollwidget))
  "Cursor position relative to viewport window offset"
  (- (sw-curabs x) (sw-offset x)))

(defmethod sw-curitem ((x scrollwidget))
  "Get single item under current cursor"
  (aref (sw-all x) (sw-curabs x)))

(defmethod sw-visible ((x scrollwidget))
  "Visible items inside viewport window"
  (subseq (sw-all x) (sw-offset x) (+ (sw-offset x) (sw-limit x))))

(defmethod (setf sw-limit) (v (x scrollwidget))
  "Set viewport height"
  (when (or (< v 0) (> v (length (slot-value x 'provider))))
    (error "Limit is out of bounds"))
  (setf (slot-value x 'limit) v))

; ALSO:TODO: sw-curinc -- set pos relative to current cursor position
(defmethod (setf sw-currel) (v (x scrollwidget))
  "Set cursor position relative to viewport"
  (setf (sw-curabs x) (+ v (slot-value x 'offset))))

; (sw-offset *sw*)
; (sw-limit *sw*)
(defmethod (setf sw-curabs) (v (x scrollwidget))
  "Set cursor absolute position relative to snapshot -- and move window"
  (let* ((keep 2)
         (beg 0)
         (bot (+ beg keep))
         (off (sw-offset x))
         (low (+ off keep))
         (lim (sw-limit x))
         ;; FIXME: when {lim/wnd < keep*2}
         (upp (- (+ off lim) keep 1))
         (end (1- (length (sw-all x))))
         (mst (1+ (- end lim)))
         (top (- end keep)))
    (multiple-value-bind (wnd abs)
        (cond
          ; HACK: jump to last element
          ((null v) (values mst end))
          ((< v beg) (values beg beg))
          ((< v bot) (values beg v))
          ((< v low) (values (- v keep) v))
          ((<= v upp) (values off v))
          ((<= v top) (values (1+ (+ v (- lim) keep)) v))
          ((<= v end) (values mst v))
          (t (values mst end)))
      ; MAYBE:ALT: multiple-value-setq
      (unless (= off wnd) (setf (slot-value x 'offset) wnd))
      (setf (slot-value x 'curabs) abs)
      )))

; FIXME
(defmethod (setf sw-offset) (v (x scrollwidget))
  "Set window absolute position relative to snapshot -- and move cursor"
  (let* ((keep 2)
         (bot 0)
         (cur (sw-curabs x))
         (off (sw-offset x))
         (low (+ off keep))
         (lim (sw-limit x))
         (upp (- (+ off lim) keep 1))
         (end (1- (length (sw-all x))))
         (top (- end lim)))
    (multiple-value-bind (wnd abs)
        (cond
          ; HACK: jump to last element
          ((null v) (values (- end lim) (max low (min end cur))))
          ((< v bot) (values bot (max bot (min upp cur))))
          ((<= v top) (values v (max low (min upp cur))))
          (t (values (- end lim) (max low (min end cur)))))
      ; MAYBE:ALT: multiple-value-setq
      (unless (= off wnd) (setf (slot-value x 'offset) wnd))
      (setf (slot-value x 'curabs) abs)
      )))

(defparameter *db* (make-db (myinput)))
; (defparameter *sw* (make-scrollwidget (make-instance 'snapshot :items (db-query *db*)) 10))
(defparameter *sw* (make-scrollwidget (db-query *db*) 10))


(defun runeditor ()
  (shell-out *scr* nil *editor* "--"
             (filespec (item-text (sw-curitem *sw*)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Action

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
    (dolist (line (mapcar 'item-text (coerce (sw-visible *sw*) 'list)))
      (write-line line fd)))
    ;; TODO:CHG: "%m" -> "%r|%s" (ERR?)
    (shell-out *scr* nil *editor* "-c" "setl bt=nofile nowrap efm=%f:%l:%c:%m | cbuffer"
               "--" tmp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; View

(defun drawline (scr i x ww cur off)
  (let* ((text (str:shorten ww (item-text x)))
         (attr (if (= i cur) '(:reverse :bold) '(:normal)))
         (clr (if (eql :key (item-type x)) :terminal :teal))
         (pfx (format nil "~02d| ~02d" i (+ off i))))
    ;; DEBUG:
    ; (nc:submit (croatoan:add-string miur::*scr* "Hey!"))
    ; (nc:submit (format *swank-output* "Hellou!~%"))
    (nc:add-string scr pfx :x 0 :y i :fgcolor :lime :bgcolor :terminal)
    (nc:add-string scr text :x 7 :y i :attributes attr :fgcolor clr :bgcolor :terminal)
    ))

;; DEBUG: (nc:submit (draw *scr*))
(defun draw (scr)
  ; BAD:(not implemented): (nc:window-size *scr*)
  (let ((ww (nc:width scr))
        ; (hh (nc:height scr))
        (off (sw-offset *sw*))
        (cur (sw-currel *sw*)))
    (nc:clear scr)
    (nc:move scr 0 2)
    (loop for x across (sw-visible *sw*) and i from 0
          do (drawline scr i x ww cur off))
    (nc:add-string scr (concatenate 'string "> " *textinput*)
                   :x 0 :y (sw-limit *sw*) :fgcolor :yellow :bgcolor :terminal)
    (nc:move scr cur 6)
    (nc:refresh scr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UIX

;; DEBUG: (nc:submit (shell-out *scr*))
;; FAIL: can't
;; (defun shell-out (scr &rest cmdline &key input &allow-other-keys)
(defun shell-out (scr input &rest cmdline)
  ; cmd = args or [os.environ.get("SHELL", "sh")]
  ; MAYBE: (nc:clear scr)
  (ncurses:def-prog-mode)  ; save current tty modes
  (ncurses:endwin)  ; restore original tty modes
  (format *swank-output* "Shelling out...~%")
  (let ((ret))
    (unwind-protect
      ; ALT:(:interactive): *swank-output* | uiop/stream:*output*
      (setf ret (nth-value 2 (uiop:run-program cmdline
                                               :force-shell nil
                                               :directory (uiop:getcwd)
                                               :input (or input :interactive)
                                               :output :interactive
                                               :ignore-error-status t)))  ; don't error-out if last SHELL cmd had err!=0
      (format *swank-output* "...Restored~%")
      (nc:refresh scr)  ; restore save modes, repaint screen
      (nc:add-string scr (format nil "Returned (~a)" ret) :x (- (nc:width scr) 15) :y 4)
      )
  ))


(defun drawkeyinfo (w ev)
  (declare (ignore w))
  (nc:add-string *scr* (write-to-string
                       (list (nc:event-key ev)
                             (nc:event-code ev)))
                 :x 80 :y 0))


(defun menu-navi (scr key)
  (case key
    ("^D" (sb-ext:quit))
    (#\q (setf *quit* t))  ; OR: (throw scr :exit-event-loop)  // (catch scr ... (do ...))

    (#\c (nc:clear scr) (nc:refresh scr))
    (#\Space (draw scr))
    (#\a (runzshprompt (concatenate 'string " " (item-text (sw-curitem *sw*)))))
    (#\s (runshell))
    (#\Newline (runeditor))
    (#\o (runquickfix))
    (#\/ (setf *menu* 'menu-input))

    ; FIXME: J/K should keep wnd-rel position same
    (#\J (incf (sw-offset *sw*)  1) (draw scr))
    (#\K (incf (sw-offset *sw*) -1) (draw scr))
    (#\j (incf (sw-currel *sw*)  1) (draw scr))
    (#\k (incf (sw-currel *sw*) -1) (draw scr))
    (#\g (setf (sw-curabs *sw*)  0) (draw scr))
    (#\G (setf (sw-curabs *sw*) nil) (draw scr))
    (#\l (incf (sw-currel *sw*)  10) (draw scr))
    (#\h (incf (sw-currel *sw*) -10) (draw scr))
  ))


(defun menu-input (scr key)
  (case key
    (#\Bel (setf *menu* 'menu-navi)) ; = "^G"
    (:backspace (setf *textinput* "")) ; (str:substring 2 t *textinput*)
    ; SEE https://twiserandom.com/lisp/characters-in-lisp-a-tutorial/index.html#Character_comparison
    (otherwise (when (standard-char-p key)
                 (setf *textinput* (concatenate 'string *textinput* (string key)))
                 (draw scr)))
  ))


(defun event-dispatch (scr ev)
  (let ((key (nc:event-key ev)))
    (case key
      (nil t)
      (:resize
        (setf (sw-limit *sw*) (1- (nc:height scr)))
        (nc:add-string scr (format nil "Resize: WxH=~a ~a    "
                                    (nc:width scr) (nc:height scr))
                        :x (- (nc:width scr) 20) :y 1)
        (nc:refresh scr))
      (otherwise (or (funcall *menu* scr key)
                    ; (menu-navi scr key)
                    (drawkeyinfo nil ev))))))
  ; (format *swank-output* "~a~%" (nc:event-key ev))
  ; (princ ev scr)

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
                       ; :input-blocking 100
                       :input-blocking t
                       :bind-debugger-hook nil)
    (setf *scr* scr)
    (setf (sw-limit *sw*) (1- (nc:height scr)))
    ; (setf (nc:attributes scr) '(:bold))
    ; (setf (nc:color-pair scr) '(:yellow :red))
    (nc:clear scr)
    ; (keybinds scr)
    ; (nc:run-event-loop scr)
    (do () (*quit*)
      (let ((ev (nc:get-wide-event scr)))
        (event-dispatch scr ev)))
    ; (print (nc:event-key (nc:get-wide-event scr)))
    )
  (print "=cleanup")
  (finish-output nil))


;; DEBUG: (if (fboundp '_live) (_live))
(defun _live ()
  (nc:clear *scr*)
  (nc:submit (draw *scr*)))
