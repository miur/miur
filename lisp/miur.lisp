;;
;; WORKFLOW:
;;   * run slime/swank in separate terminal $ ./ctl s
;;   * add/modify sexpr
;;   * <, e f> to eval top-level sexpr under cursor
;;
(in-package #:miur)

(defstruct wmstate
  display
  screen
  root)

(defparameter *mywm* (make-wmstate))
(defparameter *quit* nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utils
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym))) syms)
     ,@body))

(defmacro defrestart (name)
  (let ((fname (intern (concatenate 'string "RESTART-" (symbol-name name)))))
    (with-gensyms (c restart)
      `(defun ,fname (,c)
         (let ((,restart (find-restart ',name)))
           (when ,restart
             (invoke-restart ,restart ,c)))))))

(defrestart window-error)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Launch
;; INFO:
;; Common Lisp: uiop:run-program outputs but uiop:launch-program does not - Stack Overflow ⌇⡞⣔⠆⡤
;;   https://stackoverflow.com/questions/57720432/common-lisp-uioprun-program-outputs-but-uioplaunch-program-does-not

(defun ps ()
  (let ((output (uiop:run-program '("ls" "-l") :output :string)))
    (loop for line in (rest (cl-ppcre:split "(\\n+)" output))
          collect (cl-ppcre:split "(\\s\\s+)" line))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main

(defun main (mywm) ;&key (myvar 2000))
  (declare (ignore mywm))
  ; (format t "~A~%" myvar)
  (print "=main")
  (finish-output nil)
  (unwind-protect
       (progn
         (print "=loop")
         )
    (print "=cleanup"))
  ;; (print myvar)
  (finish-output nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run
(eval-when (:execute)
  (progn
    (print "-----")
    (print "=start")
    (finish-output nil)
    (main *mywm*)
    (print "=end")
    (finish-output nil)))
