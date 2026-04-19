
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
