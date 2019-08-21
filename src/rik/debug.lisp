(in-package :RIK)
;; sk-tag can be either an explicit sk-tag-struct, a symbol, or a tuple with ('symbol level)

(defgeneric flatten (lst))
(defgeneric condense-all (l) (:documentation "return an all tag if it exists in the list l"))
(defgeneric is-all (l) (:documentation "is an 'all' tag"))

(defgeneric sk-tag (tag &optional level) (:documentation "takes an sk-tag-struct, symbol, or '(symbol int) and returns an sk-tag-struct"))
(defgeneric sk-tag-allowed (reference query) (:documentation "determines if the reference allows a query tag/tag-set through"))
(defgeneric sk-debug-tags (dbg) (:documentation "return the list of tags being tracked by this debugger"))

(defgeneric sk-debug (dbg tag msg &rest values) (:documentation "print a debug message if any of tags are valid in dbg"))

; alteration
(defgeneric sk-alter (dbg tag model) (:documentation "don't call directly.  Generically set or unset tags"))
(defgeneric sk-set (dbg tag) (:documentation "set a tag or list of tags to be tracked by dbg"))
(defgeneric sk-unset (dbg tag) (:documentation "unset a tag or list of tags to be tracked by a dbg"))

(defmethod flatten ((lst sequence)) (mapcan #'flatten lst))
(defmethod flatten ((lst null)) nil)
(defmethod flatten (lst) (list lst))

(defvar *sk-tag-singleton* (make-hash-table :test 'equalp) "make sure only one instance of every unique tag exists")

;; Structs
(defstruct (sk-tag-struct 
	     (:constructor make-sk-tag-hidden (name &optional (level 0)))
	     (:predicate sk-tag-p)
	     (:print-function
	       (lambda (p s k) 
		 (declare (ignore k))
		 (if (= (sk-tag-struct-level p) 0) 
		   (format s "DBG-~S" (sk-tag-struct-name p))
		   (format s "DBG-~S(~S)" (sk-tag-struct-name p) (sk-tag-struct-level p))
	         )))
	     :named
	   ) 
  (name 'all :type symbol) (level 0 :type integer)
)

(defstruct (sk-debug-system
	     (:constructor make-sk-debugger (&optional (tags (sk-tag-list 'misc 'default)) (output-stream t) (verbose t)))
	     (:predicate sk-debugger-p)
	     (:print-function
	       (lambda (p s k)
		 (declare (ignore k))
		 (if (sk-debug-system-verbose p) 
		   (format s "(sk-debugger ~A)" (sk-debug-tags p))
		   (format s "(sk-debugger ~S)" (fmt-truncate-list (sk-debug-tags p) :trunc 2))
		 )))
	     :named
	   )
  (tags (sk-tag-list 'misc 'default) :type list) (verbose nil) (output-stream t) (cache (make-hash-table :test 'equalp))
)


; predicates for sk-tag-struct

(defun list-of-sk-tag-p (tags) (every #'sk-tag-p tags))
(defun sk-tag-shape-p (tag) 
  "check to see if tag is a valid sk-tag shape (sk-tag-struct, symbol or (symbol int)"
  (or
    (sk-tag-p tag)
    (symbolp tag)
    (and (listp tag) (= (length tag) 2) (symbolp (car tag)) (integerp (cadr tag)))
  )
)

(defun list-of-sk-tag-shape-p (tags) (every #'sk-tag-shape-p tags))

; constructors etc for sk-tag-struct
(defun make-sk-tag (name &optional (level 0)) 
  "check the cache and then return the tag"
  (let* ((premade (gethash (list name level) *sk-tag-singleton*))) 
    (progn 
      (if premade 
	  premade 
	  (setf (gethash (list name level) *sk-tag-singleton*) (make-sk-tag-hidden name level))
      )
    )
  )
)

;(defun sk-tag-list (&rest l)
;  (cond
;	((null l) '())
;	((sk-tag-shape-p l) (sk-tag l))
;	((listp l) (append (sk-tag-list (car l)) (sk-tag-list (cdr l))))
;	(t '())
;    )
;  )

(defun sk-tag-list (&rest l) 
  "ensure a list is a list of sk-tags"
  (cond 
       ((null l) (make-sk-tag 'all -1))
       ((sk-tag-shape-p l) (list (sk-tag l)))
       ((and (listp l) (list-of-sk-tag-shape-p l)) (map 'list #'sk-tag l))
       ((and 
	  (listp l) 
	  (listp (car l)) 
	  (list-of-sk-tag-shape-p (car l))) 
	(map 'list #'sk-tag (car l)))
       (t (list (make-sk-tag 'all -1)))
))

(defun sk-tag-name (tag) 
  (let 
    ((name (sk-tag-struct-name (sk-tag tag))))
    (if name name 'all)
  )
)

(defun sk-tag-level (tag) 
  (sk-tag-struct-level (sk-tag tag))
)

; ensure tag-types
(defmethod sk-tag ((tag sk-tag-struct) &optional level) 
  (declare (ignore level))
  tag
)

(defmethod sk-tag ((tag null) &optional (level 0))  (make-sk-tag 'NONE level))
(defmethod sk-tag ((tag symbol) &optional (level 0))  (make-sk-tag tag level))
(defmethod sk-tag ((tag list) &optional level) 
  "some error checking here because I don't want to allow general lists"
  (declare (ignore level))
  (handler-case (make-sk-tag (car tag) (cadr tag))
    (type-error (c) 
      (let ((a (sk-tag nil)))
      	(format t "~%error: ~A~% reading sk-tag in ~A.~%returning ~A" c tag a)
	a
      )
    )  
  )
)

(defmethod is-all ((l sk-tag-struct)) (if (eq (sk-tag-name l) 'all) l nil))
(defmethod is-all (l) (if (sk-tag-shape-p l) (is-all (sk-tag l)) nil))

(defmethod condense-all ((l list)) 
  (or (is-all (car l)) 
    (condense-all (cdr l)))
)

(defmethod condense-all ((l null)) nil)

(defmethod sk-tag-allowed ((reference list) (query list)) 
  ; if called on two lists, we want to return the satisfying members of the query list
  (let* (
	(ref (sk-tag-list reference)) 
	(q (sk-tag-list query))
	(result (remove-if-not #'(lambda (x) (sk-tag-allowed ref x)) q))
       )
    (cond 
      (result (values t result))
      (t (values nil nil)))
  )
)

(defmethod sk-tag-allowed ((reference list) (query sk-tag-struct))
  ; middle of recursion.  ugly
  (values-list (if (sk-tag-shape-p reference) 
    (multiple-value-list (sk-tag-allowed (sk-tag reference) query))
    (let ((cartest (multiple-value-list (sk-tag-allowed (sk-tag (car reference)) query))))
      (if (car cartest) cartest
	(multiple-value-list (sk-tag-allowed (cdr reference) query)))
    )
  ))
)

(defmethod sk-tag-allowed ((reference null) (query sk-tag-struct)) (values nil nil)) ; base case

(defmethod sk-tag-allowed ((reference sk-tag-struct) (query sk-tag-struct))
  ; individual comparison
  (let (
     (res-name (eq (sk-tag-name reference) (sk-tag-name query)))
     (res-level (or (= -1 (sk-tag-level reference)) (= -1 (sk-tag-level query)) (>= (sk-tag-level reference) (sk-tag-level query))))
     (res-all (or (is-all reference) (is-all query)))
    )
    (cond 
      ((and res-level res-all) (values t query))
      ((and res-name res-level) (values (and res-name res-level) query)) 
      (t (values nil nil))
    )
  )
)

(defun fmt-truncate-list (lst &key (trunc 2))
  (let* (
	 (ll (length lst))
	 (truncated (if (> ll trunc) (append (subseq lst 0 trunc) '("...")) lst))
	)
    (format nil "~A" truncated)
))

(defmethod sk-dbg-stream ((dbg sk-debug-system)) (sk-debug-system-output-stream dbg))

(defmethod sk-debug-tags ((dbg sk-debug-system)) (sk-debug-system-tags dbg))

(defun lookup-or-compute (dbg q)
  (let 
    ((exists (gethash q (sk-debug-system-cache dbg))))
      (if exists
       exists
       (setf (gethash q (sk-debug-system-cache dbg)) (nth-value 1 (sk-tag-allowed (sk-debug-tags dbg) q)))
      )))

(defmethod sk-tag-allowed ((reference sk-debug-system) query) 
  ; get the reference list and carry on
  (let* (
	(nq (sk-tag-list query))
	(result (delete nil (map 'list #'(lambda (q) (lookup-or-compute reference q)) nq)))
       )
    (if result
      (values t result)
      (values nil nil)
      )
  )
)


(defmethod sk-alter ((dbg sk-debug-system) (tag null) (mode symbol)) nil)
(defmethod sk-alter ((dbg sk-debug-system) (tag list) (mode symbol)) 
  (if (sk-tag-shape-p tag) 
    (sk-alter dbg (sk-tag tag) mode)
    (progn 
      (sk-alter dbg (cdr tag) mode)
      (sk-alter dbg (car tag) mode)
    )
  )
)

(defmethod sk-alter ((dbg sk-debug-system) (tag sk-tag-struct) (mode symbol))
  (progn 
    (setf (sk-debug-system-cache dbg) (make-hash-table :test 'equalp))
    (setf (sk-debug-system-tags dbg) (remove-if #'(lambda (x) (eq (sk-tag-name x) (sk-tag-name tag))) (sk-debug-tags dbg)))
    (if (eq mode 'set) (setf (sk-debug-system-tags dbg) (append (sk-debug-system-tags dbg) (list tag))))
    dbg
  )
)

(defmethod sk-set ((dbg sk-debug-system) tag) 
  "set tag level"
  (sk-alter dbg tag 'set)
)

(defmethod sk-unset ((dbg sk-debug-system) tag)
  "remove tag"
  (sk-alter dbg tag 'unset)
)

(defmethod sk-debug ((dbg null) tag msg &rest values) nil)
(defmethod sk-debug ((dbg sk-debug-system) tag msg &rest values)
  "if a tag is provided print the message if the tag is in or a subset of dbg.  if tag is nil, print the message always"
  (let* (
    (the-tag (nth-value 1 (sk-tag-allowed dbg (sk-tag-list tag))))
    (has-all (condense-all the-tag))
    )
    (let* (
	(header (format nil "~%~A - " (or has-all the-tag)))
      	(body (apply #'format `(,nil ,msg ,@values)))
      )
      (if the-tag (format (sk-debug-system-output-stream dbg) "~A~A" header body))
    )
  )
)

(defvar *DBG* (make-sk-debugger))
