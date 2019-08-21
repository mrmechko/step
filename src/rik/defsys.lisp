;;;;
;;;; defsys.lisp : Defsystem for the TRIPS Lisp logging routines
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 17 Aug 1999
;;;; $Id: defsys.lisp,v 1.1.1.1 2005/01/14 19:48:08 ferguson Exp $
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(defpackage :rik
  (:use common-lisp)
  (:export 
    sk-set 
    sk-unset
    sk-tag
    sk-tag-list
    make-sk-debugger
    sk-debug
))

(defpackage :func
  (:use common-lisp)
  (:export curry)
)


;(in-package :rik)

(mk:defsystem :rik
  :source-pathname #!TRIPS"src;rik;"
  :components (
	       "debug"
	       "functools"
	       )
)

