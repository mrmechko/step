;;;;
;;;; defsys.lisp : Defsystem for the TRIPS Lisp logging routines
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 17 Aug 1999
;;;; $Id: defsys.lisp,v 1.1.1.1 2005/01/14 19:48:08 ferguson Exp $
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(defpackage :dbg
  (:use common-lisp)
  (:export 
    sk-set 
    sk-unset
    sk-tag
    sk-tag-list
    make-sk-debugger
    sk-debug
))

(in-package :dbg)


(mk:defsystem :dbg
  :source-pathname #!TRIPS"src;dbg;"
  :components ((:file "debug"))
)
