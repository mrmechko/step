;;;;
;;;; File: Systems/STEP/system.lisp
;;;; Creator: George Ferguson
;;;; Created: Wed Jul 11 12:51:11 2007
;;;; Time-stamp: <Sat Feb  4 08:17:58 EST 2017 jallen>
;;;;
;;;; Defines and loads an instance of the TRIPS system.
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;Systems;core;system")

(trips:def-trips-system :step
  (:old-trips-component	:lxm	#!TRIPS"src;LexiconManager;")
  (:dfc-component	:parser	#!TRIPS"src;Parser;")
  (:dfc-component	:im	#!TRIPS"src;NewIM;")
  )

;; add WebParser to the system when we have its source directory
(when (probe-file #!TRIPS"src;WebParser")
  (nconc (assoc :step trips::*trips-systems*)
	 (list '(:dfc-component :webparser #!TRIPS"src;WebParser;"))))

;; Now load the system
(trips:load-trips-system)

;;;
;;; After-load customizations for this TRIPS system
;;; Copied from plow, 10 Mar 2008
;;;

;; Set to T to run w/o web learning
;;(setq weblearn::*always-accept* nil)

;; Set to DEBUG if you want some output
;;(setq palm::*debug-level* 'error)


(setq parser::*parser-init-settings*
      '((parser::*in-system* :step)
	(parser::*word-length* 7)      ;; guess and average number of letters in a word
;	(parser::*score-length-multiplier* 1)   ;;  boost factor for constituent size
	(parser::*score-length-multiplier* 0.4)   ;;  boost factor for constituent size
	(parser::*score-corner-multiplier* 0)    ;; not clear this is helpful
	(parser::*use-tags-as-filter* t)        ;;  indicate we should use POS information
	(parser::*bad-tag-multiplier* .98)       ;;  penalty multiplier for lex entries that do not match POS tags
	(parser::*skeleton-constit-cats* '(W::NP W::CP W::VP W::ADVBL W::S))  ;; constituents that we expect in the skeleton
	(parser::*skeleton-boost-factor* 1.01)   ;;  boost if we build a constituent that matches the skeleton (from stat. parser)
	;;(parser::*remove-subsumed-skeleton-constit* t)  ;; remove subsumed constits in skeleton (e.g., NP within NP) as they are unreliable predictors
	((setf (parser::barrier-penalty parser::*chart*) .99))        ;; this is the penalty for arcs that attempt to cross barriers from the preferences from stat parser
	(parser::*kr-type-info-desired* '(:WNsense))
	(parser::*no-positions-in-lf* nil)       ;; generate start and end positions
	((parser::setmaxnumberentries 5000))    ;;  # constituents built before stopping
        ((parser::setmaxchartsize 5000))        ;;  max #  characters in any input
	((setf (parser::number-parses-to-find parser::*chart*) 10))
	((setf (parser::flexible-semantic-matching parser::*chart*) t))  ;;  selection preferences rather than restrictions
	((setf (parser::number-parses-desired parser::*chart*) 4))  ;; get # interpretations before stopping
	(parser::*include-parse-tree-in-messages* '(w::lex)) ;; required for WebParser
	(parser::*semantic-skeleton-scoring-enabled* t) ; enable semantic scoring
	((parser::customize-cost-table '((ont::SA_QUERY 1.2) (ont::SA_IDENTIFY 2) (ont::SA_pred-fragment 2) 
					 (ont::SA_request 1.2) (ont::SA_YN-QUESTION 1.2)
					 (ont::SA_CONFIRM 1.3) (ont::SA_WH-QUESTION 1.2) (ont::SA_TELL 1)(w::CP 2) (w::VP 2) 
					 (w::punc .5))))
	))

(parser::initialize-settings)   ;; do it here to set the parser for testing (so it matches the settings of the parser component)

  (setq im::*max-allowed-utts-in-turn* 20)  ;; basically try to do something with everything
  (setq im::*external-name-resolution* nil) ;; no domain-specific reasoner
  ;;(setq im::*tma-suppress-list* '(w::passive)) ;; w::progr))  ;; suppress passive and progressive indicators
  (setq im::*show-lf-graphs* t)    ;; turn on LF-graph displays for debugging
  (setq im::*no-BA-mode* t)        ;; there is no behavioral agent in STEP

;; STEP uses text-tagger
(setq *use-texttagger* t)
(setq im::*current-dialog-manager* #'im::textIM)
(setq im::*output-format* 'im::LF)
  
(defun parse-eval (x)
  (im::send-msg `(request :receiver parser :content (eval ,x))))

;;(load  #!TRIPS"src;Systems;step;adj.lisp")
;;(load  #!TRIPS"src;Systems;step;attributes.lisp")

;;(format t "~% LF SEM on ONT::INSIPID is ~S" (om::lf-sem 'ONT::INSIPID))

;;(setf wf::wordnet-synset-to-ont-type-mappings (make-hash-table :test #'equalp))
;;(wf::make-synset-to-ont-type-table)

;;(format t "~% LF SEM on ONT::INSIPID hash is ~S is ~S" (gethash 'ont::insipid (om::ling-ontology-lf-table om::*lf-ontology*)) (om::lf-sem 'ONT::INSIPID))
