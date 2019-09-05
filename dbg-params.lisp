(in-package :PARSER)

(setf +dbg-params+ '(
	default
;; utility functions
	;sstring 
	;stringify-elements-first-two
;; set/clear map
	;clear-skeleton-map
	set-skeleton-map
	;(subtype-check-wrapped 1)
;; adjust probability
	adjust-prob-based-on-skeleton-score
	;skel-range
	;convert-to-adjustment-factor
;; check spans/overlap
	;point-in-span
	;span-overlap
	;constit-span
	;constit-overlap
;; basic structure for book-keeping
	;format-key-pair
	;format-wconstit
;; drivers for function
	;compute-skeleton 
	;(compute-skeleton 1)
	;build-semantic-skeleton
	;(build-semantic-skeleton 1)
;; lookups from caches
	;lookup-from-skeleton-map
	;(lookup-from-skeleton-map 1)
	;get-simple-type
	;test-element ; 1
	;(test-element 1)
	;query-lex-advice
	;(query-lex-advice 1)
	;get-wsd-data
	;count-wsd-checks
	;score-wsd
	;get-cached-score
))

(setf *max-semantic-skeleton-factor* 1.1)
(setf *min-semantic-skeleton-factor* 0.9)

(setf *skeleton-wsd-hierarchy* t)
(setf *dbg-output-stream* t)
(format *dbg-output-stream* "DBG_PARAMS IS ~A" +dbg-params+)
