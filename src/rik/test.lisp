(load (make-pathname :directory '(:relative :up "config" "lisp") :name "trips")) 
(load #!TRIPS"src;rik;defsys") 

(mk:load-system :rik)

(defvar +dbg-functions+
  (rik::sk-tag-list
    'default
;; utility functions
	;'sstring 
	;'stringify-elements-first-two
;; set/clear map
	'clear-skeleton-map
	'set-skeleton-map
;; adjust probability
	'adjust-prob-based-on-skeleton-score
	;'skel-range
	'convert-to-adjustment-factor
;; check spans/overlap
	'point-in-span
	;'span-overlap
	;'constit-span
	;'constit-overlap
;; basic structure for book-keeping
	;'format-key-pair
	;'format-wconstit
;; drivers for function
	;'compute-skeleton 
	;'(compute-skeleton 1)
	;'build-semantic-skeleton
	;'(build-semantic-skeleton 1)
;; lookups from caches
	;'lookup-from-skeleton-map
	;'get-simple-type
	;'test-element ; 1
	'(test-element 1)
	;'check-wsd-map
	'(check-wsd-map 1)
	;'get-wsd-data
	'count-wsd-checks
	;'score-wsd
    )
  )


