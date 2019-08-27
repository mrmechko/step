(in-package "PARSER")

(format *error-output* "Skeletons!~%")

(defvar +dbg-general+ (rik::sk-tag-list 'default 'misc))
(defvar +dbg-params+ nil)

(defvar +dbg-functions+
  (rik::sk-tag-list
        'default
;; utility functions
	'sstring 
	'stringify-elements-first-two
;; set/clear map
	;'clear-skeleton-map
	;'set-skeleton-map
;; adjust probability
	'adjust-prob-based-on-skeleton-score
	;'skel-range
	;'convert-to-adjustment-factor
;; check spans/overlap
	;'point-in-span
	'span-overlap
	;'constit-span
	;'constit-overlap
;; basic structure for book-keeping
	;'format-key-pair
	;'format-wconstit
;; drivers for function
	;'compute-skeleton 
	'(compute-skeleton 1)
	;'build-semantic-skeleton
	'(build-semantic-skeleton 1)
;; lookups from caches
	'lookup-from-skeleton-map
	'(lookup-from-skeleton-map 1)
	;'get-simple-type
	;'test-element ; 1
	'(test-element 1)
	;'check-wsd-map
	'(check-wsd-map 1)
	'get-wsd-data
	'get-cached-score
	'compute-skeleton-score
    )
  )

(defun def-or-config (backup)
  "Loads dbg settings from the code or from a config file"
  (let ((param-file #!TRIPS"dbg-params.lisp"))
    (format t "PARAMETERS: ~S" param-file)
    (if (and param-file (probe-file param-file)) (load param-file))
    (if +dbg-params+ (setf +dbg-params+ (rik::sk-tag-list +dbg-params+)))
    (or +dbg-params+ backup)
    )
  )

(defun as-ont (s)
  (cond ((symbolp s) (intern (symbol-name s) :ont))
	((stringp s) (intern s :ont))
	(t nil)
	)
  )

(defvar *skel-dbg* nil)
(defvar *skeleton-wsd-hierarchy* t)

(defun skdbg (tags msg &rest args) 
  "wraps debug configuration loading and the debugger itself"
  (if (not *skel-dbg*) 
    (setf *skel-dbg* (rik::make-sk-debugger (def-or-config +dbg-params+) *error-output*))
  )
  (apply #'rik::sk-debug `(,*skel-dbg* ,tags ,msg ,@args))
)



(defun sstring (q) 
  "convenience. make sure what comes out is a string"
  (cond 
    ((stringp q) q)
    ((symbolp q) (symbol-name q))
    ((listp q) (sstring (cadr q)))
    (t q)
    )
  )

(defvar *max-semantic-skeleton-factor* 1.20)
(defvar *min-semantic-skeleton-factor* .80)
(defvar *semantic-skeleton-scoring-enabled* nil)

(defvar *semantic-skeleton-map* nil)
(defvar *semantic-skeleton-map-filled* nil)

(defvar *var-type-map* nil) ; cache
(defvar *semantic-skeleton-score-factor* .1) ;; currently ignored
(defvar *essential-roles* 
  '(ont::agent ont::agent1 
    ont::affected ont::affected1 
    ont::neutral ont::neutral1 
    ont::formal 
    ont::result ont::affected-result ont::transient-result
    ont::of ont::val 
    ont::figure ont::ground 
    ont::experiencer 
    ont::source))

(defun clear-skeleton-map ()
  "clear the skeleton map and mark it as empty"
    (skdbg 'clear-skeleton-map "clearing skeleton map")
    (setf *var-type-map* nil)
    (setf *semantic-skeleton-map* nil)
    (setf *semantic-skeleton-map-filled* nil)
)

(defun stringify-elements-first-two (a)
  "convenience: take a list of at least two elements and return a list with the first two elements converted to strings"
  (append (list (symbol-name (car a)) (symbol-name (cadr a))) (cddr a))
  )

(defun set-skeleton-map (content) 
	(skdbg 'set-skeleton-map "setting skeleton map to ~S" content)
	(skdbg 'set-skeleton-map "adjustment range is (~A ~A)" *max-semantic-skeleton-factor* *min-semantic-skeleton-factor*)
	(setf *semantic-skeleton-map* (map 'list #'stringify-elements-first-two content))
	(setf *semantic-skeleton-map-filled* t)
)

;; set this variable to a set of scored skeleton debugging
(defvar *debug-skeleton-info* ;; ignored
  nil)

;; set this to T if you want the skeletons generated for a sentence
(defvar *generate-skeletons* t) ;; ignored

;; Functions

; adjust-prob-based-on-skeleton-score: 
;   takes a probability and a skel tuple ? and returns the adjusted score

; lookup-ont-type-from-skeleton-map
;   takes a constituent id and returns the ont type for it.  If the entry is in var-type-map get it from there

; compute-skeleton
;   given a constituent, does a bunch of stuff, returns a skel-typle

(defun adjust-prob-based-on-skeleton-score (prob skel)
  "take a probability and a skel entry (consisting of (info coefficient)) and adjust probability by coefficient"
  (skdbg 'adjust-prob-based-on-skeleton-score "rq: adjust ~S by ~S..." prob skel)
  (if (null skel)
      prob
      (let ((score (skel-range (second skel)))) ;
	(if (and (second skel) (< 0 (second skel))) 
	  (progn
		(skdbg 'adjust-prob-based-on-skeleton-score "adjusting ~S by ~S" prob score)
		(* prob score)    
	    )
	  prob ; skip the ones that aren't scored
	  )
	)))

(defun skel-range (b) 
  "take a skeleton score and transform it into a coefficient"
  (if (numberp b) 
  	(+ *min-semantic-skeleton-factor* (* b (- *max-semantic-skeleton-factor* *min-semantic-skeleton-factor*))) 
	1
))

(defun point-in-span (p s2)
  "Check if a point, p is in a span s2"
	(and (>= p (first s2)) (<= p (second s2)))
)

(defun span-overlap (s1 s2)
  "Check if two spans overlap"
	(progn
		(skdbg 'span-overlap "span check: ~S overlaps ~S" s1 s2)
		(or 
			(or (point-in-span (first s1) s2) (point-in-span (second s1) s2))
			(or (point-in-span (first s2) s1) (point-in-span (second s2) s1))
		)
	)
)

(defun constit-span (c1) 
  "extract a span from a wconstit struct"
  (list (wconstit-start c1) (wconstit-end c1))
)

;(defun constit-overlap (c1 c2) 
;  "check if two wconstit structs have overlapping spans"
;  (span-overlap (constit-span c1) (constit-span c2))
;)
;
;(defstruct wconstit id class (word nil) (syncat nil) (start -1) (end -1)) ;; wconstit is currently not being used at all.
;
;(defun format-key-pair (l)
;  "Mainly for outputing json"
;	(let 
;	((s (make-array 0
;		:element-type 'character
;		:adjustable t
;		:fill-pointer t)))
;	(format 
;		s 
;		"\{~{~{\"~S\":\"~S\",~}~}\}" 
;		(values l)
;	)
;	s)
;)
;
;(defun format-wconstit (p) ;; TODO: embrace  ;; TODO: embrace the kqml and return a whole kqml nested list and jsonify on python side
;  "format a wconstit object into a json object.  Currently unused"
;	(format-key-pair (list (list `id (wconstit-id p))
;		(list `class (wconstit-class p))
;		(list `word (wconstit-word p))
;		(list `syncat (wconstit-syncat p))
;		(list `start (wconstit-start p))
;		(list `end (wconstit-end p))))
;)
;
(defun lookup-from-skeleton-map (q) ;; should eventually take a key
  "Lookup a cached score for a constituent. Assumes that a constituent doesn't get appended to an existing constituent..."
(let 
  ((res (sstring q)))
  (progn  
    (skdbg 'lookup-from-skeleton-map "lookup: ~S gives ~S" res (assoc res *var-type-map*))
    (if *var-type-map* (skdbg '(lookup-from-skeleton-map 1) "var-type-map looks like ~S" (nth (random (length *var-type-map*)) *var-type-map*)))
    (if 
      (assoc res *var-type-map*) 
      (values (cdr (assoc res *var-type-map*)) t)
      (values 0 nil)
  ))) 
)
;
;(defun get-simple-type (c)
;  (let ((type (if (consp c) (cadr c) c)))
;    (if (symbolp type)
;	type
;	'bad-type)))

(defun compute-skeleton (constit start end)
  (when 
    (member (constit-cat constit) '(w::s w::vp w::np w::adjp w::advbl)) ; if constit-cat is one of these
      (let* ((lf (get-fvalue (constit-feats constit) 'w::lf)) ; get feature value of w::lf from  constit-feats
	     (complete-class (get-value lf 'w::class)) ; get the complete-class name
	     (class (if (consp complete-class) (second complete-class) complete-class)) ; if class is consd, take the second item
	     (var (get-value lf 'w::var))
	     (constraint (if lf (get-value lf 'w::constraint)))
	     (role-pairs (if (and constraint (not (eq constraint '-))
				  (not (var-p constraint)))
			     (constit-feats constraint)))
	     ;; find all essential roles, convert unbound values to constant <VAR>
	     (found-bound-role nil)
	     (roles (mapcar #'(lambda (d)
				   (if (var-p (cadr d)) 
				       (list (car d) '<var>)
				       (progn 
					 (setq found-bound-role t)
					      d)))
			       (remove-if-not #'(lambda (e) (member (car e) *essential-roles*)) role-pairs)))
	     )
	     (progn
	     	(skdbg '(compute-skeleton 1) "skeleton map is ~S" *semantic-skeleton-map*)

		(let (
		      (skel (build-semantic-skeleton var complete-class (if found-bound-role roles) (constit-cat constit) start end))
		      ) 
		  	;; only pass in roles if there is at least one bound value 
			(skdbg '(build-semantic-skeleton 1) "SKEL: ~S" skel)
			(if (and (second skel) (> 0 (second skel))) 
			  (progn
			  	(skdbg '(build-semantic-skeleton 1) "adjusting ~S by ~S" (first skel) (second skel))
		  	  	(setf *var-type-map* (remove-if #'(lambda (f) (and (> (second skel) (third f)) (equal (first f) var))) *var-type-map*))
				(skdbg '(lookup-from-skeleton-map 1) "Pushing ~S to *var-type-map*" (append (list (sstring var)) skel))
		  	  	(push (append (list (sstring var)) skel) *var-type-map*)
			  )
			)
			skel
			)
	     ))))

(defun subtype-check-wrapped (e1 e2) 
  (skdbg '(subtype-check-wrapped 2) "checking ~S against ~S" e1 e2)
 (let 
   ((res (subtype-check (as-ont (first e1)) (as-ont (first e2)))))
   (if res 
     (if (not (equalp (first e1) (first e2)))
     	(skdbg '(subtype-check-wrapped 1) "~S is a child of ~S" (first e1) (first e2))
     	(skdbg '(subtype-check-wrapped 1) "~S matches" (first e1))
     )
     ;(skdbg 'subtype-check-wrapped "~S is not a child of ~S" (second e1) (second e2))
     )
   res
   )
  )

(defun test-element (e1 e2)
  ;; Need to check to see if a word isn't getting a wsd entry
  ;; if no entry is given, it should be removed from consideration
  (if *skeleton-wsd-hierarchy* 
    (skdbg '(test-element 1) "comparing constit ~S - ~S using hierarchy" e1 e2)
    (skdbg '(test-element 1) "comparing constit ~S - ~S" e1 e2)
  )
  (let (
	(tps (if *skeleton-wsd-hierarchy* 
	       (subtype-check-wrapped e1 e2)
	       (equalp (first e1) (first e2)))) 
    	(spns (span-overlap (car (last e1)) (car (last e2)))) 
    	(word (equalp (first e1) (first e2)))
    )
    ;(if (and tps word spns) (skdbg '(test-element 1) "comparing ~S - ~S : ~S" e1 e2 (list tps word spns)))
    (and tps spns word)
    )
)

(defun check-wsd-map (cls lex span &optional (against *semantic-skeleton-map*))
  "Checks to see if the (cls lex span) triple matches something we've already seen."
  ;; ideally, could have a bunch of different word hints come in as a dictionary and this function could look them up by key
  (skdbg '(check-wsd-map 1) "looking for: ~S ~S ~S" cls lex span)
  (skdbg '(check-wsd-map 1) "in: ~S" against)
  (if against 
    (if (test-element (list (symbol-name cls) (symbol-name lex) span) (car against))
       1 ;; NOTE: this should be returning the probability but it is returning a 1 for now
       (check-wsd-map cls lex span (cdr against))
    )
    nil
    )
)


(defun get-wsd-data ()
  ;; Gets data from SkeletonScore python module if the list is empty
  ;; Should add a guard that gets replaced at reset
  (skdbg 'get-wsd-data "semantic-skeleton-map filled: ~S" *semantic-skeleton-map-filled*)
  (if (not *semantic-skeleton-map-filled*) 
    (let* (
	   (reply (send-and-wait `(REQUEST :content (get-wsd-data)))))
      (skdbg 'get-wsd-data "recieved reply ~S" reply)
      (set-skeleton-map (cdr reply))
)))


(defun build-semantic-skeleton (id class roles syncat start end)
  ;; this builds the skeleton and adds to skeleton-map and the var-map if necessary"
  (get-wsd-data)
  (skdbg '(build-semantic-skeleton 1) "all-roles: ~S" roles)
  (let* (
	  (word (if (consp class) (caddr class) '())) ;; get word and class (syncat provided)
	  (rolescores (map 'list #'(lambda (res) (first res)) (remove-if-not 'cadr (mapcar 'get-cached-score roles))))
    	  (cls (if (consp class) (cadr class) class))
	) 
    	(skdbg '(build-semantic-skeleton 1) "Rolescores: ~S" rolescores)
	 (if (member word (map 'list #'second *semantic-skeleton-map*) :test #'string=) 
	  (let ((result (check-wsd-map cls word (list start end))))
	    (if result
		  (list (list cls word) (compute-skeleton-score (map 'list #'second rolescores) result)) ;; This 1 should be replaced with the raw score
		  (list (list cls word) (compute-skeleton-score (map 'list #'second rolescores) 0)))
	  ) 
	  ;; This seems to be penalizing constituents even if they aren't being advised with wsd information
	  ;; at the same time, might not matter because every non-advised constituent would get equally downgraded
	  ;; Also, this does reduce the impact of non-advised constituents that take inadvisable arguments.
	)
))

(defun get-cached-score (id) 
  (skdbg 'get-cached-score "~S" id)
  (multiple-value-bind  
    (score exist) 
    (lookup-from-skeleton-map id) 
    (progn 
      (skdbg 'get-cached-score "(~S, ~S)" score exist)
      (list score exist)
)))

(defun compute-skeleton-score (r score)
  (let ((s (+ score ( apply #'+ r)))
	 (size (+ 1 (length r))) 
	 )
    (skdbg 'compute-skeleton-score "score is ~S" s)
    (/ s size)
    )
  )


(defun convert-to-adjustment-factor (adj)
   (if (numberp adj) (max (min adj *max-semantic-skeleton-factor*) *min-semantic-skeleton-factor*)
       1))
