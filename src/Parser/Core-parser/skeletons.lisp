(in-package "PARSER")

(format *error-output* "Skeletons!~%")

(defvar +dbg-general+ (rik::sk-tag-list 'default 'misc))
(defvar +dbg-wsd+ (rik::sk-tag-list 'wsd-lookup 'wsd-check 'spans))
(defvar +dbg-cache+ (rik::sk-tag-list 'set-map 'clear-map 'semantic-skeleton-cache))
(defvar +dbg-skeleton-score+ (rik::sk-tag-list 'build-skeletons 'skeleton-prob))
(defvar +dbg-skeleton-score-mini+ (rik::sk-tag-list 'skeleton-prob1 'build-skeletons2 'set-map 'rolescores )) ;telement
(defvar +dbg-misc+ (rik::sk-tag-list 'misc))

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
	;('test-element 1)
	;'check-wsd-map
	;('check-wsd-map 1)
	;'get-wsd-data
	'count-wsd-checks
	;'score-wsd
    )
  )

(defvar *skel-dbg* (rik::make-sk-debugger +dbg-functions+ *error-output*))

(defun skdbg (tags msg &rest args) (apply #'rik::sk-debug `(,*skel-dbg* ,tags ,msg ,@args)))
(skdbg 'default "~A" *skel-dbg*)

(defun sstring (q) 
  (cond 
    ((stringp q) q)
    ((symbolp q) (symbol-name q))
    ((listp q) (sstring (cadr q)))
    (t q)
    )
  )

(defvar *max-semantic-skeleton-factor* 1.05)
(defvar *min-semantic-skeleton-factor* .95)
(defvar *semantic-skeleton-scoring-enabled* nil)
(defvar *semantic-skeleton-map* nil)
(defvar *semantic-skeleton-map-filled* nil)
(defvar *var-type-map* nil) ; cache
(defvar *semantic-skeleton-score-factor* .1)
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
    (skdbg 'clear-skeleton-map "clearing skeleton map")
    (setf *var-type-map* nil)
    (setf *semantic-skeleton-map* nil)
    (setf *semantic-skeleton-map-filled* nil)
)

(defun stringify-elements-first-two (a)
  (append (list (symbol-name (car a)) (symbol-name (cadr a))) (cddr a))
  )

(defun set-skeleton-map (content) 
	(skdbg 'set-skeleton-map "setting skeleton map to ~S" content)
	(setf *semantic-skeleton-map* (map 'list #'stringify-elements-first-two content))
	(setf *semantic-skeleton-map-filled* t)
)

;; set this variable to a set of scored skeleton debugging
(defvar *debug-skeleton-info* 
  nil)

;; set this to T if you want the skeletons generated for a sentence
(defvar *generate-skeletons* t)

;; Functions

; adjust-prob-based-on-skeleton-score: 
;   takes a probability and a skel tuple ? and returns the adjusted score

; lookup-ont-type-from-skeleton-map
;   takes a constituent id and returns the ont type for it.  If the entry is in var-type-map get it from there

; compute-skeleton
;   given a constituent, does a bunch of stuff, returns a skel-typle

(defun adjust-prob-based-on-skeleton-score (prob skel)
  (if (null skel)
      prob
      (let ((score (skel-range (second skel)))) ;
	(if (not (third skel)) prob ; skip the ones that aren't scored
	  (progn
		(skdbg 'adjust-prob-based-on-skeleton-score "adjusting ~A by ~A" prob score)
		(* prob score)    
	    ))
	)))

(defun skel-range (b) 
  (if (numberp b) 
  	(+ *min-semantic-skeleton-factor* (* b (- *max-semantic-skeleton-factor* *min-semantic-skeleton-factor*))) 
	1
))

(defun point-in-span (p s2)
	(and (>= p (first s2)) (<= p (second s2)))
)

(defun span-overlap (s1 s2)
	(progn
		(skdbg 'span-overlap "span check: ~S overlaps ~S" s1 s2)
		(or 
			(or (point-in-span (first s1) s2) (point-in-span (second s1) s2))
			(or (point-in-span (first s2) s1) (point-in-span (second s2) s1))
		)
	)
)

(defun constit-span (c1) (list (wconstit-start c1) (wconstit-end c1)))

(defun constit-overlap (c1 c2) (span-overlap (constit-span c1) (constit-span c2)))

(defstruct wconstit id class (word nil) (syncat nil) (start -1) (end -1))
(defstruct wquery info (roles nil))

(defun format-key-pair (l)
	(let 
	((s (make-array 0
		:element-type 'character
		:adjustable t
		:fill-pointer t)))
	(format 
		s 
		"\{~{~{\"~S\":\"~S\",~}~}\}" 
		(values l)
	)
	s)
)

(defun format-wconstit (p)
	(format-key-pair (list (list `id (wconstit-id p))
		(list `class (wconstit-class p))
		(list `word (wconstit-word p))
		(list `syncat (wconstit-syncat p))
		(list `start (wconstit-start p))
		(list `end (wconstit-end p))))
)

(defun lookup-from-skeleton-map (q)
(let 
  ((res (sstring q))) 
  (progn  
    (skdbg 'lookup-from-skeleton-map "lookup: ~A gives ~A" res (assoc res *var-type-map*))
    (if 
      (assoc res *var-type-map*) 
      (values (cdr (assoc res *var-type-map*)) t)
      (values 0 nil)
  ))) 
)

(defun get-simple-type (c)
  (let ((type (if (consp c) (cadr c) c)))
    (if (symbolp type)
	type
	'bad-type)))

(defun compute-skeleton (constit start end)
  (when (member (constit-cat constit) '(w::s w::vp w::np w::adjp w::advbl)) ; if constit-cat is one of these
      (let* ((lf (get-fvalue (constit-feats constit) 'w::lf)) ; get feature value of w::lf from  constit-feats
	     (complete-class (get-value lf 'w::class)) ; get the complete-class name
	     (class (if (consp complete-class) (second complete-class) complete-class)) ; if class is consd, take the second item
	     ;(xxx (if (member class '(ont::and ont::sequence)) ; if class is one of these things
	     ;	      (skdbg 'compute-skeleton "FOUND SEQUENCE from constit ~S" constit)))
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

		(let ((skel (build-semantic-skeleton var complete-class (if found-bound-role roles) (constit-cat constit) start end))) 
		  	;; only pass in roles if there is at least one bound value 
			(if (third skel) 
			  (progn
			  	(skdbg '(build-semantic-skeleton 1) "adjusting ~A by ~A" (first skel) (second skel))
		  	  	(setf *var-type-map* (remove-if #'(lambda (f) (equal (first f) var)) *var-type-map*))
		  	  	(push (append (list (sstring var)) skel) *var-type-map*)
			  )
			)
			skel
			)
	     ))))

;(defun wsd-check (root roles)
;  (let* ((reply (send-and-wait `(REQUEST :content (WSD-CHECK :root ,root :roles ,roles))))
;	 (score (car (or (find-arg reply :score) (find-arg-in-act reply :score)))))
;		   (skdbg 'wsd-check "~%Recieved a score of ~S" score)
;		   (convert-to-adjustment-factor score)
;))

(defun test-element (e1 e2)
  ;; TODO: how do you match ont name
  (skdbg 'test-element "testing...")
  ;; Need to check to see if a word isn't getting a wsd entry
  ;; if no entry is given, it should be removed from consideration
  (skdbg '(test-element 1) "comparing ~A - ~A" e1 e2)
  (let (
	(tps (equalp (second e1) (second e2))) 
    	(spns (span-overlap (car (last e1)) (car (last e2)))) 
    	(word (equalp (first e1) (first e2)))
    )
    (if (and tps word spns) (skdbg '(test-element 1) "comparing ~A - ~A : ~A" e1 e2 (list tps word spns)))
    (and tps spns word)
    )
)

(defun check-wsd-map (cls lex span &optional (against *semantic-skeleton-map*))
  ;; Checks to see if the (cls lex span) triple matches something we've already seen.
  (skdbg '(check-wsd-map 1) "looking for: ~S ~S ~S" cls lex span)
  (skdbg '(check-wsd-map 1) "in: ~S" against)
  (if against 
    (or 
       (test-element (list (symbol-name cls) (symbol-name lex) span) (car against)) 
       (check-wsd-map cls lex span (cdr against)))
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
  (skdbg '(build-semantic-skeleton 1) "all-roles: ~A" roles)
  (let* (
	  (word (if (consp class) (caddr class) '())) ;; get word and class (syncat provided)
	  (rolescores (map 'list #'(lambda (res) (first res)) (remove-if-not 'cadr (mapcar 'count-wsd-checks roles))))
    	  (cls (if (consp class) (cadr class) class))
	) 
    	(skdbg '(build-semantic-skeleton 1) "Rolescores: ~S" rolescores)
	 (if (and 
	      (member word (map 'list #'second *semantic-skeleton-map*) :test #'string=) 
	      (check-wsd-map cls word (list start end))
	)
	  (list (list cls word) (score-wsd (map 'list #'second rolescores) 1))
	  (list (list cls word) (score-wsd (map 'list #'second rolescores) 0))
	)
))

(defun count-wsd-checks (id) 
  (skdbg 'count-wsd-checks "~A" id)
  (multiple-value-bind  
    (score exist) 
    (lookup-from-skeleton-map id) 
    (progn 
      (skdbg 'count-wsd-checks "(~A, ~A)" score exist)
      (list score exist)
)))

(defun score-wsd (r score)
  (let ((s (+ score ( apply #'+ r)))
	 (size (+ 1 (length r))) 
	 )
    (skdbg 'score-wsd "score is ~A" r)
    (/ s size)
    )
  )


(defun convert-to-adjustment-factor (adj)
   (if (numberp adj) (max (min adj *max-semantic-skeleton-factor*) *min-semantic-skeleton-factor*)
       1))
