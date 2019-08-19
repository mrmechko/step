(in-package "PARSER")

(defvar +dbg-general+ (dbg::sk-tag-list 'default 'misc))
(defvar +dbg-wsd+ (dbg::sk-tag-list 'wsd-lookup 'wsd-check 'spans))
(defvar +dbg-cache+ (dbg::sk-tag-list 'set-map 'clear-map 'semantic-skeleton-cache))
(defvar +dbg-skeleton-score+ (dbg::sk-tag-list 'build-skeletons))
(defvar +dbg-misc+ (dbg::sk-tag-list 'misc))

(defvar +all-rik+ (dbg::sk-tag-list
		    +dbg-general+
		    +dbg-misc+
		    +dbg-cache+ 
		    ;+dbg-wsd+ 
		    ;+dbg-skeleton-score+ 
		    ))

(defvar *skel-dbg* (dbg::make-sk-debugger +all-rik+))
(format t "~A" *skel-dbg*)

(defun skdbg (tags msg &rest args) (apply #'dbg::sk-debug `(,*skel-dbg* ,tags ,msg ,@args)))


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
    (skdbg 'clear-map "clearing skeleton map")
    (setf *semantic-skeleton-map* nil)
    (setf *semantic-skeleton-map-filled* nil)
)

(defun set-skeleton-map (content) 
	(skdbg 'set-map "setting skeleton map to ~S" content)
	(setf *semantic-skeleton-map* content)
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
      (let ((score (cadr skel)))
	(if (not (numberp score)) (setq score 1))
	(skdbg +dbg-skeleton-score+ "adjusting PROB by ~S" score)
	(* prob score))))

(defun point-in-span (p s2)
	(and (>= p (first s2)) (<= p (second s2)))
)

(defun span-overlap (s1 s2)
	(progn
		(skdbg 'spans "span check: ~S overlaps ~S" s1 s2)
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

(defun lookup-from-skeleton-map (id)
	(let ((q (if (listp id) (car id) id)))
		(cond 
			((eq q '<var>) 
				q)
		((symbolp q)
			(cadr (assoc q *var-type-map*))
			;(let ((entry (assoc id *var-type-map*)))
			;    (values (cadr entry) (caddr entry)))
		)
		((constit-p q)  ;; this is an IMPRO, find type directly - the item passed in was a constit
			(get-simple-type (get-value q 'w::class))))) ; how often do we get things without spans?
)

(defun lookup-ont-type-from-skeleton-map (id)
	(let ((entry (lookup-from-skeleton-map id))
		(if (listp entry)
			(values (cadr entry) (caddr entry))
			entry
		)
	))
)

(defun get-simple-type (x)
	(skdbg 'build-skeletons "~%Getting simple type from: ~S" x)
  (let ((type (if (consp x) (cadr x) x)))
    (if (symbolp type)
	type
	'bad-type)))

(defun compute-skeleton (constit start end)
  (when (member (constit-cat constit) '(w::s w::vp w::np w::adjp w::advbl)) ; if constit-cat is one of these
      (let* ((lf (get-fvalue (constit-feats constit) 'w::lf)) ; get feature value of w::lf from  constit-feats
	     (complete-class (get-value lf 'w::class)) ; get the complete-class name
	     (class (if (consp complete-class) (second complete-class) complete-class)) ; if class is consd, take the second item
	     ;(xxx (if (member class '(ont::and ont::sequence)) ; if class is one of these things
	     ;	      (skdbg 'build-skeletons "FOUND SEQUENCE from constit ~S" constit)))
	     (var (get-value lf 'w::var))
	     (constraint (if lf (get-value lf 'w::constraint)))
	     (role-pairs (if (and constraint (not (eq constraint '-))
				  (not (var-p constraint)))
			     (constit-feats constraint)))
	     ;; find all essential roles, convert unbound values to constant <VAR>
	     (found-bound-role nil)
	     (roles (mapcar #'(lambda (x)
				   (if (var-p (cadr x)) 
				       (list (car x) '<var>)
				       (progn (setq found-bound-role t)
					      x)))
			       (remove-if-not #'(lambda (x) (member (car x) *essential-roles*))
				   role-pairs)))
	     (skel (build-semantic-skeleton var complete-class (if found-bound-role roles) ;; only pass in roles if there is at least one bound value
					    (constit-cat constit) start end)))
	     
	     (skdbg 'semantic-skeleton-cache-verbose "skeleton map is ~S" *semantic-skeleton-map*)
	     skel)))

(defun wsd-check (root roles)
  (let* ((reply (send-and-wait `(REQUEST :content (WSD-CHECK :root ,root :roles ,roles))))
	 (score (car (or (find-arg reply :score) (find-arg-in-act reply :score)))))
		   (skdbg 'wsd-check "~%Recieved a score of ~S" score)
		   (convert-to-adjustment-factor score)
))

(defun test-element (e1 e2)
  ;; TODO: how do you match ont name
  (skdbg 'build-skeletons "testing...")
  (skdbg 'build-skeletons "comparing ~S against ~S" e1 e2)
  (and (span-overlap (car (last e1)) (car (last e2))) (equalp (string (first e1)) (string (first e2))))
)

(defun check-wsd-map (cls lex span &optional (against *semantic-skeleton-map*))
  ;; Checks to see if the (cls lex span) triple matches something we've already seen.
  (skdbg 'build-skeletons "looking for: ~S ~S ~S" cls lex span)
  (skdbg 'build-skeletons "in: ~S" against)
  (skdbg 'build-skeletons "in: ~S" against)
  (if against 
    (or 
       (test-element (list cls lex span) (car against)) 
       (check-wsd-map cls lex span (cdr against)))
    nil
    )
)


(defun get-wsd-data ()
  ;; Gets data from SkeletonScore python module if the list is empty
  ;; Should add a guard that gets replaced at reset
  (skdbg 'semantic-skelton-cache "semantic-skeleton-map filled: ~S" *semantic-skeleton-map-filled*)
  (if (not *semantic-skeleton-map-filled*) 
    (let* (
	   (reply (send-and-wait `(REQUEST :content (get-wsd-data)))))
      (skdbg 'semantic-skeleton-cache "recieved reply ~S" reply)
      (set-skeleton-map (cdr reply))
)))


(defun build-semantic-skeleton (id class roles syncat start end)
  ;; this builds the skeleton and adds to skeleton-map and the var-map if necessary"
  (get-wsd-data)
  ;; This should probably all get replaced?
  (let* (
	  (word (if (consp class) (caddr class) '())) ;; get word and class (syncat provided)
	  (rolemap (mapcar 'unpack-role roles))
    	  (cls (if (consp class) (cadr class) class))
	) 
	(skdbg 'build-skeletons "Roles: ~S" rolemap)
	(skdbg 'build-skeletons "class ~S" class)
	(skdbg 'build-skeletons "word ~S" word)
	(skdbg 'build-skeletons "cls ~S" cls)
	(skdbg 'build-skeletons "start: ~S, end ~S. span ~S" start end (list start end))
	(if (check-wsd-map cls word (list start end))
	  (list (list cls word) 1)
	  nil
	  )
))

(defvar *max-semantic-skeleton-factor* 1.05)
(defvar *min-semantic-skeleton-factor* .95)

(defun convert-to-adjustment-factor (log)
   (if (numberp log) (max (min log *max-semantic-skeleton-factor*) *min-semantic-skeleton-factor*)
       1))

(defun unpack-role (role)
	(let ((result (lookup-from-skeleton-map (cdr role))))
		(list 
			(keywordify (car role))
			(format-wconstit (if (wconstit-p result) result (make-wconstit :id nil :class result)))
		)
	)
)
