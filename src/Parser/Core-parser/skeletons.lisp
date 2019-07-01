(in-package "PARSER")

(defvar *semantic-skeleton-scoring-enabled* nil)
(defvar *semantic-skeleton-map* nil)
(defvar *var-type-map* nil) ; cache
(defvar *semantic-skeleton-score-factor* .1)
(defvar *essential-roles* '(ont::agent ont::agent1 ont::affected ont::affected1 ont::neutral ont::neutral1 ont::formal ont::result ont::affected-result ont::of ont::val ont::figure ont::ground ont::experiencer ont::source ont::transient-result))

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
	(trace-msg 3 "~%adjusting PROB by ~S" score)
	(* prob score))))

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
	(format t "~%Getting simple type from: ~S" x)
  (let ((type (if (consp x) (cadr x) x)))
    (if (symbolp type)
	type
	'bad-type)))

(defun compute-skeleton (constit start end)
  (when (member (constit-cat constit) '(w::s w::vp w::np w::adjp w::advbl)) ; if constit-cat is one of these
      (let* ((lf (get-fvalue (constit-feats constit) 'w::lf)) ; get feature value of w::lf from  constit-feats
	     (complete-class (get-value lf 'w::class)) ; get the complete-class name
	     (class (if (consp complete-class) (second complete-class) complete-class)) ; if class is consd, take the second item
	     (xxx (if (member class '(ont::and ont::sequence)) ; if class is one of these things
		      (trace-msg 2 "~%~% FOUND SEQUENCE from constit ~S" constit)))
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
	      ;;(xxy (format t "~% cleaned up pairs are ~S" roles))
	     (skel (build-semantic-skeleton var complete-class (if found-bound-role roles) ;; only pass in roles if there is at least one bound value
					    (constit-cat constit) start end)))
	     
	     ;;(if roles (format t "~% sleeton map is ~S" *semantic-skeleton-map*))
	     skel)))

(defun wsd-check (root roles)
  (let* 
    ((reply (send-and-wait `(REQUEST :content (WSD-CHECK :root ,root :roles ,roles))))
	 (score (car (or (find-arg reply :score) (find-arg-in-act reply :score)))))
		   (format t "~%Recieved a score of ~S" score)
))

(defun build-semantic-skeleton (id class roles syncat start end)
  ;; this builds the skeleton and adds to skeleton-map and the var-map if necessary"
  (let (
	  (word (if (consp class) (caddr class))) ;; get word and class (syncat provided)
	  (rolemap (mapcar 'unpack-role roles))
	) 
	;(format t "~%Roles: ~S" rolemap)
    (setq class (if (consp class) (cadr class) class))
	(setq thisskel (make-wconstit :id id :class class :word word :syncat syncat :start start :end end))
    (wsd-check (format-wconstit thisskel) (format-key-pair rolemap))

  (if (not (assoc id *var-type-map*)) ; if id is not already in the map, put it there
    	(push (list id thisskel) *var-type-map*))
	(list thisskel 1)))

  	;(let* (
	;	(skeleton (list* class (unpack-roles roles)))
	;	(cached-skeleton (assoc skeleton *semantic-skeleton-map* :test #'equalp)) ; changed to equalp for when I uncomment this
	;)
    ;; the following is for debugging - it provides a quick way to generate a chache from sentences
    ;(if (and *generate-skeletons* (not cached-skeleton) (> (list-length skeleton) 1))
	;	(push (list skeleton 1) *semantic-skeleton-map*))
	;;(if cached-skeleton (format t "~%found cached info: ~S:" cached-skeleton))
	;; if no cache, get the new value and record it in the cache
    ;(or cached-skeleton
	;; a single atom with no roles gets a score of 1
	;	(if (and (consp skeleton) (eq (list-length skeleton) 1))
	;    	(list skeleton 1))
	;	(let ((res (evaluate-skeleton skeleton syncat)))
	;		(push res *semantic-skeleton-map*)
	;	res)))))
    

;; (defun evaluate-skeleton (skel syncat)
;;   (let* ((reply (send-and-wait `(REQUEST :content (EVALUATE-SKELETON ,skel :syncat ,syncat))))
;; 	 (score (car (or (find-arg reply :score) (find-arg-in-act reply :score)))))
;;     (list skel (if (numberp score)
;; 		   (convert-to-adjustment-factor score)
;; 		   1))))

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
;; (defun unpack-roles (roles)
;;   (when roles
;;     (let 
;; 		((rolename (keywordify (caar roles))))
;;       	(multiple-value-bind
;; 			(type word)
;; 			(lookup-ont-type-from-skeleton-map (cadar roles)) ; assumes that anything here has been seen already...
;; 			(multiple-value-bind 
;; 	    		(rest-types rest-words)
;; 	    		(unpack-roles (cdr roles))
;; 	    		(values (list* rolename
;; 			 				type
;; 			 				rest-types)
;; 		    			(list* rolename
;; 			 				word
;; 			 				rest-words))))))
;)
