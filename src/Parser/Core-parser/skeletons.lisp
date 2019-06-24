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

(defun lookup-ont-type-from-skeleton-map (id)
  (cond 
    ((eq id '<var>) 
        id)
	((symbolp id)
        (let ((entry (assoc id *var-type-map*)))
            (values (cadr entry) (caddr entry))))
	((constit-p id)  ;; this is an IMPRO, find type directly
	 (get-simple-type (get-value id 'w::class)))))

(defun get-simple-type (x)
  (let ((type (if (consp x) (cadr x) x)))
    (if (symbolp type)
	type 
	'bad-type)))

(defun compute-skeleton (constit)
  (format t "~%it works.")
  (when (member (constit-cat constit) '(w::s w::vp w::np w::adjp w::advbl))
      (let* ((lf (get-fvalue (constit-feats constit) 'w::lf))
	     (complete-class (get-value lf 'w::class))
	     (class (if (consp complete-class) (second complete-class) complete-class))
	     (xxx (if (member class '(ont::and ont::sequence))
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
					    (constit-cat constit))))
	     
	     ;;(if roles (format t "~% sleeton map is ~S" *semantic-skeleton-map*))
	     skel)))

(defun get-wsd-index (id syncat)
    (if (assoc id *var-type-map*) ; if id is not already in the map, put it there
        (let ((entry (assoc id *var-type-map*)))
            (format t "~%this is the info: ~S" entry)
        )
    )
)

(defun check-wsd (cls word syncat)
  (let* 
    ((reply (send-and-wait `(REQUEST :content (CHECK-WSD ,cls :word ,word :syncat ,syncat))))
	 (score (car (or (find-arg reply :score) (find-arg-in-act reply :score)))))
		   (format t "~%Recieved a score of ~S" score)
	))

(defun build-semantic-skeleton (id class roles syncat)
  ;; this builds the skeleton and adds to skeleton-map and the var-map if necessary"
  ;;(format t "~% Building skeleton for ~S constituent ~S with roles ~S " syncat id roles)
  ;; (format t "~%build semantic skeleton")
  ;(format t "~%id: ~S" id)
  ;(format t "~%class: ~S" class)
  ;(format t "~%roles: ~S" roles)
  (format t "~%syncat: ~S" syncat)
  (let ((word (if (consp class) (caddr class))))
    (setq class (if (consp class) (cadr class) class))
    (check-wsd class word syncat)
  (if (not (assoc id *var-type-map*)) ; if id is not already in the map, put it there
    (progn (push (list id class word) *var-type-map*)
	    (trace-msg 3 "~%pushing ~S onto var-type-match for ~S" class id)
    ))
  ;(get-wsd-index id)
  (let* ((skeleton (list* class (unpack-roles roles)))
	 (cached-skeleton (assoc skeleton *semantic-skeleton-map* :test #'equal)))
    ;; the following is for debugging - it provides a quick way to generate a chache from sentences
    (if (and *generate-skeletons* (not cached-skeleton) (> (list-length skeleton) 1))
	(push (list skeleton 1) *semantic-skeleton-map*))
    ;;(if cached-skeleton (format t "~%found cached info: ~S:" cached-skeleton))
    ;; if no cache, get the new value and record it in the cache
    (or cached-skeleton
	;; a single atom with no roles gets a score of 1
	(if (and (consp skeleton) (eq (list-length skeleton) 1))
	    (list skeleton 1))
	(let ((res (evaluate-skeleton skeleton syncat)))
	(push res *semantic-skeleton-map*)
	res)
	))))
    

(defun evaluate-skeleton (skel syncat)
  (let* ((reply (send-and-wait `(REQUEST :content (EVALUATE-SKELETON ,skel :syncat ,syncat))))
	 (score (car (or (find-arg reply :score) (find-arg-in-act reply :score)))))
    (list skel (if (numberp score)
		   (convert-to-adjustment-factor score)
		   1))))

(defvar *max-semantic-skeleton-factor* 1.05)
(defvar *min-semantic-skeleton-factor* .95)

(defun convert-to-adjustment-factor (log)
  (if (numberp log) (max (min log *max-semantic-skeleton-factor*) *min-semantic-skeleton-factor*)
      1))


(defun unpack-roles (roles)
  (when roles
    (let ((rolename (keywordify (caar roles))))
      (multiple-value-bind
	(type word)
	(lookup-ont-type-from-skeleton-map (cadar roles))
	(multiple-value-bind 
	    (rest-types rest-words)
	    (unpack-roles (cdr roles))
	    (values (list* rolename
			 type
			 rest-types)
		    (list* rolename
			 word
			 rest-words)))))))