;;;;
;;;; W::BREAD
;;;;

(define-words :pos W::n
 :words (
  (W::BREAD
  (senses
	   ((LF-PARENT ONT::BREAD)
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(w::bread 
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::coat-food)
   (example "bread the chicken before you fry it")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   )
  )
 )
))

