;;;;
;;;; w::scratch
;;;;

(define-words :pos W::n
 :words (
  (w::scratch
  (senses((LF-PARENT ONT::wound)
	    (TEMPL count-pred-TEMPL)
	    )
	   )
)
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::scratch
   (SENSES
    ;;;; swier -- scratch that.
    ((LF-PARENT ONT::CANCEL)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

