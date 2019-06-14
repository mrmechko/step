;;;;
;;;; W::LOCATE
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::LOCATE
   (SENSES
    ;;;;
    ;;;; disprefer this sense (only affects passive form)
    ((LF-PARENT ONT::find)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (PREFERENCE 0.96)
     )
    )
   )
))

