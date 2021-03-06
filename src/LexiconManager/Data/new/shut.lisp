;;;;
;;;; W::shut
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::shut (W::off))
   (wordfeats (W::morph (:forms (-vb) :past W::shut)))
   (SENSES
    ((LF-PARENT ONT::turn-off)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "shut off the power")
     )
    )
   )
))

