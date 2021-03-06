;;;;
;;;; W::PROMOTE
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
   (W::PROMOTE
    (wordfeats (W::morph (:forms (-vb) :nom w::promotion :agentnom w::promoter)))
    (senses 
     ((LF-PARENT ont::encourage)
      (example "the flyers promote the idea")
      (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
      (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
      )
     ((LF-PARENT ont::cause-stimulate)
      (example "the chemical promotes the activation of enzymes")
      (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
      (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
      )  
   
   ))
))
