;;;;
;;;; W::got
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::got
   (wordfeats (W::morph (:forms NIL)) (W::vform W::pastpart))
   (SENSES
    ;;;; we've got three trucks
    ((LF-PARENT ONT::HAVE)
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     )

    
    ;;;; The necessitiy sense is only typical of the perfective
    ;;;; "He gets to go" means he's allowed -- how can we do this?
    ((LF-PARENT ONT::NECESSITY)
     (example "he's got to go")
     (SEM (F::Aspect F::Indiv-level) (f::Time-span f::extended)) ;; don't allow temporal mods on the higher verb (need)
     (TEMPL NEUTRAL-FORMAL-CP-SUBJCONTROL-TEMPL)
     )
    )
   )
))

