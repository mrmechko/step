;;;;
;;;; W::FACE
;;;;

(define-words :pos W::n
 :tags (:base500)
 :words (
;; physical systems, digestive, reproductive,. ...
;; those are adjectives
;; external
  (W::FACE
  (senses((LF-PARENT ONT::external-body-part)
    (TEMPL body-part-reln-templ)
    )
   )
)
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
 (W::face
   (SENSES
    ((LF-PARENT ONT::pointing-to)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     (example "the triangle faces the square")
     (meta-data :origin fruitcarts :entry-date 20050401 :change-date nil :comments nil)
     )
    ((LF-PARENT ONT::pointing-to)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL (xp (% W::PP (W::ptype (? pt W::toward w::towards))))) 
     (example "the triangle faces towards the square")
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :comments Orient)
     )
    ((LF-PARENT ONT::orient)
     (SEM (F::Aspect F::dynamic) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-RESULT-TO-OBJCONTROL-TEMPL (xp (% W::PP (W::ptype (? pt W::toward w::towards)))))
     (example "face the triangle towards the square")
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :comments Orient)
     )
    )
   )
))

