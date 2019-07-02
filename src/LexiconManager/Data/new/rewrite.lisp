;;;;
;;;; W::rewrite
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(W::rewrite
 (wordfeats (W::morph (:forms (-vb) :past W::rewrote :pastpart W::rewritten :ing W::rewriting :nom w::rewrite)))
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ONT::revise)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    ((meta-data :origin lam :entry-date 20050707 :change-date nil :comments missing-as-frame)
     (LF-PARENT ONT::revise)
     (SEM (F::Aspect F::bounded))
     (TEMPL AGENT-AFFECTED-RESULT-XP-NP-TEMPL (xp (% w::PP (w::ptype w::as)))) 
     )
    )
   )
))

