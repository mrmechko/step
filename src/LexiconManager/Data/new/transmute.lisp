;;;;
;;;; W::transmute
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::transmute
   (SENSES
    ((meta-data :origin "verbnet-2.0-corrected" :entry-date 20060315 :change-date nil :comments nil :vn ("turn-26.6.1") :wn ("transmute%2:30:00" "transmute%2:30:01" "transmute%2:30:02"))
     (LF-PARENT ONT::adjust)
     (TEMPL AGENT-AFFECTED-RESULT-XP-PP-INTO-OPTIONAL-TEMPL (xp (% w::pp (w::ptype w::into)))) ; like translate but pp is into not to
     )
    )
   )
))

