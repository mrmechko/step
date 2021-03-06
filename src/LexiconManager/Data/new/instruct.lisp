;;;;
;;;; W::instruct
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::instruct
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("advise-37.9-1"))
     (LF-PARENT ONT::command)
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-OPTIONAL-TEMPL) ; like warn
     (PREFERENCE 0.96)
     )
    ((meta-data :origin task-learning :entry-date 20050825 :change-date nil :comments nil)
     (LF-PARENT ONT::command)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "I'll instruct him to do it")
     (TEMPL AGENT-AGENT1-FORMAL-OBJCONTROL-TEMPL) 
     )
    )
   )
))

