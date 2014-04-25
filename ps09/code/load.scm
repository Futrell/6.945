(set! (access user-initial-environment system-global-environment)
      (extend-top-level-environment system-global-environment))

(environment-define system-global-environment
		    'generic-evaluation-environment
		    (extend-top-level-environment user-initial-environment))

(load "utils" user-initial-environment)
(load "time-share" user-initial-environment)
(load "schedule" user-initial-environment)

(load "ghelper" user-initial-environment)
(load "syntax" user-initial-environment)
(load "rtdata" user-initial-environment)

(load "interp-actor" generic-evaluation-environment)
(load "repl" generic-evaluation-environment)

;; 

(ge generic-evaluation-environment)