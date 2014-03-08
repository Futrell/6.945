;;; Load up extended Scheme interpreter
;;;

(load "utils" user-initial-environment)
(load "ghelper" user-initial-environment)
(load "syntax" user-initial-environment)
(load "rtdata" user-initial-environment)


(define generic-evaluation-environment
  (extend-top-level-environment user-initial-environment))

(load "interp" generic-evaluation-environment)
(load "repl" generic-evaluation-environment)

(ge generic-evaluation-environment)
