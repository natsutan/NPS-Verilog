(use srfi-1)
(add-load-path "../../../src/" :relative)
(require "npsv")

(define *instance* '())

;;; --------------------------------------------------------------------------------
;;; main
;;; --------------------------------------------------------------------------------
(define (main args)
  (when (not (= (length args) 2))
    (usage-exit (car args)))
  
  (set! *instance* (make-to-mtx-from-file (second args)))
  (print-setting *instance*)
  (make-template *instance*)
  0)




