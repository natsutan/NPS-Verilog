(add-load-path "../../../common/" :relative)

(require "npsv")


;;; main
;;; --------------------------------------------------------------------------------
;;; main
;;; --------------------------------------------------------------------------------
(define (main args)
  (when (not (= (lentgh args 3)))
    (usage-exit (car args))))











