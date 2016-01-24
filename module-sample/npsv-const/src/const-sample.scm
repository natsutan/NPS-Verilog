(use srfi-1)
(add-load-path "../../../src/" :relative)
(use npsv)

(define *instance* '())

;;; --------------------------------------------------------------------------------
;;; main
;;; --------------------------------------------------------------------------------
(define (main args)
  (when (not (= (length args) 2))
    (usage-exit (car args)))

  (set! *instance* (make-const-from-file (second args)))
  (print-setting *instance*)
  (make-verilog-file *instance*)
  (make-template *instance*)
  (make-initialize-file *instance*)
  (make-verilog-testbench-file *instance*)
  0)

