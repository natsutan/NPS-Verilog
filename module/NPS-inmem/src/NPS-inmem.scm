(use srfi-1)
(add-load-path "../../../common/" :relative)
(require "npsv")

(define *instance* '())

(define print-setting
  (lambda ()
    (format #t "module name ~A~%" *npsv-module-name*)
    (format #t "data number ~A~%" *npsv-data-num*)
    (format #t "initialize file ~A~%" *npsv-init-file*)
    (format #t "fixed number W ~A I ~A~%" *npsv-W* *npsv-I*)
    (format #t "delta T ~A ~%" *npsv-delta-T*)

    (format #t "output dir ~A~%" *npsv-rtl-output-dir*)
    (format #t "initialize file output dir ~A~%" *npsv-init-output-dir*)
    (format #t "testbench dir ~A~%" *npsv-testbench-output-dir*)
    (format #t "template dir ~A~%" *npsv-template-output-dir*)))

(define make-instance
  (lambda (name data-num init-file W I delta)
    (let ((inst (make <npsv-module> :name name :type 'NPS-inmem :comment "input memory module")))
      (add-port inst (make <npsv-port> :name "start" :dir 'input))
      (add-port inst (make <npsv-port> :name "set" :dir 'input))
      (add-port inst (make <npsv-port> :name "vo" :dir 'output :type 'reg))
      (add-port inst (make <npsv-port> :name "fo" :dir 'output :type 'reg))
      (let ((fixed (make <npsv-fixed> :W W :I I)))
        (add-port inst
                  (make <npsv-fixed-port>
                    :name "datao"
                    :dir 'output
                    :msb (- W 1)
                    :lsb 0
                    :fixed-info fixed
                    :type 'reg)))
      inst)))


;;; --------------------------------------------------------------------------------
;;; main
;;; --------------------------------------------------------------------------------
(define (main args)
  (when (not (= (length args) 2))
    (usage-exit (car args)))
  
  (load-setting-file (second args))
  (print-setting)
  (set! *instance* (make-instance
                    *npsv-module-name*
                    *npsv-data-num*
                    *npsv-init-file*
                    *npsv-W* *npsv-I*
                    *npsv-delta-T*))
  (print-instance *instance*)
  (write-verilog-file *instance* *npsv-rtl-output-dir*)
  
  )



