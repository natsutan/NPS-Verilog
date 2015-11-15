(use srfi-1)
(add-load-path "../../../common/" :relative)
(require "npsv")

(define *instance* '())

(define print-setting
  (lambda ()
    (format #t "module name ~A~%" *npsv-module-name*)
    (format #t "data number ~A~%" *npsv-data-num*)
    (format #t "fixed number W ~A I ~A~%" *npsv-W* *npsv-I*)

    (format #t "output dir ~A~%" *npsv-rtl-output-dir*)
    (format #t "testbench dir ~A~%" *npsv-testbench-output-dir*)
    (format #t "template dir ~A~%" *npsv-template-output-dir*)))

(define make-instance
  (lambda (name data-num W I)
    (let ([inst (make <npsv-module> :name name :type 'NPS-outmem :comment "output memory module")]
          [adr_w (datanum->adr-w data-num)])
      (add-port inst (make <npsv-port> :name "start" :dir 'input))
      (add-port inst (make <npsv-port> :name "set" :dir 'input))
      (add-port inst (make <npsv-port> :name "vi" :dir 'input))
      (add-port inst (make <npsv-port> :name "fi" :dir 'input))
      (add-port inst (make <npsv-port> :name "vo" :dir 'output))
      (add-port inst (make <npsv-port> :name "fo" :dir 'output :type 'reg))
      (let ((fixed (make <npsv-fixed> :W W :I I)))
        (add-port inst
                  (make <npsv-fixed-port>
                    :name "datai"
                    :dir 'input
                    :msb (- W 1)
                    :lsb 0
                    :fixed-info fixed
                    )))
      (add-port inst (make <npsv-port> :name "cpu_adr" :dir 'input :lsb 0 :msb (- adr_w 1)))
      (add-port inst (make <npsv-port> :name "cpu_data" :dir 'output :lsb 0 :msb (- W 1)))
      (add-port inst (make <npsv-port> :name "cpu_rd" :dir 'input))
      inst)))

(define read-write-initialize-file
  (lambda (fpi fpo W I adr)
    (let ((buf (read-line fpi)))
      (when (not (eof-object? buf))
        (let ((n (string->number buf)))
          (when n              
            (let ((s (toFix n W I)))
              (format fpo "\tcpu_wr_task(~A,~A);  // ~A~%" adr s buf)
              (read-write-initialize-file fpi fpo W I (+ adr 1)))))))))

(define write-initialize-file-header
  (lambda (fp name W I )
    (write-header fp name)
    (format fp "// W = ~A, I = ~A~%" W I)))

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
                    *npsv-W* *npsv-I*
                    ))
  (print-instance *instance*)
  ;(make-verilog-file *instance* *npsv-rtl-output-dir*
  ;                    (eval rtl-template (interaction-environment)))
  ;(make-template *instance* *npsv-template-output-dir*)
  ;(make-initialize-file *instance* *npsv-init-file* *npsv-testbench-output-dir* *npsv-W* *npsv-I*)
  ;(make-verilog-testbench-file *instance* *npsv-testbench-output-dir*
  ;                              (eval testbench-template (interaction-environment))))
  )

;;; --------------------------------------------------------------------------------
;;; verilog source
;;; --------------------------------------------------------------------------------
(define rtl-template
'#"module ~*npsv-module-name* # (parameter DATA_WIDTH = ~*npsv-W* , DATA_NUM = ~*npsv-data-num*, DELTA_T = ~*npsv-delta-T* , ADR_WIDTH = ~(datanum->adr-w *npsv-data-num*), DELTA_WIDTH = ~(datanum->adr-w *npsv-delta-T*) )
"
  
  )

(define testbench-template
  '#"
module ~|*npsv-module-name*|_tb();
"
  )
