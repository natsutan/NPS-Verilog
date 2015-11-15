(use srfi-1)
(add-load-path "../../../common/" :relative)
(require "npsv")

(define *instance* '())
(define *data* '())

(define print-setting
  (lambda ()
    (format #t "function ~A~%" sin)
    (format #t "range ~A - ~A~%" *npsv-min* *npsv-max*)
    (format #t "adr width ~A~%"  *npsv-adr-width*)
    (format #t "data width ~A (integer ~A)~%" *npsv-W* *npsv-I*)
    (format #t "module name ~A~%" *npsv-module-name*)
    (format #t "output dir ~A~%" *npsv-rtl-output-dir*)
    (format #t "testbench dir ~A~%" *npsv-testbench-output-dir*)
    (format #t "template dir ~A~%" *npsv-template-output-dir*)))


;;; --------------------------------------------------------------------------------
;;; make rom data
;;; --------------------------------------------------------------------------------


(define make-data
  (lambda (func min max adrw W I)
    (let* ((num (power 2 adrw))
	   (data (make-vector num))
	   (unit (/ (- max min) num)))
      (dotimes(x num)
       (vector-set! data x (toFix (func (+ (* x unit) min)) W I)))
;      (print data)
      data)))


;;; --------------------------------------------------------------------------------
;;; verilog rtl
;;; --------------------------------------------------------------------------------

(define romdata->string
  (lambda (data dataw)
    (let ((format (- (power 2 dataw) 1)))
      (number->string (logand format data) 16))))

(define make-rtl
  (lambda (dir name data adrw dataw)
    (let ((fp (open-verilog-file dir name)))
      (make-header fp name)
      (format fp "module ~A~%" name)
      (format fp "\t(~%")
      (format fp "\tinput CLK,~%")
      (format fp "\tinput RESET_X,~%")
      (format fp "\tinput \[~A:0\] ADR,~%" (- adrw 1))
      (format fp "\toutput reg \[~A:0\] DATA~%" (- dataw 1))
      (format fp "\t);~%")
      (format fp "\talways @(posedge CLK or negedge RESET_X)begin~%")
      (format fp "\t\tif(RESET_X == 1'b0)begin~%")
      (format fp "\t\t\tDATA <= ~A'd0;~%" dataw)
      (format fp "\t\tend else begin~%")
      (format fp "\t\t\tcase(ADR)~%")
      (dotimes (x (power 2 adrw))
               (format fp "\t\t\t~A:DATA <= ~A'h~A;~%"
                       x dataw (romdata->string (vector-ref data x) dataw)))
      (format fp "\t\t\tendcase~%")
      (format fp "\t\tend~%")
      (format fp "\tend~%")
      (format fp "endmodule~%")
      (close-verilog-file fp))))


;;; --------------------------------------------------------------------------------
;;; verilog template
;;; --------------------------------------------------------------------------------


(define make-instance
  (lambda (name min max adr-w W I)
    (let ([inst (make <npsv-module> :name name :type 'NPS-rom :comment "rom module")]
          [fixed (make <npsv-fixed> :W W :I I)])
      (add-port inst (make <npsv-port> :name "start" :dir 'input :comment "no use"))
      (add-port inst (make <npsv-port> :name "set" :dir 'input :comment "no use"))
      (add-port inst (make <npsv-port> :name "vi" :dir 'input))
      (add-port inst (make <npsv-port> :name "fi" :dir 'input))
      (add-port inst (make <npsv-port> :name "vo" :dir 'output :type 'reg))
      (add-port inst (make <npsv-port> :name "fo" :dir 'output :type 'reg))
      (add-port inst (make <npsv-fixed-port> :name "datao" :dir 'output :msb (- W 1) :lsb 0 :fixed-info fixed :type 'reg))
      (add-port inst (make <npsv-port> :name "datai" :dir 'input :msb (- adr-w 1) :lsb 0 ))
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
                 *npsv-min*
                 *npsv-max*
                 *npsv-adr-width* 
                 *npsv-W*
                 *npsv-I*))
  (print-instance *instance*)
  (set! *data* (make-data *npsv-func*
                 *npsv-min*
                 *npsv-max*
                 *npsv-adr-width* 
                 *npsv-W*
                 *npsv-I*))
  (make-verilog-file *instance* *npsv-rtl-output-dir*
                     (eval rtl-template (interaction-environment)))


    0)


