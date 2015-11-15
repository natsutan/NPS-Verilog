;;; --------------------------------------------------------------------------------
;;; Nagato Pipeline System Core module
;;; --------------------------------------------------------------------------------

(define-module npsv-core
  (export-all))

(define-class <npsv-fixed> ()
  ((W :init-keyword :W) ; Whole bit width
   (I :init-keyword :I) ; Inter bit width
   (Q :init-keyword :Q :init-value 'SC_TRN)  ; quantize mode
   (O :init-keyword :O :init-value #f)  ; overflow mode, reserved
   (N :init-keyword :N :init-value #f)  ; reserved
   ))  

(define-class <npsv-port> ()
  ((name :init-keyword :name)
   (dir :init-keyword :dir)
   (lsb :init-keyword :lsb :init-value 0)
   (msb :init-keyword :msb :init-value 0)
   (special :init-keyword :special :init-value #f)
   (comment :init-keyword :comment :init-value "")
   (type :init-keyword :type :init-value 'signal)
   (ch :init-keyword :ch :init-value 0)
   ))

(define-class <npsv-fixed-port> (<npsv-port>)
  ((fixed-info :init-keyword :fixed-info)))

(define-class <npsv-module> ()
  ((name :init-keyword :name)
   (type :init-keyword :type)
   (ports :init-value '())
   (comment :init-keyword :comment :init-value "")
   (rtl-output-dir :init-keyword :rtl-output-dir)
   (testbench-output-dir :init-keyword :testbench-output-dir)
   (template-ouput-dir :init-keyword :template-ouput-dir)
   ))

(define-class <npsv-process> ()
  ((label :init-keyword :label :init-value "")
   (reset_process :init-keyword :reset)
   (process :init-keyword :process)))

(define-class <npsv-initial-process> (<npsv-process>)
  ())

(define-class <npsv-wire> ()
   ((name :init-keyword :name)
    (assign :init-keyword :assign)))

   
(define-method initialize ((self <npsv-module>) initargs)
  (next-method)
  (let ((clk (make <npsv-port> :name "clk" :dir 'input :spetial #t))
	(reset_x (make <npsv-port> :name "reset_x" :dir 'input :spetial #t)))
    (add-port self clk)
    (add-port self reset_x)))

(define-method add-port ((inst <npsv-module>) (port <npsv-port>))
  (set! (ref inst 'ports) (append (ref inst 'ports) (cons port '()))))
    
(define-method add-process ((inst <npsv-module>) (process <npsv-process>))
  (set! (ref inst 'processes) (cons process (ref inst 'processes))))
    
(define-method add-wire ((inst <npsv-module>) (process <npsv-wire>))
  (set! (ref inst 'wires) (cons processes (ref inst 'wires))))

(define-method print ((inst <npsv-module>))
  (print (string-concatenate  (list "<npsv-module> " (ref inst 'name) " (" (symbol->string (ref inst 'type)) ")" )))
  ;(next-method)
  )

(define-method print ((p <npsv-port>))
  (print (make-port-string p)))

(define-method print ((p <npsv-fixed-port>))
  (let ((fp (ref p 'fixed-info)))
    (let ((W (number->string (ref fp 'W)))
          (I (number->string (ref fp 'I))))
      (print (string-append
              (make-port-string p)
              "   (W:" W ",I:" I ")")))))

(define print-instance
  (lambda (inst)
    (print inst)
    (print (string-concatenate (list "\"" (ref inst 'comment) "\"")))
    (dolist (p (ref inst 'ports)) (print p))))

(define make-port-string
  (lambda (p)
    (let* ((dir (symbol->string (ref p 'dir)))
           (lsb (ref p 'lsb))
           (msb (ref p 'msb))
           (name (ref p 'name))
           (slise (if (and (zero? lsb) (zero? msb))
                      ""
                      (string-append "[" (number->string msb) ":" (number->string lsb) "]"))))
      (string-append dir " " slise " " name))))

(provide "npsv-core")
