(define-module npsv-build-top
  (export make-top-rtl npsv-get-top))

(define *top-inst* '())

(define-class <npsv-top> (<npsv-module>)
  ((module :init-keyword :module)
   (ch :init-keyword :ch)
   (wires :init-value '())
   ))

(define-class <npsv-wire> ()
  ((name :init-keyword :name)
   (lsb :init-keyword :lsb :init-value 0)
   (msb :init-keyword :msb :init-value 0)))


(define make-wires-from-ch
  (lambda (ch)
    (let ([src (ref ch 'src)]
          [dst (ref ch 'dst)]
          [prefix (string-append (ref ch 'name) "_")]
          [wires '()])
      (set! wires (cons (make <npsv-wire> :name (string-append prefix "vo")) wires))
      (set! wires (cons (make <npsv-wire> :name (string-append prefix "fo")) wires))
  
      
      wires)))

(define make-top-wires
  (lambda (top)
    (dolist (ch (ref top 'ch))
            (add-top-wires top (make-wires-from-ch ch)))))

    
(define add-top-wires
  (lambda (inst wires)
    (set! (ref inst 'wires)
          (concatenate (list wires (ref inst 'wires))))))


(define make-top-module
  (lambda ()
    (set! *top-inst*
          (make <npsv-top> :name *top-name* :type 'npsv-top
                :module (npsv-get-modules) :ch (npsv-get-ch)))))

(define make-top-ports 
  (lambda (top)
    (add-port top (make <npsv-port> :name "start" :dir 'input))
    (add-port top (make <npsv-port> :name "set" :dir 'input))
    (dolist (m (ref top 'module))
            (add-top-ports top m))))

(define-method add-top-ports (top (m <npsv-module>))
  #t)

(define npsv-get-top
  (lambda ()
    *top-inst*))

(define make-top-rtl
  (lambda (odir)
    (make-top-module)
    ;(set! (ref *top-inst* 'rtl-output-dir) odir)
    (set! (ref *top-inst* 'rtl-output-dir) ".")
    (set! (ref *top-inst* 'template-ouput-dir) ".")

    (make-top-ports *top-inst*)
    (make-top-wires *top-inst*)
    (write-top-verilog odir *top-inst*)
    
    
    (make-template *top-inst*)  
    (print *top-inst*)))

(define write-top-ports
  (lambda (fp ports)
    (if (= (length ports) 1)
        (format fp "\t~A~%" (make-port-string (car ports)))  ; no comma
        (begin
          (format fp "\t~A,~%" (make-port-string (car ports)))
          (write-top-ports fp (cdr ports))))))

(define write-top-wires
  (lambda (fp wires)
    (dolist (c wires)
            (format fp "\twire ~A;~%" (make-wire-string c)))))


(define write-top-verilog
  (lambda (dir inst)
    (let* ([name (ref inst 'name)]
           [fp (open-verilog-file dir name)])
      (write-header fp name)
      (format fp "module ~A\n" name)
      (format fp "(\n")
      (write-top-ports fp (ref inst 'ports))
      (format fp ");\n")
      (write-top-wires fp (ref inst 'wires))
      (format fp "endmodule\n")
      )))

    




(provide "npsv-build-top")





