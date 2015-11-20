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


(define ch->wire-prefix
  (lambda (ch)
    (string-append (ref ch 'name) "_")))

(define write-outport-assign
  (lambda (fp name ch . last-opt)
    (let ([wire (string-append (ch->wire-prefix ch) name)]
          [last (get-optional last-opt #f)])
      (if last
          (format fp "\t\t.~A(~A)\n" name wire)
          (format fp "\t\t.~A(~A),\n" name wire)))))


(define make-wires-from-ch
  (lambda (ch)
    (let ([src (ref ch 'src)]
          [dst (ref ch 'dst)]
          [prefix (ch->wire-prefix ch)]
          [wires '()])
      (set! wires (cons (make <npsv-wire> :name (string-append prefix "vo")) wires))
      (set! wires (cons (make <npsv-wire> :name (string-append prefix "fo")) wires))
      (set! wires (cons
                   (make-wire-from-dataport (find-src-port src) (find-dst-port dst) (string-append prefix "datao"))
                   wires))
      wires)))

(define-method make-wire-from-dataport ((src-port <npsv-port>) (dst-port <npsv-port>) name)
  (make <npsv-wire> :name name :lsb 0 :msb (ref dst-port 'msb)))

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


(define write-top-instances
  (lambda (fp top)
    (dolist (m (ref top 'module))
            (write-module-instantiation fp m (ref top 'ch)))))

(define-method write-module-instantiation (fp (m <npsv-module>) channels)
  (format #t "Error:not implemented instantiaon ~A~%" (ref m 'name)))

(define write-common-connection
  (lambda (fp)
    (format fp "\t\t.clk(clk),\n")
    (format fp "\t\t.reset(reset),\n")
    (format fp "\t\t.set(set),\n")
    (format fp "\t\t.start(start),\n")))

(define write-port-assign
  (lambda (fp port sig . last-opt)
    (let ((last (get-optional last-opt #f)))
      (if last
          (format fp "\t\t.~A(~A)\n" port sig)
          (format fp "\t\t.~A(~A),\n" port sig)))))

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
      (write-cr fp)
      (write-top-instances fp inst)
      
      (format fp "endmodule\n")
      )))

    



(provide "npsv-build-top")





