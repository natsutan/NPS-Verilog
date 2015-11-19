(define-module npsv-build-top
  (export make-top-rtl npsv-get-top))

(define *top-inst* '())

(define-class <npsv-top> (<npsv-module>)
  ((module :init-keyword :module)
   (ch :init-keyword :ch)))

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
    (set! (ref *top-inst* 'rtl-output-dir) odir)
    (set! (ref *top-inst* 'template-ouput-dir) ".")

    (make-top-ports *top-inst*)
    (make-template *top-inst*)    
    (print *top-inst*)))






(provide "npsv-build-top")





