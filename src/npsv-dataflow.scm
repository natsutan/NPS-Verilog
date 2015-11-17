(define-module npsv-dataflow
  (export-all))

(define *top* '())
(define *top-name* "")

(define-class <nspv-ch> ()
  ((src :init-keyword :src)
   (dst :init-keyword :dst)
   (ch  :init-keyword :ch :init-value 0)
   (name :init-keyword :name)))
  
(define npsv-initialize!
  (lambda (topname)
    (set! *top-name* topname)
    (set! *top* '())))

(define npsv-get-topname
  (lambda ()
    *top-name*))

(define npsv-get-top
  (lambda ()
    *top*))


(define connect
  (lambda (src dst)
    (let ((connection (make <nspv-ch> )))
      (set! *top* (cons connection *top*)))))



(provide "npsv-dataflow")
