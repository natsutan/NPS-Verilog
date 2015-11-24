(define-module npsv-misc
  (export-all))

;;misc

(define (copy-instance obj)
  (rlet1 new (make (class-of obj))
    (dolist [slot (class-slots (class-of obj))]
      (set! (~ new (slot-definition-name slot))
            (~ obj (slot-definition-name slot))))))

(define quantize
  (lambda (x)
    (if (< x 0)
        (- x 1)
        x)))

(define toFix
  (lambda (x W I)
    (let ((val (* x (power 2 (- W I 1))))
          (max (- (power 2 (- W 1)) 1))
          (min (- (power 2 (- W 1)))))
      (let ((val_int (exact (values-ref (modf val) 1))))
        (clamp  (quantize val_int)  min  max)))))

    
(define clear-global-parameters!
  (lambda (params)
    (dolist (p params)
            (set! p '()))))

(define usage-exit
  (lambda (program-name)
    (format #t "Usage: gosh ~A setting-file~%" program-name)
    (exit 1)))

(define power
  (lambda (x n)
    (if (= n 1)
        x
        (* (power x (- n 1)) x))))

(define datanum->adr-w
  (lambda (datanum)
    (ceiling->exact (log datanum 2))))

(define last-elem?
  (lambda (l)
    (= (length ports) 1)))


(provide "npsv-misc")
