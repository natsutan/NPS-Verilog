(define-module npsv-portconv
  (export-all))


(define-class <npsv-portconv> (<npsv-module>)
  ((src :init-keyword :src)
   (dst :init-keyword :dst)))
        



  
(define-method make-portconv ((src <npsv-fixed-port>) (dst <npsv-adr-port>) name)
  (let ([inst (make <npsv-portconv> :type 'npsv-portconv :name name :src src :dst dst)]
        [new-src (copy-instance src)]
        [new-dst (copy-instance dst)]
        )
    (add-port inst (make <npsv-port> :name "vi" :dir 'input))
    (add-port inst (make <npsv-port> :name "fi" :dir 'input))
    (add-port inst (make <npsv-port> :name "vo" :dir 'output))
    (add-port inst (make <npsv-port> :name "fo" :dir 'output))

    (set! (ref new-src 'name) "datai")
    (set! (ref new-src 'dir) 'input)
    (set! (ref new-dst 'name) "datao")
    (set! (ref new-dst 'dir) 'output)
    
    (add-port inst new-src)
    (add-port inst new-dst)

    (flush-all-ports)
    inst))



(define make-pconv-name
  (lambda (src dst)
    (string-append "pconv_" (ref src 'name) "_to_" (ref dst 'name))))

    

(provide "portconv")


