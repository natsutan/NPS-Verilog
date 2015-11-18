(add-load-path "../../src/" :relative)
(use npsv)

; initialize
(npsv-initialize! "npsv_top")
(print (npsv-get-topname))

; instancation
(define inp (make-inmem-from-file "./setting/inmem.scm"))
(define sinrom (make-rom-from-file "./setting/sin.scm"))
(define outp (make-outmem-from-file  "./setting/outmem.scm"))

; wire connection
(connect inp sinrom)
(connect sinrom outp)



(print (npsv-get-top))

;(make-top-rtl)
;(make-top-testbench)
(make-dataflow)




