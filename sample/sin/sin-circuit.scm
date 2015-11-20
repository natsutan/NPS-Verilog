(use slib)
(require 'trace)

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
;(print (npsv-get-top))
;(print (npsv-get-modules))


(make-all-rtl "./output/rtl")

;(make-template (npsv-get-top))
;(make-top-testbench)
(make-dataflow)




