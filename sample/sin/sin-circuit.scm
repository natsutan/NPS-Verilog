(add-load-path "../../src/" :relative)
(use npsv)

; initialize
(npsv-initialize! "npsv_top")

; instancation
(define inp (make-inmem-from-file "./setting/inmem.scm"))
(define sinrom (make-rom-from-file "./setting/sin.scm"))
(define outp (make-outmem-from-file  "./setting/outmem.scm"))

; wire connection
(connect inp sinrom)
(connect sinrom outp)

; output
(make-all-rtl "./output/rtl")
(make-initialize-file inp)
(make-top-testbench "./output/tb")
(make-dataflow)




