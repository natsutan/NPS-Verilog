(use srfi-19)
(use srfi-13)
(use file.util)
(add-load-path "module/" :relative)

(define-module npsv
  (extend
   npsv-core
   npsv-file
   npsv-verilog
   npsv-misc
   npsv-inmem
   npsv-outmem
   npsv-rom
   npsv-const
   npsv-portconv
   npsv-dataflow
   npsv-build-top
   ))

(require "npsv-core")
(require "npsv-file")
(require "npsv-verilog")
(require "npsv-misc")
(require "npsv-inmem")
(require "npsv-outmem")
(require "npsv-rom")
(require "npsv-const")
(require "npsv-portconv")
(require "npsv-dataflow")
(require "npsv-build-top")
          
(provide "npsv")
