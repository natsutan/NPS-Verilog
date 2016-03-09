(use srfi-19)
(use srfi-13)
(use file.util)
(use util.combinations)
(add-load-path "module/" :relative)

(define-module npsv
  (extend
   npsv-core
   npsv-file
   npsv-verilog
   npsv-mtx
   npsv-misc
   npsv-inmem
   npsv-outmem
   npsv-rom
   npsv-const
   npsv-to-mtx
   npsv-portconv
   npsv-dataflow
   npsv-build-top
   ))

(require "npsv-core")
(require "npsv-file")
(require "npsv-verilog")
(require "npsv-misc")
(require "npsv-mtx")
(require "npsv-inmem")
(require "npsv-outmem")
(require "npsv-rom")
(require "npsv-to-mtx")
(require "npsv-const")
(require "npsv-portconv")
(require "npsv-dataflow")
(require "npsv-build-top")
          
(provide "npsv")
