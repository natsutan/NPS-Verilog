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
   ))

(require "npsv-core")
(require "npsv-file")
(require "npsv-verilog")
(require "npsv-misc")
(require "npsv-inmem")
(require "npsv-outmem")
(require "npsv-rom")

(provide "npsv")
