(define-module npsv-file
  (export-all))

;;;--------------------------------------------------------------------------------
;;; file
;;;--------------------------------------------------------------------------------
(define open-verilog-file
  (lambda (dir name)
    (let ((fname (string-append dir "/" name ".v")))                 
      (make-directory* dir)
      (format #t "open ~A~%" fname)
      (open-output-file fname)
      )))

(define close-verilog-file
  (lambda (fp)
    (close-output-port fp)))

(define load-setting-file
  (lambda (fname)
    (format #t "load ~A~%" fname)
    (load fname :environment (current-module))))

(provide "npsv-file")
