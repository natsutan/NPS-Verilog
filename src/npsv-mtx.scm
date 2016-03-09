;;; --------------------------------------------------------------------------------
;;; Nagato Pipeline System Matrix module
;;; --------------------------------------------------------------------------------
(define-module npsv-mtx
  (export-all))


;;; --------------------------------------------------------------------------------
;;; String conversion
;;; --------------------------------------------------------------------------------

(define mtx-dim-string
  (lambda (r c)
    (format #f "~A_~A" r c)))

(define mtx-portname
  (lambda (prefix r c)
    (string-append prefix (mtx-dim-string r c))))

