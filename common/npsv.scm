(use srfi-19)
(use srfi-13)
(use file.util)

(define-class <npsv-fixed> ()
  ((W :init-keyword :W) ; Whole bit width
   (I :init-keyword :I) ; Inter bit width
   (Q :init-keyword :Q :init-value 'SC_TRN)  ; quantize mode
   (O :init-keyword :O)  ; overflow mode, reserved
   (N :init-keyword :N)  ; reserved
   ))  


(define-class <npsv-port> ()
  ((name :init-keyword :name)
   (dir :init-keyword :dir)
   (lsb :init-keyword :lsb :init-value 0)
   (msb :init-keyword :msb :init-value 0)
   (special :init-keyword :special :init-value #f)
   (comment :init-keyword :comment :init-value "")
   (type :init-keyword :type :init-value 'signal)))

(define-class <npsv-fixed-port> (<npsv-port>)
  ((fiexed-info )))

(define-class <npsv-module> ()
  ((name :init-keyword :name)
   (type :init-keyword :type)
   (ports :init-value '())
   (comment :init-keyword :comment :init-value "")
   (function)))


(define-method initialize ((self <npsv-module>) initargs)
  (next-method)
  (let ((clk (make <npsv-port> :name "clk" :dir 'input :spetial #t))
	(reset_x (make <npsv-port> :name "reset_x" :dir 'input :spetial #t)))
    (add-port self clk)
    (add-port self reset_x)))

(define-method add-port ((inst <npsv-module>) (port <npsv-port>))
  (set! (ref inst 'ports) (cons port (ref inst 'ports))))
    

(define-method print ((inst <npsv-module>))
  (print (string-concatenate  (list "<npsv-module> " (ref inst 'name) " (" (symbol->string (ref inst 'type)) ")" )))
  ;(next-method)
  )


(define print-instance
  (lambda (inst)
    (print inst)
    (print (string-concatenate (list "\"" (ref inst 'comment) "\"")))
    (dolist (p (ref inst 'ports))
            (let ((s (string-concatenate
                      (list "Port:" (ref p 'name) " (" (symbol->string (ref p 'dir)) ")"))))
              (print s)))))


;;;--------------------------------------------------------------------------------
;;; file
;;;--------------------------------------------------------------------------------
(define load-setting-file
  (lambda (fname)
    (format #t "load ~A" fname)
    (load fname)))



;;misc
(define usage-exit
  (lambda (program-name)
    (format #t "Usage: gosh ~A setting-file~%" program-name)
    (exit 1)))




