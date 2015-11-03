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
   (ports)
   (comment :init-keyword :comment :init-value "")
   (function)))










