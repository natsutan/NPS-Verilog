(use srfi-19)
(use srfi-13)
(use file.util)

(define-class <npsv-fixed> ()
  ((W :init-keyword :W) ; Whole bit width
   (I :init-keyword :I) ; Inter bit width
   (Q :init-keyword :Q :init-value 'SC_TRN)  ; quantize mode
   (O :init-keyword :O :init-value #f)  ; overflow mode, reserved
   (N :init-keyword :N :init-value #f)  ; reserved
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
  ((fixed-info :init-keyword :fixed-info)))

(define-class <npsv-module> ()
  ((name :init-keyword :name)
   (type :init-keyword :type)
   (ports :init-value '())
   (comment :init-keyword :comment :init-value "")))

(define-class <npsv-process> ()
  ((label :init-keyword :label :init-value "")
   (reset_process :init-keyword :reset)
   (process :init-keyword :process)))

(define-class <npsv-initial-process> (<npsv-process>)
  ())

(define-class <npsv-wire> ()
   ((name :init-keyword :name)
    (assign :init-keyword :assign)))

   
(define-method initialize ((self <npsv-module>) initargs)
  (next-method)
  (let ((clk (make <npsv-port> :name "clk" :dir 'input :spetial #t))
	(reset_x (make <npsv-port> :name "reset_x" :dir 'input :spetial #t)))
    (add-port self clk)
    (add-port self reset_x)))

(define-method add-port ((inst <npsv-module>) (port <npsv-port>))
  (set! (ref inst 'ports) (append (ref inst 'ports) (cons port '()))))
    
(define-method add-process ((inst <npsv-module>) (process <npsv-process>))
  (set! (ref inst 'processes) (cons process (ref inst 'processes))))
    
(define-method add-wire ((inst <npsv-module>) (process <npsv-wire>))
  (set! (ref inst 'wires) (cons processes (ref inst 'wires))))

(define-method print ((inst <npsv-module>))
  (print (string-concatenate  (list "<npsv-module> " (ref inst 'name) " (" (symbol->string (ref inst 'type)) ")" )))
  ;(next-method)
  )

(define-method print ((p <npsv-port>))
  (print (make-port-string p)))

(define-method print ((p <npsv-fixed-port>))
  (let ((fp (ref p 'fixed-info)))
    (let ((W (number->string (ref fp 'W)))
          (I (number->string (ref fp 'I))))
      (print (string-append
              (make-port-string p)
              "   (W:" W ",I:" I ")")))))

(define print-instance
  (lambda (inst)
    (print inst)
    (print (string-concatenate (list "\"" (ref inst 'comment) "\"")))
    (dolist (p (ref inst 'ports)) (print p))))

(define make-port-string
  (lambda (p)
    (let* ((dir (symbol->string (ref p 'dir)))
           (lsb (ref p 'lsb))
           (msb (ref p 'msb))
           (name (ref p 'name))
           (slise (if (and (zero? lsb) (zero? msb))
                      ""
                      (string-append "[" (number->string msb) ":" (number->string lsb) "]"))))
      (string-append dir " " slise " " name))))


;;;--------------------------------------------------------------------------------
;;; RTL
;;;--------------------------------------------------------------------------------
(define write-header
  (lambda (fp name)
    (format fp "//-------------------------------------------------~%")
    (format fp "// ~A~%" name)
    (format fp "// This file was auto-generated by npsv ~A~%" (date->string (current-date) "~5"))
    (format fp "//-------------------------------------------------~%")))


(define bit-slice-str
  (lambda (lsb msb)
    (if (and (zero? lsb) (zero? msb))
        ""
        (string-append "[" (number->string msb) ":" (number->string lsb) "]"))))

(define comment-str
  (lambda (comment)
    (if (string=? comment "")
        ""
        (string-append "// " comment))))

(define write-cr
  (lambda (fp)
    (format fp "~%")))

(define make-verilog-file
  (lambda (inst dir text)
    (let* ([name (ref inst 'name)]
           [fp (open-verilog-file dir name)])
      (write-header fp name)
      (format fp text)
      (close-verilog-file fp))))

(define make-verilog-testbench-file
  (lambda (inst dir text)
    (let* ([name (ref inst 'name)]
           [fp (open-verilog-file dir (string-append name "_tb"))])
      (write-header fp name)
      (format fp text)
      (close-verilog-file fp))))


(define write-template-ports
  (lambda (fp ports)
    (let ([name (ref (car ports) 'name)])
      (if (= (length ports) 1)
          (format fp "\t\t~A()~%" name)  ;; no camma
          (begin
            (format fp "\t\t~A(),~%" name)
            (write-template-ports fp (cdr ports)))))))

(define make-template
  (lambda (inst dir)
    (let* ([name (ref inst 'name)]
           [fp (open-verilog-file dir (string-append name "_template"))])
      (write-header fp name)
      (write-cr fp)
      (format fp "\t~A ~A (~%" name name)
      (write-template-ports fp (ref inst 'ports))
      (format fp "\t);~%")
      (close-verilog-file fp))))

(define read-write-initialize-file
  (lambda (fpi fpo W I adr)
    (let ((buf (read-line fpi)))
      (when (not (eof-object? buf))
        (let ((n (string->number buf)))
          (when n              
            (let ((s (toFix n W I)))
              (format fpo "\tcpu_write(~A,~A);  // ~A~%" adr s buf)
              (read-write-initialize-file fpi fpo W I (+ adr 1)))))))))

(define write-initialize-file-header
  (lambda (fp name W I )
    (write-header fp name)
    (format fp "// W = ~A, I = ~A~%" W I)))


(define make-initialize-file
  (lambda (inst initfilename odir W I)
    (let* ([name (ref inst 'name)]
           [fpi (open-input-file initfilename) ]
           [fpo (open-verilog-file odir (string-append name "_init"))])
      (format #f "open ~A~%" initfilename)
      (write-initialize-file-header fpo name W I)
      (read-write-initialize-file fpi fpo W I 0)
      (close-verilog-file fpo)
      (close-input-port fpi)
      )))

(define quantize
  (lambda (x)
    (if (< x 0)
        (- x 1)
        x)))

(define toFix
  (lambda (x W I)
    (let ((val (* x (power 2 (- W I 1))))
          (max (- (power 2 (- W 1)) 1))
          (min (- (power 2 (- W 1)))))
      (let ((val_int (exact (values-ref (modf val) 1))))
        (clamp  (quantize val_int)  min  max)))))


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
    (format #t "load ~A" fname)
    (load fname)))

;;misc
(define usage-exit
  (lambda (program-name)
    (format #t "Usage: gosh ~A setting-file~%" program-name)
    (exit 1)))

(define power
  (lambda (x n)
    (if (= n 1)
        x
        (* (power x (- n 1)) x))))

(define datanum->adr-w
  (lambda (datanum)
    (ceiling (log datanum 2))))


