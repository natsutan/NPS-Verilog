(define-module npsv-build-top
  (export make-all-rtl npsv-get-top make-top-testbench))

(define *top-inst* '())

(define-class <npsv-top> (<npsv-module>)
  ((module :init-keyword :module)
   (ch :init-keyword :ch)
   (wires :init-value '())
   ))

(define-class <npsv-wire> ()
  ((name :init-keyword :name)
   (lsb :init-keyword :lsb :init-value 0)
   (msb :init-keyword :msb :init-value 0)))

(define ch->wire-prefix
  (lambda (ch)
    (string-append (ref ch 'name) "_")))

(define write-outport-assign
  (lambda (fp name ch . last-opt)
    (let ([wire (string-append (ch->wire-prefix ch) name)]
          [last (get-optional last-opt #f)])
      (if last
          (format fp "\t\t.~A(~A)\n" name wire)
          (format fp "\t\t.~A(~A),\n" name wire)))))

(define write-inport-assign
  (lambda (fp inport-name outport-name ch . last-opt)
    (let ([wire (string-append (ch->wire-prefix ch) outport-name)]
          [last (get-optional last-opt #f)])          
      (if last
          (format fp "\t\t.~A(~A)\n" inport-name wire)
          (format fp "\t\t.~A(~A),\n" inport-name wire)))))

(define make-wires-from-ch
  (lambda (ch)
    (let ([src (ref ch 'src)]
          [dst (ref ch 'dst)]
          [prefix (ch->wire-prefix ch)]
          [wires '()])
      (set! wires (cons (make <npsv-wire> :name (string-append prefix "vo")) wires))
      (set! wires (cons (make <npsv-wire> :name (string-append prefix "fo")) wires))
      (set! wires (cons
                   (make-wire-from-dataport (find-src-port src) (find-dst-port dst) (string-append prefix "datao"))
                   wires))
      wires)))

(define-method make-wire-from-dataport ((src-port <npsv-port>) (dst-port <npsv-port>) name)
  (make <npsv-wire> :name name :lsb 0 :msb (ref dst-port 'msb)))

;; data port connection
; 出力ポートは全ビットを出し、入力ポートで調整

(define make-top-wires
  (lambda (top)
    (dolist (ch (ref top 'ch))
            (add-top-wires top (make-wires-from-ch ch)))))
    
(define add-top-wires
  (lambda (inst wires)
    (set! (ref inst 'wires)
          (concatenate (list wires (ref inst 'wires))))))


(define make-top-module
  (lambda ()
    (set! *top-inst*
          (make <npsv-top> :name *top-name* :type 'npsv-top
                :module (npsv-get-modules) :ch (npsv-get-ch)))))

(define make-top-ports 
  (lambda (top)
    (add-port top (make <npsv-port> :name "start" :dir 'input))
    (add-port top (make <npsv-port> :name "set" :dir 'input))
    (dolist (m (ref top 'module))
            (add-top-ports top m))))

(define-method add-top-ports (top (m <npsv-module>))
  #t)

(define npsv-get-top
  (lambda ()
    *top-inst*))

(define make-all-rtl
  (lambda (odir)
    (make-top-module)
    ;(set! (ref *top-inst* 'rtl-output-dir) odir)
    (set! (ref *top-inst* 'rtl-output-dir) ".")
    (set! (ref *top-inst* 'template-ouput-dir) ".")

    (make-top-ports *top-inst*)
    (make-top-wires *top-inst*)
    (write-top-verilog odir *top-inst*)
;    (dolist (m (ref *top-inst* 'module))
;            (make-verilog-file m))    
    (make-template *top-inst*)  
    (print *top-inst*)))

(define write-top-ports
  (lambda (fp ports)
    (if (= (length ports) 1)
        (format fp "\t~A~%" (make-port-string (car ports)))  ; no comma
        (begin
          (format fp "\t~A,~%" (make-port-string (car ports)))
          (write-top-ports fp (cdr ports))))))

(define write-top-wires
  (lambda (fp wires)
    (dolist (c wires)
            (format fp "\twire ~A;~%" (make-wire-string c)))))


(define write-top-instances
  (lambda (fp top)
    (dolist (m (ref top 'module))
            (begin
              (write-module-instantiation fp m (ref top 'ch))
              (write-cr fp)))))

(define-method write-module-instantiation (fp (m <npsv-module>) channels)
  (format #t "Error:not implemented instantiaon ~A~%" (ref m 'name)))

(define write-common-connection
  (lambda (fp)
    (format fp "\t\t.clk(clk),\n")
    (format fp "\t\t.reset_x(reset_x),\n")
    (format fp "\t\t.set(set),\n")
    (format fp "\t\t.start(start),\n")))

(define write-port-assign
  (lambda (fp port sig . last-opt)
    (let ((last (get-optional last-opt #f)))
      (if last
          (format fp "\t\t.~A(~A)\n" port sig)
          (format fp "\t\t.~A(~A),\n" port sig)))))

(define write-top-verilog
  (lambda (dir inst)
    (let* ([name (ref inst 'name)]
           [fp (open-verilog-file dir name)])
      (write-header fp name)
      (format fp "/* verilator lint_off SYMRSVDWORD */\n")
      (format fp "module ~A\n" name)
      (format fp "(\n")
      (write-top-ports fp (ref inst 'ports))
      (format fp ");\n")
      (write-top-wires fp (ref inst 'wires))
      (write-cr fp)
      (write-top-instances fp inst)
      
      (format fp "endmodule\n")
      (close-verilog-file fp)
      )))

(define port->reg-or-wire
  (lambda (p)
    (let ([dir (ref p 'dir)]
          [name (ref p 'name)]
          [bit (bit-slice-str (ref p 'lsb) (ref p 'msb))]) 
      (if (eq? dir 'input)
          (format #f "reg ~A ~A" bit name)
          (format #f "wire ~A ~A" bit name)))))


(define write-registers-and-wires
  (lambda (fp top)
    (let ([ports (ref top 'ports)])
      (dolist (p ports)
              (format fp "\t~A;\n" (port->reg-or-wire p))))))

(define write-clk
  (lambda (fp)
    (format fp "\tparameter PERIOD = 10.0;\n")
    (format fp "\talways # (PERIOD/2) clk = !clk;\n")
    (format fp "\tinitial begin \n")
    (format fp "\t\tclk = 1;\n")
    (format fp "\tend\n")))

(define write-top-instance
  (lambda (fp top)
    (let ([name (ref top 'name)]
          [ports (ref top 'ports)])
      (format fp "\t~A U0 (\n" name)
      (write-port-connection-with-same-name fp ports)
      (format fp "\t);\n"))))

(define write-port-connection-with-same-name
  (lambda (fp ports)
    (let* ([p (car ports)]
           [name (ref p 'name)])
      (if (= (length ports) 1)
          (format fp "\t\t.~A(~A)\n" name name)
          (begin
            (format fp "\t\t.~A(~A),\n" name name)
            (write-port-connection-with-same-name fp (cdr ports)))))))

(define write-host-wr-task
  (lambda (fp top)
    (define host-wr-string
      (lambda (m)
        (let* ([name (ref m 'name)]
               [cpu-adr-name (inmem-cpu-adr-name name)]
               [cpu-data-name (inmem-cpu-data-name name)]
               [cpu-wr-name (inmem-cpu-wr-name name)]
               [adr-w (datanum->adr-w (ref m 'data-num))]
               [data-w (ref m 'W)])
          (format fp "\ttask ~A_wr_task;\n" name)
          (format fp "\t\tinput [~A:0] adr;\n" (- adr-w 1))
          (format fp "\t\tinput [~A:0] data;\n" (- data-w 1))
          (format fp "\t\tbegin\n")
          (format fp "\t\t\t@(posedge clk);\n")
          (format fp "\t\t\t~A = 1;\n"  cpu-wr-name)
          (format fp "\t\t\t~A = adr;\n" cpu-adr-name)
          (format fp "\t\t\t~A = data;\n" cpu-data-name)
          (format fp "\t\t\t@(posedge clk);\n")
          (format fp "\t\t\t~A = 0;\n"  cpu-wr-name)
          (format fp "\t\t\t@(posedge clk);\n")
          (format fp "\t\tend\n")
          (format fp "\tendtask\n")
          
          )))
    (map host-wr-string (filter (lambda (m) (eq? (class-of m) <npsv-inmem>)) (ref top 'module)))))


(define all-finish-str
  (lambda (outmems)
    (if (= (length outmems) 1)
        (outmem-cpu-fo-name (ref (car outmems) 'name))
        (string-append "& " (outmem-cpu-fo-name (ref outmems 'name)) (all-finish-str (cdr outmems))))))


(define write-top-iniitial
  (lambda (fp top dir)
    (let ([inmems (filter (lambda (m) (eq? (class-of m) <npsv-inmem>)) (ref top 'module))]
          [outmems (filter (lambda (m) (eq? (class-of m) <npsv-outmem>)) (ref top 'module))]
          )
      (format fp "\twire finish_all;\n");
      (format fp "\tassign finish_all = ~A;\n" (all-finish-str outmems))
      (format fp "\n")
      
      (format fp "\tinitial begin\n")
      (format fp "\t\t#1 reset_x = 1; set = 0; start = 0;\n")

      (dolist (m inmems)
              (let ([name (ref m 'name)])
                (format fp "\t\t~A = 0; ~A = 0; ~A = 0;\n" (inmem-cpu-adr-name name) (inmem-cpu-data-name name) (inmem-cpu-wr-name name))))
      
      (format fp "\t\t# (PERIOD * 3)  reset_x = 0;\n")
      (format fp "\t\t# (PERIOD * 5)  reset_x = 1;\n")

      (dolist (m inmems)
                (format fp "\t\t`include \"~A_init.v\"\n" (ref m 'name)))
      
      (format fp "\t\t# (PERIOD * 3) set = 1;\n")
      (format fp "\t\t# (PERIOD) set = 0;\n")
      (format fp "\t\t# (PERIOD * 3) start = 1;\n")
      (format fp "\t\t$display(\"start\");\n")
      (format fp "\t\t# (PERIOD) start = 0;\n")

      (format fp "\t\t@(posedge finish_all)\n")
      
      (format fp "\t\t# (PERIOD * 10)\n")
      (format fp "\t\t$display(\"finish\");\n")
      (format fp "\t\t$finish();\n")
      (format fp "\tend\n"))))
      

(define make-top-testbench
  (lambda (dir)
    (let* ([top *top-inst*]
           [tb_name (string-append (ref top 'name) "_tb")]
           [fp (open-verilog-file dir tb_name)])
      (write-header fp tb_name)
      (format fp "module ~A ();\n" tb_name)
      (write-registers-and-wires fp top)
      (write-cr fp)
      (write-clk fp)
      (write-cr fp)
      (write-top-instance fp top)
      (write-cr fp)
      (write-host-wr-task fp top)
      (write-cr fp)
      (write-top-iniitial fp top dir)
      (write-cr fp)
      (format fp "endmodule\n")
      (close-verilog-file fp)
    )))


(provide "npsv-build-top")




