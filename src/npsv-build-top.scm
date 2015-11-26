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
    (format fp "\t\t.reset(reset),\n")
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
            (format fp "\t\t.~A(~A).\n" name name)
            (write-port-connection-with-same-name fp (cdr ports)))))))

(define write-top-iniitial
  (lambda (fp top)
    (format fp 


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
      (write-top-iniitial fp top)
      (write-cr fp)
      (format fp "endmodule\n")
      (close-verilog-file fp)
    )))


(provide "npsv-build-top")



"
  task cpu_wr_task;
    input [ADR_WIDTH-1:0] adr;
    input [DATA_WIDTH-1:0] data;
    begin
       @(posedge clk);
       cpu_wr = 1;
       cpu_adr = adr;
       cpu_data = data;
       @(posedge clk);
       cpu_wr = 0;
       @(posedge clk);
    end
  endtask


  initial begin

    #1 reset_x = 1; cpu_adr = 0; cpu_wr = 0; set = 0; start = 0; cpu_data = 0;
    # (PERIOD * 3)  reset_x = 0;
    # (PERIOD * 5)  reset_x = 1;

    `include "sample_init.v"

    # (PERIOD * 3) set = 1;
    # (PERIOD) set = 0;
    # (PERIOD * 3) start = 1;
    # (PERIOD) start = 0;
    
    @(posedge fo)
    
    # (PERIOD * 10)  $finish();
  end
"


