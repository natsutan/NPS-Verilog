(use srfi-1)

(define-module npsv-const
  (export make-const-from-file))

(define *const-parameters*
  '(*npsv-module-name*
    *npsv-W*
    *npsv-I*
    *npsv-value*
    *npsv-rtl-output-dir*
    *npsv-testbench-output-dir*
    *npsv-template-output-dir*))
               
(define-class <npsv-const> (<npsv-module>)
  ((W :init-keyword :W)
   (I :init-keyword :I)
   (value :init-keyword :value)
   ))

(define make-const-from-file
  (lambda (fname)
    (clear-global-parameters! *const-parameters*)
    (load-setting-file fname)
    (let ((inst  (make-const-instance
                  *npsv-module-name*
                  *npsv-W* *npsv-I*
                  *npsv-value*
                  *npsv-rtl-output-dir*
                  *npsv-testbench-output-dir*
                  *npsv-template-output-dir*
                  )))
      (make-verilog-file inst)
      inst)))

(define-method print-setting ((inst <npsv-const>))
  (next-method)
  (format #t "fixed number W ~A I ~A~%" (ref inst 'W) (ref inst 'I))
  (format #t "vaule ~A ~%" (ref inst 'value))
  (print-setting-dirs inst)
  )


(define make-const-instance
  (lambda (name W I value rtl-odir tb-odir temp-odir)
    (let ([inst (make <npsv-const> :name name :type 'npsv-const :comment "input const module"
                      :W W :I I :value value
                      :rtl-output-dir rtl-odir :testbench-output-dir tb-odir :template-ouput-dir temp-odir)])
      (add-port inst (make <npsv-port> :name "start" :dir 'input))
      (add-port inst (make <npsv-port> :name "set" :dir 'input))
      (add-port inst (make <npsv-port> :name "vo" :dir 'output :type 'reg))
      (add-port inst (make <npsv-port> :name "fo" :dir 'output :type 'reg))
      (let ((fixed (make <npsv-fixed> :W W :I I)))
        (add-port inst
                  (make <npsv-fixed-port>
                    :name "datao"
                    :dir 'output
                    :msb (- W 1)
                    :lsb 0
                    :fixed-info fixed
                    :type 'reg)))
      inst)))

(define-method make-verilog-file ((inst <npsv-const>))
  (set! *npsv-module-name* (ref inst 'name))
  (write-verilog-file inst (eval const-rtl-template (interaction-environment))))

(define read-write-initialize-file
  (lambda (fpi fpo name W I adr)
    (let ((buf (read-line fpi)))
      (when (not (eof-object? buf))
        (let ((n (string->number buf)))
          (when n              
            (let ((s (dec->verilolg-hex-str (toFix n W I) W)))
              (format fpo "\t~A_wr_task(~A,~A);  // ~A~%" name adr s buf)
              (read-write-initialize-file fpi fpo name W I (+ adr 1)))))))))

(define write-initialize-file-header
  (lambda (fp name W I )
    (write-header fp name)
    (format fp "// W = ~A, I = ~A~%" W I)))

(define-method make-initialize-file ((inst <npsv-const>))
  (let* ([name (ref inst 'name)]
         [initfilename (ref inst 'init-file)]
         [odir (ref inst 'testbench-output-dir)]
         [W (ref inst 'W)]
         [I (ref inst 'I)]
         [fpi (open-input-file initfilename) ]
         [fpo (open-verilog-file odir (string-append name "_init"))])
    (format #t "open ~A (read) ~%" initfilename)
    (format #t "open ~A~%" (string-append name "_init"))
    (write-initialize-file-header fpo name W I)
    (read-write-initialize-file fpi fpo name W I 0)
    (close-verilog-file fpo)
    (close-input-port fpi)
      ))

(define-method make-verilog-testbench-file ((inst <npsv-const>))
  (write-verilog-testbench-file inst (eval const-testbench-template (interaction-environment))))

(define-method add-top-ports (top (inst <npsv-const>))
  (let ([name (ref inst 'name)]
        [W (ref inst 'W)]
        [I (ref inst 'I)]
        [adr_w (datanum->adr-w (ref inst 'data-num))])
    (add-port top (make <npsv-port> :name (inmem-cpu-adr-name name) :dir 'input :lsb 0 :msb (- adr_w 1)))
    (add-port top (make <npsv-port> :name (inmem-cpu-data-name name) :dir 'input :lsb 0 :msb (- W 1)))
    (add-port top (make <npsv-port> :name (inmem-cpu-wr-name name) :dir 'input))
  ))

(define-method write-module-instantiation (fp (m <npsv-const>) channels)
  (let* ([name (ref m 'name)]
         [output-ch (find
                     (lambda (ch)
                       (eq? (ref ch 'src) m))
                     channels)]
         )
    (format fp "\t~A ~A (\n" name name)
    (write-common-connection fp)
    (write-port-assign fp "cpu_adr" cpu-adr-name)
    (write-port-assign fp "cpu_data" cpu-data-name)
    (write-port-assign fp "cpu_wr" cpu-wr-name) 
    (when (not output-ch)
      (format #t "Error:no output ~A~%" name))

    (write-outport-assign fp "vo" output-ch)
    (write-outport-assign fp "fo" output-ch)
    (write-outport-assign fp "datao" output-ch :last-flag #t)
    (format fp "\t);\n");
    ))

;;; --------------------------------------------------------------------------------
;;; verilog source
;;; --------------------------------------------------------------------------------
(define const-rtl-template
  '#"module ~*npsv-module-name* # (parameter DATA_WIDTH = ~*npsv-W*)
(
 input 			     clk,
 input 			     reset_x,
 input 			     start,
 input 			     set,
 output 		     vo,
 output 		     fo,
 output [DATA_WIDTH-1:0] datao,

);

assign vo = 1;
assign fo = 0;
assign datao = *npsv-value*;
  
endmodule // const

  "
  )

(define inmem-testbench-template
  '#"
module ~|*npsv-module-name*|_tb();
  parameter DATA_WIDTH = ~*npsv-W*;
  parameter DATA_NUM = ~*npsv-data-num*;
  parameter DELTA_T = ~*npsv-delta-T*;
  parameter ADR_WIDTH = ~(datanum->adr-w *npsv-data-num*);

 reg 			     clk;
 reg 			     reset_x;
 reg 			     start;
 reg 			     set;
 wire 		     vo;
 wire 		     fo;
 wire [DATA_WIDTH-1:0] datao;

 //CPU I/F
  reg [ADR_WIDTH-1:0]  cpu_adr;
  reg [DATA_WIDTH-1:0] cpu_data;
  reg 		       cpu_wr;			     

  parameter PERIOD = 10.0;
  always # (PERIOD/2) clk = !clk;
  initial begin 
    clk = 1;
  end
  
  ~*npsv-module-name* U0
    (
     .clk(clk),
     .reset_x(reset_x),
     .start(start),
     .set(set),
     .vo(vo),
     .fo(fo),
     .datao(datao),
     .cpu_adr(cpu_adr),
     .cpu_data(cpu_data),
     .cpu_wr(cpu_wr)
     );

  task ~|*npsv-module-name*|_wr_task;
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

    `include \"~|*npsv-module-name*|_init.v\"

    # (PERIOD * 3) set = 1;
    # (PERIOD) set = 0;
    # (PERIOD * 3) start = 1;
    # (PERIOD) start = 0;
    
    @(posedge fo)
    
    # (PERIOD * 10)  $finish();
  end

endmodule // NPS
  "
  )


(provide "npsv-const")
