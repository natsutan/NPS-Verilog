(use srfi-1)

(define-module npsv-outmem
  (export make-outmem-from-file))

(define *outmnem-parameters*
  '(*npsv-data-num*
    *npsv-W*
    *npsv-I*
    *npsv-module-name*
    *npsv-rtl-output-dir*
    *npsv-testbench-output-dir*
    *npsv-template-output-dir*))

(define-class <npsv-outmem> (<npsv-module>)
  ((data-num :init-keyword :data-num)
   (W :init-keyword :W)
   (I :init-keyword :I)))

(define-method print-setting ((inst <npsv-outmem>))
  (next-method)
  (format #t "data number ~A~%" (ref inst 'data-num))
  (format #t "fixed number W ~A I ~A~%" (ref inst 'W) (ref inst 'I)))

;; TODO separate make-verilog-file
(define make-outmem-from-file
 (lambda (fname)
  (clear-global-parameters! *outmnem-parameters*)
  (load-setting-file fname)
  (let ((inst  (make-outmem-instance
                *npsv-module-name*
                *npsv-data-num*
                *npsv-W*
                *npsv-I*
                *npsv-rtl-output-dir*
                *npsv-testbench-output-dir*
                *npsv-template-output-dir*)))
    (make-verilog-file inst)
    inst)))



(define make-outmem-instance
  (lambda (name data-num W I rtl-odir tb-odir temp-odir)
    (let ([inst (make <npsv-outmem>  :name name :type 'npsv-outmem :comment "output memory module"
                      :data-num data-num :W W :I I
                      :rtl-output-dir rtl-odir :testbench-output-dir tb-odir :template-ouput-dir temp-odir)]
          [adr_w (datanum->adr-w data-num)])
      (add-port inst (make <npsv-port> :name "start" :dir 'input))
      (add-port inst (make <npsv-port> :name "set" :dir 'input))
      (add-port inst (make <npsv-port> :name "vi" :dir 'input))
      (add-port inst (make <npsv-port> :name "fi" :dir 'input))
      (add-port inst (make <npsv-port> :name "vo" :dir 'output))
      (add-port inst (make <npsv-port> :name "fo" :dir 'output :type 'reg))
      (let ((fixed (make <npsv-fixed> :W W :I I)))
        (add-port inst
                  (make <npsv-fixed-port>
                    :name "datai"
                    :dir 'input
                    :msb (- W 1)
                    :lsb 0
                    :fixed-info fixed
                    )))
      (add-port inst (make <npsv-port> :name "cpu_adr" :dir 'input :lsb 0 :msb (- adr_w 1)))
      (add-port inst (make <npsv-port> :name "cpu_data" :dir 'output :lsb 0 :msb (- W 1)))
      (add-port inst (make <npsv-port> :name "cpu_rd" :dir 'input))
      inst)))




(define-method make-verilog-file ((inst <npsv-outmem>))
  (write-verilog-file inst (eval outmem-rtl-template (interaction-environment))))

(define-method make-verilog-testbench-file ((inst <npsv-outmem>))
  (write-verilog-testbench-file inst (eval outmem-testbench-template (interaction-environment))))


(define-method write-module-instantiation (fp (m <npsv-outmem>) channels)
  (let* ([name (ref m 'name)]
         [cpu-adr-name (outmem-cpu-adr-name name)]
         [cpu-data-name (outmem-cpu-data-name name)]
         [cpu-rd-name (outmem-cpu-rd-name name)]
         [cpu-fo-name (outmem-cpu-fo-name name)]
         [input-ch (find
                     (lambda (ch)
                       (eq? (ref ch 'dst) m))
                     channels)]
         )
    
    (format fp "\t~A ~A (\n" name name)
    (write-common-connection fp)
    (write-port-assign fp cpu-adr-name cpu-adr-name)
    (write-port-assign fp cpu-data-name cpu-data-name)
    (write-port-assign fp cpu-rd-name cpu-rd-name)
    (write-port-assign fp cpu-fo-name cpu-fo-name)
   
    (when (not input-ch)
      (format #t "Error:no input ~A~%" name))

    (write-inport-assign fp "vi" "vo" input-ch)
    (write-inport-assign fp "fi" "fo" input-ch)
    (write-inport-assign fp "datai" "data_o" input-ch)


    (format fp "\t);\n");
    ))

(define outmem-cpu-adr-name
  (lambda (name)
    (string-append name "_cpu_adr")))

(define outmem-cpu-data-name
  (lambda (name)
    (string-append name "_cpu_data")))

(define outmem-cpu-rd-name
  (lambda (name)
    (string-append name "_cpu_rd")))

(define outmem-cpu-fo-name
  (lambda (name)
    (string-append name "_cpu_fo")))


(define-method add-top-ports (top (inst <npsv-outmem>))
  (let ([name (ref inst 'name)]
        [W (ref inst 'W)]
        [I (ref inst 'I)]
        [adr_w (datanum->adr-w (ref inst 'data-num))])
    (add-port top (make <npsv-port> :name (string-append name "_cpu_adr") :dir 'input :lsb 0 :msb (- adr_w 1)))
    (add-port top (make <npsv-port> :name (string-append name "_cpu_data") :dir 'output :lsb 0 :msb (- W 1)))
    (add-port top (make <npsv-port> :name (string-append name "_cpu_rd") :dir 'input))
    (add-port top (make <npsv-port> :name (string-append name "_fo") :dir 'output))
  
  ))


;;; --------------------------------------------------------------------------------
;;; verilog source
;;; --------------------------------------------------------------------------------
(define outmem-rtl-template
'#"module ~*npsv-module-name* # (parameter DATA_WIDTH = ~*npsv-W* , DATA_NUM = ~*npsv-data-num* , ADR_WIDTH = ~(datanum->adr-w *npsv-data-num*) )
(
 input 			     clk,
 input 			     reset_x,
 input 			     start,
 input 			     set,
 input 			     vi,
 input 			     fi,
 output reg 		     vo,
 output reg 		     fo,
 input [DATA_WIDTH-1:0]     datai,

 //CPU I/F
 input [ADR_WIDTH-1:0] 	     cpu_adr,
 output reg [DATA_WIDTH-1:0] cpu_data,
 input 			     cpu_rd			     
  
);
  reg [ADR_WIDTH:0] 	     adr_cnt;
  reg [DATA_WIDTH-1:0] mem [0:DATA_NUM-1];

  // CPU read
  always @ (posedge clk or negedge reset_x) begin
    if(reset_x == 1'b0)begin
      cpu_data <= 0;
    end else if(cpu_rd)begin
      cpu_data <= mem[cpu_adr];
    end 
  end

  // mem write
  always @ (posedge clk or negedge reset_x) begin
    if(vi)begin
      mem[adr_cnt] <= datai;
    end
  end
  
  //adr cnt
 always @ (posedge clk or negedge reset_x)begin
    if(reset_x == 0)begin
      adr_cnt <= 0;
    end else begin
      if(vi)begin
	adr_cnt <= adr_cnt + 1;
      end
    end
 end 
  
 always @ (posedge clk or negedge reset_x) begin
    if(reset_x == 0)begin
      vo <= 0;
      fo <= 0;
    end else begin
      vo <= vi;
      fo <= fi;
    end
 end

  
endmodule // mem
"
  
  )

(define outmem-testbench-template
  '#"
module ~|*npsv-module-name*|_tb();
  parameter DATA_WIDTH = ~*npsv-W*;
  parameter DATA_NUM = ~*npsv-data-num*;
  parameter ADR_WIDTH = ~(datanum->adr-w *npsv-data-num*) ;

  reg 			     clk;
  reg 			     reset_x;
  reg 			     start;
  reg 			     set;
  wire 			     vo;
  wire 			     fo;
  reg 			     vi;
  reg 			     fi;
  reg [DATA_WIDTH-1:0] 	     datai;

  //CPU I/F
  reg [ADR_WIDTH-1:0] 	     cpu_adr;
  wire [DATA_WIDTH-1:0]      cpu_data;
  reg 			     cpu_rd;

  integer 		     i;
  integer 		     fp;
  
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
     .vi(vi),
     .fi(fi),
     .datai(datai),
     .cpu_adr(cpu_adr),
     .cpu_data(cpu_data),
     .cpu_rd(cpu_rd)
     );


  initial begin
    fp=$fopen(\"~|*npsv-module-name*|_dump.txt\");
    #1 reset_x = 1; cpu_adr = 0; cpu_rd = 0; set = 0; start = 0; datai = 0; vi = 0; fi = 0;
    # (PERIOD * 3)  reset_x = 0;
    # (PERIOD * 5)  reset_x = 1;

    for(i=0;i<DATA_NUM;i=i+1)begin
      #(PERIOD) vi = 1; datai = i;
    end
    vi = 0;
    #(PERIOD)
    fi = 1;
    @(posedge fo)      

    for(i=0;i<DATA_NUM;i=i+1)begin
      #(PERIOD) cpu_rd = 1; cpu_adr = i;
      #(PERIOD) $fwrite(fp, \"%X\\n\", cpu_data);
    end
    cpu_rd = 0;

      
    # (PERIOD * 10)  $finish();
  end
  
endmodule // NPS
"
  )
(provide "npsv-outmem")
