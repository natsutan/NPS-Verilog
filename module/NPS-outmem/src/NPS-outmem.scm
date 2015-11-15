(use srfi-1)
(add-load-path "../../../common/" :relative)
(require "npsv")

(define *instance* '())

(define print-setting
  (lambda ()
    (format #t "module name ~A~%" *npsv-module-name*)
    (format #t "data number ~A~%" *npsv-data-num*)
    (format #t "fixed number W ~A I ~A~%" *npsv-W* *npsv-I*)

    (format #t "output dir ~A~%" *npsv-rtl-output-dir*)
    (format #t "testbench dir ~A~%" *npsv-testbench-output-dir*)
    (format #t "template dir ~A~%" *npsv-template-output-dir*)))

(define make-instance
  (lambda (name data-num W I)
    (let ([inst (make <npsv-module> :name name :type 'NPS-outmem :comment "output memory module")]
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

(define read-write-initialize-file
  (lambda (fpi fpo W I adr)
    (let ((buf (read-line fpi)))
      (when (not (eof-object? buf))
        (let ((n (string->number buf)))
          (when n              
            (let ((s (toFix n W I)))
              (format fpo "\tcpu_wr_task(~A,~A);  // ~A~%" adr s buf)
              (read-write-initialize-file fpi fpo W I (+ adr 1)))))))))

(define write-initialize-file-header
  (lambda (fp name W I )
    (write-header fp name)
    (format fp "// W = ~A, I = ~A~%" W I)))

;;; --------------------------------------------------------------------------------
;;; main
;;; --------------------------------------------------------------------------------
(define (main args)
  (when (not (= (length args) 2))
    (usage-exit (car args)))
  
  (load-setting-file (second args))
  (print-setting)
  (set! *instance* (make-instance
                    *npsv-module-name*
                    *npsv-data-num*
                    *npsv-W* *npsv-I*
                    ))
  (print-instance *instance*)
  (make-verilog-file *instance* *npsv-rtl-output-dir*
                      (eval rtl-template (interaction-environment)))
  (make-template *instance* *npsv-template-output-dir*)
  (make-verilog-testbench-file *instance* *npsv-testbench-output-dir*
                                (eval testbench-template (interaction-environment)))
  )

;;; --------------------------------------------------------------------------------
;;; verilog source
;;; --------------------------------------------------------------------------------
(define rtl-template
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

(define testbench-template
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
