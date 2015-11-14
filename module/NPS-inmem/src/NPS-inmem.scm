(use srfi-1)
(add-load-path "../../../common/" :relative)
(require "npsv")

(define *instance* '())

(define print-setting
  (lambda ()
    (format #t "module name ~A~%" *npsv-module-name*)
    (format #t "data number ~A~%" *npsv-data-num*)
    (format #t "initialize file ~A~%" *npsv-init-file*)
    (format #t "fixed number W ~A I ~A~%" *npsv-W* *npsv-I*)
    (format #t "delta T ~A ~%" *npsv-delta-T*)

    (format #t "output dir ~A~%" *npsv-rtl-output-dir*)
    (format #t "testbench dir ~A~%" *npsv-testbench-output-dir*)
    (format #t "template dir ~A~%" *npsv-template-output-dir*)))

(define make-instance
  (lambda (name data-num init-file W I delta)
    (let ([inst (make <npsv-module> :name name :type 'NPS-inmem :comment "input memory module")]
          [adr_w (datanum->adr-w data-num)])
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
      (add-port inst (make <npsv-port> :name "cpu_adr" :dir 'input :lsb 0 :msb (- adr_w 1)))
      (add-port inst (make <npsv-port> :name "cpu_data" :dir 'input :lsb 0 :msb (- W 1)))
      (add-port inst (make <npsv-port> :name "cpu_wr" :dir 'input))
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
                    *npsv-init-file*
                    *npsv-W* *npsv-I*
                    *npsv-delta-T*))
  ;(print-instance *instance*)
  (make-verilog-file *instance* *npsv-rtl-output-dir*
                      (eval rtl-template (interaction-environment)))
  (make-template *instance* *npsv-template-output-dir*)
  (make-initialize-file *instance* *npsv-init-file* *npsv-testbench-output-dir* *npsv-W* *npsv-I*)
  (make-verilog-testbench-file *instance* *npsv-testbench-output-dir*
                                (eval testbench-template (interaction-environment))))


;;; --------------------------------------------------------------------------------
;;; verilog source
;;; --------------------------------------------------------------------------------
(define rtl-template
  '#"module ~*npsv-module-name* # (parameter DATA_WIDTH = ~*npsv-W* , DATA_NUM = ~*npsv-data-num*, DELTA_T = ~*npsv-delta-T* , ADR_WIDTH = ~(datanum->adr-w *npsv-data-num*), DELTA_WIDTH = ~(datanum->adr-w *npsv-delta-T*) )
(
 input 			     clk,
 input 			     reset_x,
 input 			     start,
 input 			     set,
 output reg 		     vo,
 output reg 		     fo,
 output reg [DATA_WIDTH-1:0] datao,

 //CPU I/F
 input [ADR_WIDTH-1:0] 	     cpu_adr,
 input [DATA_WIDTH-1:0]      cpu_data,
 input 			     cpu_wr			     
  
);
  reg [ADR_WIDTH:0] 	     adr_cnt;
  reg [DELTA_WIDTH:0] 	     delta_cnt;
  reg 			     delta_cnt_en;
  reg 			     en;
  reg [DATA_WIDTH-1:0] mem [0:DATA_NUM-1];

  // CPU write
  always @ (posedge clk or negedge reset_x) begin
    if(cpu_wr)begin
      mem[cpu_adr] <= cpu_data;
    end
  end

  // mem read
  always @ (posedge clk or negedge reset_x) begin
    if(reset_x == 1'b0)begin
      datao <= DATA_WIDTH-1'd0;
    end else begin
      if(adr_cnt<DATA_NUM)begin
	datao <= mem[adr_cnt];
      end else begin
	datao <= 0;
      end
    end
  end

  //delta wait
  always @ (posedge clk or negedge reset_x) begin
    if(reset_x == 0)begin
      delta_cnt_en <= 0;
    end else begin
      if(start)begin
	delta_cnt_en <= 1;
      end
    end
  end
  
  always @ (posedge clk or negedge reset_x) begin
    if(reset_x == 0)begin
      delta_cnt <= 0;
    end else begin
      if(delta_cnt == DELTA_T)begin
	delta_cnt <= delta_cnt;
      end else if(delta_cnt_en)begin
	delta_cnt <= delta_cnt + 1;
      end
    end
  end 

  always @ (posedge clk or negedge reset_x) begin
    if(reset_x == 0)begin
      en <= 0;
    end else begin
      if(delta_cnt == DELTA_T-1)begin
	en <= 1;
      end else if(adr_cnt == DATA_NUM - 1)begin
	en <= 0;
      end
    end
  end 


  //adr cnt
 always @ (posedge clk or negedge reset_x)begin
    if(reset_x == 0)begin
      adr_cnt <= 0;
    end else begin
      if(adr_cnt == DATA_NUM)begin
	adr_cnt <= 0;
      end else if(en)begin
	adr_cnt <= adr_cnt + 1;
      end
    end
 end 
  
 always @ (posedge clk or negedge reset_x) begin
    if(reset_x == 0)begin
      vo <= 0;
    end else begin
      vo <= en;
    end
 end
  
 always @ (posedge clk or negedge reset_x) begin
   if(reset_x == 0)begin
     fo <= 0;
    end else begin
      if(adr_cnt == DATA_NUM)begin
	fo <= 1;
      end else begin
	fo <= 0;
      end
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

