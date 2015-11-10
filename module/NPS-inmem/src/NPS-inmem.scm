(use srfi-1)
(add-load-path "../../../common/" :relative)
(require "npsv")

(define *instance* '())


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
      
      ;; reg
      
      ;; process
      
      inst)))

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
  (print-instance *instance*)
  (write-verilog-file *instance* *npsv-rtl-output-dir*
                      (eval rtl-template (interaction-environment)))
  )



