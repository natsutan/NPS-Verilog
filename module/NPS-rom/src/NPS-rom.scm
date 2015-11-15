(use srfi-1)
(add-load-path "../../../common/" :relative)
(require "npsv")

(define *instance* '())
(define *data* '())

(define print-setting
  (lambda ()
    (format #t "function ~A~%" sin)
    (format #t "range ~A - ~A~%" *npsv-min* *npsv-max*)
    (format #t "adr width ~A~%"  *npsv-adr-width*)
    (format #t "data width ~A (integer ~A)~%" *npsv-W* *npsv-I*)
    (format #t "module name ~A~%" *npsv-module-name*)
    (format #t "output dir ~A~%" *npsv-rtl-output-dir*)
    (format #t "testbench dir ~A~%" *npsv-testbench-output-dir*)
    (format #t "template dir ~A~%" *npsv-template-output-dir*)))


;;; --------------------------------------------------------------------------------
;;; make rom data
;;; --------------------------------------------------------------------------------


(define make-data
  (lambda (func min max adrw W I)
    (let* ((num (power 2 adrw))
	   (data (make-vector num))
	   (unit (/ (- max min) num)))
      (dotimes(x num)
       (vector-set! data x (toFix (func (+ (* x unit) min)) W I)))
;      (print data)
      (vector->list data))))


;;; --------------------------------------------------------------------------------
;;; verilog rtl
;;; --------------------------------------------------------------------------------

(define romdata->string
  (lambda (data dataw)
    (let ((format (- (power 2 dataw) 1)))
      (number->string (logand format data) 16))))

(define insert-rom-data
  (lambda (data dataw num)
    (if (= (length data) 0)
        ""
        (string-append
         (format #f "        ~A:datao <= ~A'h~A;~%"
                 num dataw (romdata->string (car data) dataw))
         (insert-rom-data (cdr data) dataw (+ num 1))))))

;;; --------------------------------------------------------------------------------
;;; verilog template
;;; --------------------------------------------------------------------------------
(define make-instance
  (lambda (name min max adr-w W I)
    (let ([inst (make <npsv-module> :name name :type 'NPS-rom :comment "rom module")]
          [fixed (make <npsv-fixed> :W W :I I)])
      (add-port inst (make <npsv-port> :name "start" :dir 'input :comment "no use"))
      (add-port inst (make <npsv-port> :name "set" :dir 'input :comment "no use"))
      (add-port inst (make <npsv-port> :name "vi" :dir 'input))
      (add-port inst (make <npsv-port> :name "fi" :dir 'input))
      (add-port inst (make <npsv-port> :name "vo" :dir 'output :type 'reg))
      (add-port inst (make <npsv-port> :name "fo" :dir 'output :type 'reg))
      (add-port inst (make <npsv-fixed-port> :name "datao" :dir 'output :msb (- W 1) :lsb 0 :fixed-info fixed :type 'reg))
      (add-port inst (make <npsv-port> :name "datai" :dir 'input :msb (- adr-w 1) :lsb 0 :type 'address))
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
                 *npsv-min*
                 *npsv-max*
                 *npsv-adr-width* 
                 *npsv-W*
                 *npsv-I*))
  (print-instance *instance*)
  (set! *data* (make-data *npsv-func*
                 *npsv-min*
                 *npsv-max*
                 *npsv-adr-width* 
                 *npsv-W*
                 *npsv-I*))
  (make-verilog-file *instance* *npsv-rtl-output-dir*
                     (eval rtl-template (interaction-environment)))
  (make-template *instance* *npsv-template-output-dir*)
  (make-verilog-testbench-file *instance* *npsv-testbench-output-dir*
                               (eval testbench-template (interaction-environment)))

  0)


;;; --------------------------------------------------------------------------------
;;; verilog source
;;; --------------------------------------------------------------------------------
(define rtl-template
  '#"module ~*npsv-module-name* # (parameter DATA_WIDTH = ~*npsv-W* , ADR_WIDTH = ~(ceiling *npsv-adr-width*))
  (
    input 			clk,
    input 			reset_x,
    input 			start,
    input 			set,
    input 			vi,
    input 			fi,
    input [ADR_WIDTH-1:0] 	datai,   
    output reg 			vo,
    output reg 			fo,
    output reg [DATA_WIDTH-1:0] datao
   );
  
  always @(posedge clk or negedge reset_x)begin
    if(reset_x == 1'b0)begin
      datao <= 0;
    end else begin
      case(datai)
~(insert-rom-data *data* *npsv-W* 0)
    endcase
    end
  end // always @ (posedge clk or negedge reset_x)

  always @ (posedge clk or negedge reset_x) begin
    if(reset_x == 1'b0)begin
      vo <= 0;
      fo <= 0;
    end else begin
      vo <= vi;
      fo <= fi;
    end
  end
  
endmodule

")

(define testbench-template
  '#"
module ~|*npsv-module-name*|_tb();
  parameter DATA_WIDTH = ~*npsv-W*;
  parameter ADR_WIDTH = ~*npsv-adr-width*;

  reg clk;
  reg reset_x;
  reg set;
  reg [ADR_WIDTH-1:0] datai;
  wire [DATA_WIDTH-1:0] datao;
  reg 			vi;
  reg 			fi;
  wire 			vo;
  wire 			fo;

  integer 		i;
  integer 		fp;

  parameter PERIOD = 10.0;
  always # (PERIOD/2) clk = !clk;
  initial begin clk = 1;end

  ~*npsv-module-name* U0
    (
     .clk(clk),
     .reset_x(reset_x),
     .start(start),
     .set(set),
     .vi(vi),
     .fi(fi),
     .datai(datai),   
     .vo(vo),
     .fo(fo),
     .datao(datao)
     );

  initial begin
    fp=$fopen(\"~|*npsv-module-name*|_dump.txt\");
    #1 reset_x = 1; datai = 0; fi = 0; set = 0; vi = 0;
    # (PERIOD * 3)  reset_x = 0;
    # (PERIOD * 5)  reset_x = 1;
    
    for(i=0;i<(2 ** ADR_WIDTH);i=i+1)begin
      # (PERIOD) datai = i; vi = 1;
    end
    vi = 0;
    
    fi = 1;
    @(posedge fo);
    
    # (PERIOD * 10)  
    $fclose(fp);
    $finish();
  end // initial begin

  always @ (posedge clk)begin
    if(vo)begin
      $fwrite(fp, \"%X\\n\", datao);
    end
  end
  
endmodule
")
