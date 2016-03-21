(define-module npsv-rom
  (export make-rom-from-file))

(define *data* '())

(define *rom-parameters*
  '(
    *npsv-data-num*
    *npsv-adr-width*
    *npsv-min*
    *npsv-max*
    *npsv-W*
    *npsv-I*
    *npsv-func*
    *npsv-module-name*
    *npsv-rtl-output-dir*
    *npsv-testbench-output-dir*
    *npsv-template-output-dir*))

(define-class <npsv-rom> (<npsv-module>)
  ((min :init-keyword :min)
   (max :init-keyword :max)
   (adr-width :init-keyword :adr-width)
   (func :init-keyword :func)
   (W :init-keyword :W)
   (I :init-keyword :I)))

(define-method print-setting ((inst <npsv-rom>))
  (next-method)
  (format #t "min ~A~%" (ref inst 'min))
  (format #t "max ~A~%" (ref inst 'max))
  (format #t "adress width ~A~%" (ref inst 'adr-width))
  (format #t "func ~A~%" (ref inst 'func))
  (format #t "fixed number W ~A I ~A~%" (ref inst 'W) (ref inst 'I)))

;; TODO separate make-verilog-file
(define make-rom-from-file
 (lambda (fname)
  (clear-global-parameters! *outmnem-parameters*)
  (load-setting-file fname)
  (let ((inst (make-rom-instance
               *npsv-module-name*
               *npsv-min*
               *npsv-max*
               *npsv-adr-width*
               *npsv-W*
               *npsv-I*
               *npsv-func*
               *npsv-rtl-output-dir*
               *npsv-testbench-output-dir*
               *npsv-template-output-dir*)))
    (make-verilog-file inst)
    inst)))

(define make-rom-instance
  (lambda (name min max adr-w W I func rtl-odir tb-odir temp-odir)
    (let ([inst (make <npsv-rom> :name name :type 'npsv-rom :comment "rom module"
                      :min min :max max :adr-width adr-w  :W W :I I :func func
                      :rtl-output-dir rtl-odir :testbench-output-dir tb-odir :template-ouput-dir temp-odir)]
          [fixed (make <npsv-fixed> :W W :I I)])
      (add-port inst (make <npsv-port> :name "start" :dir 'input :comment "no use"))
      (add-port inst (make <npsv-port> :name "set" :dir 'input :comment "no use"))
      (add-port inst (make <npsv-port> :name "vi" :dir 'input))
      (add-port inst (make <npsv-port> :name "fi" :dir 'input))
      (add-port inst (make <npsv-port> :name "vo" :dir 'output :type 'reg))
      (add-port inst (make <npsv-port> :name "fo" :dir 'output :type 'reg))
      (add-port inst (make <npsv-fixed-port> :name "datao" :dir 'output :msb (- W 1) :lsb 0 :fixed-info fixed :type 'reg))
      (add-port inst (make <npsv-adr-port> :name "datai" :dir 'input :msb (- adr-w 1) :lsb 0 :type 'address))
      inst)))

(define-method add-top-ports ((inst <npsv-rom>))
  0)

(define-method write-module-instantiation (fp (m <npsv-rom>) channels)
  (let* ([name (ref m 'name)]
         [input-ch (find (lambda (ch) (eq? (ref ch 'dst) m)) channels)]
         [output-ch (find (lambda (ch) (eq? (ref ch 'src) m)) channels)]
         [input-prefix (ch->wire-prefix input-ch)]
         )
    (format fp "\t~A ~A (\n" name name)
    (write-common-connection fp)

    (when (not input-ch)
      (format #t "Error:no input ~A~%" name))
    (write-inport-assign fp "vi" "vo" input-ch)
    (write-inport-assign fp "fi" "fo" input-ch)
    (write-inport-assign fp "datai" "datao" input-ch)
    
    (when (not output-ch)
      (format #t "Error:no output ~A~%" name))
    (write-outport-assign fp "vo" output-ch)
    (write-outport-assign fp "fo" output-ch)
    (write-outport-assign fp "datao" output-ch :last-flag #t)
    (format fp "\t);\n");
    ))

;;; --------------------------------------------------------------------------------
;;; make rom data
;;; --------------------------------------------------------------------------------
(define make-data
  (lambda (inst)
    (let ([func (ref inst 'func)]
          [min (ref inst 'min)]
          [max (ref inst 'max)]
          [adr-w (ref inst 'adr-width)]
          [W (ref inst 'W)]
          [I (ref inst 'I)])
      (let* ((num (power 2 adr-w))
             (data (make-vector num))
             (unit (/ (- max min) num)))
        (dotimes(x num)
                (vector-set! data x (toFix (func (+ (* x unit) min)) W I)))
                                        ;      (print data)
        (vector->list data)))))


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

(define-method make-verilog-file ((inst <npsv-rom>))
  (set! *data* (make-data inst))
  (write-verilog-file inst (eval rom-rtl-template (interaction-environment))))

(define-method make-verilog-testbench-file ((inst <npsv-rom>))
  (write-verilog-testbench-file inst (eval rom-testbench-template (interaction-environment))))

;;; --------------------------------------------------------------------------------
;;; verilog source
;;; --------------------------------------------------------------------------------
(define rom-rtl-template
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

(define rom-testbench-template
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

(provide "npsv-rom")
