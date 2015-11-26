module NPS_inmem_tb();
  parameter DATA_WIDTH = 16;
  parameter DATA_NUM = 30;
  parameter DELTA_T = 20;
  parameter ADR_WIDTH = 5;

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

  integer 	       i;
  
  parameter PERIOD = 10.0;
  always # (PERIOD/2) clk = !clk;
  initial begin 
    clk = 1;
  end
  
  NPS_inmem U0
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


  initial begin

    #1 reset_x = 1; cpu_adr = 0; cpu_wr = 0; set = 0; start = 0; cpu_data = 0;
    # (PERIOD * 3)  reset_x = 0;
    # (PERIOD * 5)  reset_x = 1;


  
    # (PERIOD * 3) set = 1;
    # (PERIOD) set = 0;
    # (PERIOD * 3) start = 1;
    # (PERIOD) start = 0;
    
    @(posedge fo)
    
    # (PERIOD * 10)  $finish();
  end



  
endmodule // NPS


