//-------------------------------------------------
// sample
// This file was auto-generated by npsv 2015-11-11T14:39:13
//-------------------------------------------------

module sample_tb();
  parameter DATA_WIDTH = 16;
  parameter DATA_NUM = 32;
  parameter DELTA_T = 2;
  parameter ADR_WIDTH = 5.0;

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
  
  sample U0
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

    `include "sample_init.v"

    # (PERIOD * 3) set = 1;
    # (PERIOD) set = 0;
    # (PERIOD * 3) start = 1;
    # (PERIOD) start = 0;
    
    @(posedge fo)
    
    # (PERIOD * 10)  $finish();
  end

endmodule // NPS
  