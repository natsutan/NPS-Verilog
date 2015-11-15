module NPS_inmem_tb();
  parameter DATA_WIDTH = 24;
  parameter DATA_NUM = 300;
  parameter ADR_WIDTH = 9;

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
  
  NPS_outmem U0
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
    fp=$fopen("mem_dump.txt");
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
      #(PERIOD) $fwrite(fp, "%X\n", cpu_data);
    end
    cpu_rd = 0;

      
    # (PERIOD * 10)  $finish();
  end



  
endmodule // NPS


