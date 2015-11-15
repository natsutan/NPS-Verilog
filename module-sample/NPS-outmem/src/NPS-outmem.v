
module NPS_outmem # (parameter DATA_WIDTH = 24, DATA_NUM = 300, ADR_WIDTH = 8)
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
