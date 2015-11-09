
module NPS_inmem # (parameter DATA_WIDTH = 16, DATA_NUM = 30, DELTA_T = 20, ADR_WIDTH = 5, DELTA_WIDTH = 5)
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
//      delta_cnt <= DELTA_WIDTH'd0;
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
