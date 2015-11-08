
module mem();


  
  always @ (posedge clk or negedge reset_x)
    if(reset_x == 0)begin
      en <= 0;
    end else begin
      if(delta_cnt == *Delta_T*)begin
	en <= 1;
      end
    end

 always @ (posedge clk or negedge reset_x)
    if(reset_x == 0)begin
      cnt_en <= 0;
    end else begin
      if(start)begin
	cnt_en <= 1;
      end else if(delta_cnt == *Delta_T*)begin
	cnt_en <= 0;
      end
    end
 
 always @ (posedge clk or negedge reset_x)
    if(reset_x == 0)begin
      adr_cnt <= 0;
    end else begin
      if(adr_cnt == *datanum*)begin
	adr_cnt <= adr_cnt;
      end else if(en)begin
	adr_cnt <= adr_cnt + 1;
      end
    end
 
 always @ (posedge clk or negedge reset_x)
    if(reset_x == 0)begin
      vo <= 0;
    end else begin
      vo <= en;
    end

 always @ (posedge clk or negedge reset_x)
    if(reset_x == 0)begin
      fo <= 0;
    end else begin
      if(adr_cnt == *datanum*)begin
	fo <= 1;
      end else begin
	fo <= 0;
      end
    end
 
  
   always @ (posedge clk or negedge reset_x)
    if(reset_x == 0)begin
      adr_cnt <= 0;
    end else begin
      if(adr_cnt == *datanum*)begin
	adr_cnt <= adr_cnt;
      end else if(en)begin
	adr_cnt <= adr_cnt + 1;
      end
    end
 
 always @ (posedge clk or negedge reset_x)
    if(reset_x == 0)begin
      vo <= 0;
    end else begin
      vo <= en;
    end

 always @ (posedge clk or negedge reset_x)
    if(reset_x == 0)begin
      fo <= 0;
    end else begin
      if(adr_cnt == *datanum*)begin
	fo <= 1;
      end else begin
	fo <= 0;
      end
    end

  

  
endmodule // mem
