module NPS_rom_tb # (parameter DATA_WIDTH = 24 ,  ADR_WIDTH = 9) ();
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
  always # (PERIOD/2) clk = ~clk;
  initial begin clk = 1;end

  NPS_rom U0
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
    fp=$fopen("NPS-rom.txt");
    #1 reset_x = 1; datai = 0; fi = 0; set = 0; vi = 0;
    # (PERIOD * 3)  reset_x = 0;
    # (PERIOD * 5)  reset_x = 1;

    
    for(i=0;i<(2^ADR_WIDTH);i=i+1)begin
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
      $fwrite(fp, "%X\n", datao);
    end
  end
  
  
endmodule
