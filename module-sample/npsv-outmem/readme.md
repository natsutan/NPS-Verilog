# NPS-inmem
入力データ用メモリ

# 設定パラメータ
```scheme
(define *npsv-data-num* 32)    ; data number
(define *npsv-W* 16)   ; total word length
(define *npsv-I* 8)    ; integer word length
(define *npsv-delta-T* 2)    ; integer word length

(define *npsv-module-name* "sample")
(define *npsv-init-file* "../sample/sample.dat")
(define *npsv-rtl-output-dir* "../output/rtl")
(define *npsv-testbench-output-dir* "../output/tb")
(define *npsv-template-output-dir* "../output/template")
```*npsv-data-num*

- \*npsv-data-num\*


# 入出力

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



# サンプル出力

[sinrom.v](https://github.com/natsutan/nromgen/blob/master/output/rtl/sinrom.v "sinrom.v")


# sim結果
## xtrain


## xtrain_1




