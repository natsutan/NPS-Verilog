# NPS-inmem
���̓f�[�^�p������

# �ݒ�p�����[�^
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


# ���o��

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



# �T���v���o��

[sinrom.v](https://github.com/natsutan/nromgen/blob/master/output/rtl/sinrom.v "sinrom.v")


# sim����
## xtrain


## xtrain_1




