# NPS-const
�萔����

# �ݒ�p�����[�^

- \*npsv-W\* �f�[�^�S�̂̃r�b�g��
- \*npsv-I\* �f�[�^�̐������̃r�b�g��
- \*npsv-value\* �萔�l


�ݒ��
```scheme
; sample file
(define *npsv-W* 4)   ; total word length
(define *npsv-I* 2)    ; integer word length
(define *npsv-value* 3)    ; const value

(define *npsv-module-name* "sample")
(define *npsv-rtl-output-dir* "../output/rtl")
(define *npsv-testbench-output-dir* "../output/tb")
(define *npsv-template-output-dir* "../output/template")

```


# ���o��

| ���O | �r�b�g�� | ���� | �@�\ |
| ------------- | -------------| ----- |---- | 
|clk| |input|�N���b�N|
|reset_x||input|���Z�b�g�i���_���j|
|start||input|�����J�n�^�C�~���O(���g�p)|
|set||input|�ݒ�l�̔��f�^�C�~���O�i���g�p�j|
|vo||output|valid�o��(1�Œ�o��)|
|fo||output|�����̏I���ʒm(0�Œ�o��)|
|datao|\*npsv-W\*|�f�[�^�o�́i�Œ菬���_���j|

 input 			     clk,
 input 			     reset_x,
 input 			     start,
 input 			     set,
 output 		     vo,
 output 		     fo,
 output [DATA_WIDTH-1:0] datao




