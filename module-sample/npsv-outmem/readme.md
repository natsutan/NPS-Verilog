# NPS-outmem
�o�̓f�[�^�p������

# �ݒ�p�����[�^

- \*npsv-data-num\* �o�̓f�[�^�̃f�[�^��
- \*npsv-W\* �f�[�^�S�̂̃r�b�g��
- \*npsv-I\* �f�[�^�̐������̃r�b�g��
- \*npsv-module-name\* ���W���[����
- \*npsv-rtl-output-dir\* rtl�o�̓f�B���N�g���i���΃p�X�j
- \*npsv-testbench-output-dir\* �e�X�g�x���`�o�̓f�B���N�g���i���΃p�X�j
- \*npsv-template-output-dir\* �e���v���[�g�o�̓f�B���N�g���i���΃p�X�j


```scheme
(define *npsv-data-num* 300)    ; integer word length
(define *npsv-W* 24)   ; total word length
(define *npsv-I* 8)    ; integer word length

(define *npsv-module-name* "sample")
(define *npsv-rtl-output-dir* "../output/rtl")
(define *npsv-testbench-output-dir* "../output/tb")
(define *npsv-template-output-dir* "../output/template")
```


# ���o��

|���O|�r�b�g��|����|�@�\|
|clk||input|�N���b�N|
|reset_x||input|���Z�b�g�i���_���j|
|start||input|�����J�n�^�C�~���O|
|set||input|�ݒ�l�̔��f�^�C�~���O�i���g�p�j|
|vi||output|valid����|
|fi||output|�����̏I���ʒm|
|vo||output|valid�o��|
|fo||output|�����̏I���ʒm|
|datai|\*npsv-W\*|�f�[�^�o�́i�Œ菬���_���j|
|cpu_adr|\*npsv-data-num\*����v�Z|input|cpu����̓ǂݏo���A�h���X|
|cpu_data|\*npsv-W\|output|cpu����̓ǂݏo���f�[�^|
|cpu_rd||input|cpu����̃��[�h�M��|

# �T���v���o��

[sinrom.v](https://github.com/natsutan/nromgen/blob/master/output/rtl/sinrom.v "sinrom.v")






