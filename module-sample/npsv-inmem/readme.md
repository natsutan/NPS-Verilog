# NPS-inmem
���̓f�[�^�p������

# �ݒ�p�����[�^

- \*npsv-data-num\* ���̓f�[�^�̃f�[�^��
- \*npsv-W\* �f�[�^�S�̂̃r�b�g��
- \*npsv-I\* �f�[�^�̐������̃r�b�g��
- \*npsv-delta-T\* ����J�n�̒x���N���b�N���B2�ȏ��ݒ肷�邱�ƁB
- \*npsv-module-name\* ���W���[����
- \*npsv-init-file\* �����l�t�@�C�����B
- \*npsv-rtl-output-dir\* rtl�o�̓f�B���N�g���i���΃p�X�j
- \*npsv-testbench-output-dir\* �e�X�g�x���`�o�̓f�B���N�g���i���΃p�X�j
- \*npsv-template-output-dir\* �e���v���[�g�o�̓f�B���N�g���i���΃p�X�j

�ݒ��
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
```


# ���o��

|���O|�r�b�g��|����|�@�\|
|clk||input|�N���b�N|
|reset_x||input|���Z�b�g�i���_���j|
|start||input|�����J�n�^�C�~���O|
|set||input|�ݒ�l�̔��f�^�C�~���O�i���g�p�j|
|vo||output|valid�o��|
|fo||output|�����̏I���ʒm|
|datao|\*npsv-W\*|�f�[�^�o�́i�Œ菬���_���j|
|cpu_adr|\*npsv-data-num\*����v�Z|input|cpu����̏������݃A�h���X|
|cpu_data|\*npsv-W\|input|cpu����̏������݃f�[�^|
|cpu_wr||input|cpu����̃��C�g�M��|


# �T���v���o��

[inmem.v](https://github.com/natsutan/nromgen/blob/master/output/rtl/sinrom.v "inmem.v")







