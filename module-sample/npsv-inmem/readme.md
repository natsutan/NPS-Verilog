# NPS-inmem
入力データ用メモリ

# 設定パラメータ

- \*npsv-data-num\* 入力データのデータ数
- \*npsv-W\* データ全体のビット幅
- \*npsv-I\* データの整数部のビット幅
- \*npsv-delta-T\* 動作開始の遅延クロック数。2以上を設定すること。
- \*npsv-module-name\* モジュール名
- \*npsv-init-file\* 初期値ファイル名。
- \*npsv-rtl-output-dir\* rtl出力ディレクトリ（相対パス）
- \*npsv-testbench-output-dir\* テストベンチ出力ディレクトリ（相対パス）
- \*npsv-template-output-dir\* テンプレート出力ディレクトリ（相対パス）

設定例
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


# 入出力

|名前|ビット幅|方向|機能|
|clk||input|クロック|
|reset_x||input|リセット（負論理）|
|start||input|処理開始タイミング|
|set||input|設定値の反映タイミング（未使用）|
|vo||output|valid出力|
|fo||output|処理の終了通知|
|datao|\*npsv-W\*|データ出力（固定小数点数）|
|cpu_adr|\*npsv-data-num\*から計算|input|cpuからの書き込みアドレス|
|cpu_data|\*npsv-W\|input|cpuからの書き込みデータ|
|cpu_wr||input|cpuからのライト信号|


# サンプル出力

[inmem.v](https://github.com/natsutan/nromgen/blob/master/output/rtl/sinrom.v "inmem.v")







