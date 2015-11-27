# NPS-outmem
出力データ用メモリ

# 設定パラメータ

- \*npsv-data-num\* 出力データのデータ数
- \*npsv-W\* データ全体のビット幅
- \*npsv-I\* データの整数部のビット幅
- \*npsv-module-name\* モジュール名
- \*npsv-rtl-output-dir\* rtl出力ディレクトリ（相対パス）
- \*npsv-testbench-output-dir\* テストベンチ出力ディレクトリ（相対パス）
- \*npsv-template-output-dir\* テンプレート出力ディレクトリ（相対パス）


```scheme
(define *npsv-data-num* 300)    ; integer word length
(define *npsv-W* 24)   ; total word length
(define *npsv-I* 8)    ; integer word length

(define *npsv-module-name* "sample")
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
|vi||output|valid入力|
|fi||output|処理の終了通知|
|vo||output|valid出力|
|fo||output|処理の終了通知|
|datai|\*npsv-W\*|データ出力（固定小数点数）|
|cpu_adr|\*npsv-data-num\*から計算|input|cpuからの読み出しアドレス|
|cpu_data|\*npsv-W\|output|cpuからの読み出しデータ|
|cpu_rd||input|cpuからのリード信号|

# サンプル出力

[sinrom.v](https://github.com/natsutan/nromgen/blob/master/output/rtl/sinrom.v "sinrom.v")






