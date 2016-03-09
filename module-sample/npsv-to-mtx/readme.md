# NPSーto-mtx
行列データ作成モジュール。スカラーデータを行列に変換する。

# 設定パラメータ
(define *npsv-row* 2)   ; total word length
(define *npsv-column* 3)    ; integer word length

(define *npsv-W* 24)   ; total word length
(define *npsv-I* 8)    ; integer word length

- \*npsv-row\* 行列の行数
- \*npsv-column\* 行列の列数
- \*npsv-W\* データ全体のビット幅
- \*npsv-I\* データの整数部のビット幅
- \*npsv-module-name\* モジュール名
- \*npsv-rtl-output-dir\* rtl出力ディレクトリ（相対パス）
- \*npsv-testbench-output-dir\* テストベンチ出力ディレクトリ（相対パス）
- \*npsv-template-output-dir\* テンプレート出力ディレクトリ（相対パス）


```scheme
(define *npsv-row* 2)   ; total word length
(define *npsv-column* 3)    ; integer word length

(define *npsv-W* 24)   ; total word length
(define *npsv-I* 8)    ; integer word length

(define *npsv-module-name* "to_2_3")
(define *npsv-rtl-output-dir* "../output/rtl")
(define *npsv-testbench-output-dir* "../output/tb")
(define *npsv-template-output-dir* "../output/template")
```


# 入出力
| 名前 | ビット幅 | 方向 | 機能 |
| ------------- | -------------| ----- |---- | 
|clk||input|クロック|
|reset_x||input|リセット（負論理）|
|start||input|処理開始タイミング|
|set||input|設定値の反映タイミング（未使用）|

|vi_n_m||output|valid入力(n行m列目)|
|fi_n_m||output|処理の終了通知(n行m列目)|
|vo||output|valid出力|
|fo||output|処理の終了通知|
|datai_n_m|\*npsv-W\*|データ出力（固定小数点数） (n行m列目)|
|datao_n_m|\*npsv-W\*|データ出力（固定小数点数） (n行m列目)|

# サンプル出力
