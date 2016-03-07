# NPS-const
定数入力

# 設定パラメータ

- \*npsv-W\* データ全体のビット幅
- \*npsv-I\* データの整数部のビット幅
- \*npsv-value\* 定数値


設定例
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


# 入出力

| 名前 | ビット幅 | 方向 | 機能 |
| ------------- | -------------| ----- |---- | 
|clk| |input|クロック|
|reset_x||input|リセット（負論理）|
|start||input|処理開始タイミング(未使用)|
|set||input|設定値の反映タイミング（未使用）|
|vo||output|valid出力(1固定出力)|
|fo||output|処理の終了通知(0固定出力)|
|datao|\*npsv-W\*|output|データ出力（固定小数点数）|



