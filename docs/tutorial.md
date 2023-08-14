# Flow Tutorial

## 実行手順

```bash
$ cabel repl
ghci> :l app/Main.hs
ghci> main
> // ここでプログラムを記述する
```

## 出力

出力には`trace`を使います。

```flowlang
> 1 -> trace
> run 
1
```

繰り返し出力するには`1*`を利用します。

```flowlang
> 1* -> trace
> run
1;
1;
1;
1;
1.
teminated
```

`1* ->`は`1 -> repeat ->`の糖衣構文です。

## 計算

足し算は`+`に二つの`in-flow`を入力します。

```flowlang
> (1 ->) (2 ->) + -> trace
> run
3
```

掛け算も同様です。

3つ以上の`in-flow`を入力することもできます。

```flowlang
> (1 ->) (2 ->) (3 ->) + -> trace
> run
5
```

## 関数

関数は以下のような文法を持ちます。

```
...->:input1 ...->:input2 ... function output1:->... output2:->... ... 
```

左側に入力flowを、右側に出力flowを持ちます。引数は省略できます。

`trace`は入力flowと出力flowを1つずつ持つ関数です。`+`や`*`は入力flowが可変数で出力flowが1つの関数と考えることができます。

## 条件分岐

`if`を使うことで分岐を実現できます。booleanの値は0か1を利用します。

```flowlang
> 1 -> if (then:2 -> trace) (else:3 -> trace)
> run
2
```

`then:`に続けてflowを入力することで1の場合の処理を、`else:`に続けてflowを入力することで0の場合の処理を指定します。

## Merge

流れを合流させるために`merge`を利用できます。

```
> (1 ->) (2 ->) (3 ->) merge -> trace
> run
1;
2;
3;
```

評価は外側から実行されます。

## Copy

流れを分割し複数の流れに分けるために、`copy`を利用できます。

```flowlang
> 1 -> copy (-> trace) (-> trace) (-> trace)
> run
1;
1;
1;
```

以下は`1`の`flow`を分流し足し合わせる例です。

```flowlang 
> 1 -> copy (-> id ->) (-> id ->) (-> id ->) + -> trace
> run
3;
```

## Control

`control`を利用すると、トリガーとなるものが流れてきた場合に他のflowを後に流すことができます。

```
(trigger flow) ->:en (flow ->) control -> ...
```

例えば、以下は何かが流れてくると`2`を流す例です。

```
1 ->:en (2 ->) control -> trace
```

## 名前付きflow

`@myflow`のように、名前をつけてflowを作成できます