# adtworld

Malli や HoneySQL が示す「それはただのデータだ」という美学を、そのまま代数的データ型に持ち込んだ実験的ライブラリです。型定義はリテラルなマップ、コンストラクタはキーワード、生成される値は名前空間付きキーを数個持つだけのマップ。マクロもレコード生成もなく、どの層も自由に覗き込み／編集できます。

## ステータス

あくまで実験段階です。実装は可読性と表現力を優先しており、API もごく小さくまとめています。データ構造が目で追えることを第一にしています。

## 使い方

名前空間を require し、ADT をリテラルデータで表現します。

```clojure
(ns demo
  (:require [app.adtworld :as adt]))

(def Maybe
  (adt/adt {:name :Maybe
            :params [:a]
            :constructors
            {:Nothing []
             :Just {:fields [:value]
                    :doc "値を包む"}}}))

(def maybe-ten (adt/value Maybe :Just 10))

(adt/match maybe-ten
  {:Nothing 0
   :Just (fn [{:keys [value]}] value)})
;; => 10
```

### ADT の定義

`adt/adt` は与えられた定義を検証して正規化します。コンストラクタは `[:field …]` というベクタか、`{:fields [...]}` を含むマップ（任意のメタ情報を付加可能）として書けます。戻り値は `::adt` マーカーと正規化済みのコンストラクタ情報を持つマップです。

```clojure
(def Tree
  (adt/adt {:name :Tree
            :constructors
            [[:Leaf {:fields [:value]}]
             [:Branch {:fields [:left :right]
                       :doc "2 分木の節"}]]}))
```

### 値とのやりとり

- `adt/value` は指定コンストラクタの値を生成します。フィールド順に引数を渡すか、フィールド名をキーにした 1 つのマップを渡せます。
- `adt/value-of?` は任意のマップが ADT の値かどうか確認します。
- `adt/type-name` `adt/ctor` `adt/fields` で値マップの各断片を取り出せます。
- `adt/match` はデータ駆動のパターンマッチです。`{:Ctor handler}` 形式のマップ（` :else` ハンドラ任意）か、`[ctor handler]` のシーケンスを渡します。ハンドラが関数でない場合は定数として扱われ、関数ならフィールドマップが渡されます。

## サンプルコード集

`examples/adtworld/samples` に複数の題材を用意しています。REPL から読み込む場合は `:samples` エイリアスを使ってください。

```
clojure -M:samples -m adtworld.samples.tree
```

- `adtworld.samples.tree` … 汎用木構造の操作（走査・変換・畳み込み）
- `adtworld.samples.workflow` … ヘルプデスクのチケット状態管理と遷移
- `adtworld.samples.http` … Web API レスポンスを ADT で表し Ring 形式へ変換

## テストの実行

Cognitect の test-runner を利用しています。

```
clojure -M:test -m cognitect.test-runner
```

## ライセンス

Copyright © 2025 Masaya

ライセンスは `deps-new` デフォルトの Eclipse Public License 1.0 です。必要に応じて好きなライセンスに差し替えてください。
