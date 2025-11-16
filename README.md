# adtworld

Malli や HoneySQL が示す「それはただのデータだ」という美学を、そのまま代数的データ型に持ち込んだ実験的ライブラリです。型定義はリテラルなマップ、コンストラクタはキーワード、生成される値は名前空間付きキーを数個持つだけのマップ。マクロもレコード生成もなく、どの層も自由に覗き込み／編集できます。

## ステータス

あくまで実験段階です。実装は可読性と表現力を優先しており、API もごく小さくまとめています。データ構造が目で追えることを第一にしています。

## 使い方

名前空間を require し、ADT をリテラルデータで表現します。

```clojure
(ns demo
  (:require [app.adtworld :as adt]))

(adt/defdata Maybe
  {:params [a]}
  [:Nothing]
  [:Just "値を包む" value])

(def maybe-ten (adt/value Maybe :Just 10))

(adt/match maybe-ten
  {:Nothing 0
   :Just (fn [{:keys [value]}] value)})
;; => 10
```

### ADT の定義

`adt/defdata` は型名を重ねて書くことなく `data` 記法で ADT を定義する糖衣です。doc 文字列・オプションマップ・コンストラクタ列は `adt/data` と同じ並びで、フィールド名は素のシンボルでも自動的に扱えます。

```clojure
(adt/defdata Tree
  "2 分木"
  [:Leaf value]
  [:Branch {:db/tag :branch} left right])
```

`adt/data` 関数を直接呼び出したい場合は、先頭に型名を含むシーケンスを渡してください（`[(keyword type) ...]`）。さらに細かい制御が必要なら、従来どおり `adt/adt` にマップを渡すことも可能です。

### 値とのやりとり

- `adt/value` は指定コンストラクタの値を生成します。フィールド順に引数を渡すか、フィールド名をキーにした 1 つのマップを渡せます。
- `adt/value-of?` は任意のマップが ADT の値かどうか確認します。
- `adt/type-name` `adt/ctor` `adt/fields` で値マップの各断片を取り出せます。
- `adt/match` はデータ駆動のパターンマッチです。`{:Ctor handler}` 形式のマップ（` :else` ハンドラ任意）か、`[ctor handler]` のシーケンスを渡します。ハンドラが関数でない場合は定数として扱われ、関数ならフィールドマップが渡されます。

### 型クラス風の辞書

型とふるまいを分離して再利用したい場合は、Haskell の type class に似た簡易辞書を定義できます。

```clojure
(adt/defclass Monad
  "最小限のモナド定義。"
  [:pure value]
  [:bind monadic f])

(adt/definstance Monad Maybe
  {:pure (fn [x] (adt/value Maybe :Just x))
   :bind (fn [mv f]
           (adt/match mv
             {:Nothing mv
              :Just (fn [{:keys [value]}] (f value))}))})

(adt/invoke :Monad Maybe :bind
            (adt/value Maybe :Just 10)
            (fn [x] (adt/value Maybe :Just (inc x))))
```

`defclass`/`class` で型クラスの定義をデータ化し、`definstance`/`register-instance!` で特定の型に対する辞書（操作名 → 実装関数）を登録します。`invoke` と `operation` は、登録済みインスタンスから操作関数を取り出して実行するヘルパーです。

#### do-notation

モナド辞書が用意できれば、`adt/mdo` マクロで do 記法ライクな糖衣が使えます。束縛ベクタは `let` と同じ並びで記述し、各式はモナドの `:bind` を通って連鎖し、最後の式が `:pure` で包まれます。

```clojure
(adt/mdo :Monad Maybe
  [first (safe-div 10 2)
   second (safe-div first 5)]
  [:result first second])
;; => {:ctor :Just, ...}
```

## サンプルコード集

`examples/adtworld/samples` に複数の題材を用意しています。REPL から読み込む場合は `:samples` エイリアスを使ってください。

```
clojure -M:samples -m adtworld.samples.tree
```

- `adtworld.samples.tree` … 汎用木構造の操作（走査・変換・畳み込み）
- `adtworld.samples.workflow` … ヘルプデスクのチケット状態管理と遷移
- `adtworld.samples.http` … Web API レスポンスを ADT で表し Ring 形式へ変換
- `adtworld.samples.typeclasses` … 型クラス辞書を使って Maybe を Functor/Monad 化

## テストの実行

Cognitect の test-runner を利用しています。

```
clojure -M:test -m cognitect.test-runner
```

## ライセンス

MIT License © 2025 Masaya
