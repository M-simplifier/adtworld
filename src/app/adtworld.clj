(ns app.adtworld
  "Malli や HoneySQL に通じる「ただのデータ」という美学を、そのまま ADT に適用した実験的ヘルパー。
  型定義・コンストラクタ・値はすべてプレーンな Clojure データなので、実行時に好きなように眺めたり加工できる。"
  (:refer-clojure :exclude [match]))

(def ^:private type-key ::type)
(def ^:private ctor-key ::ctor)
(def ^:private fields-key ::fields)
(def ^:private not-found ::not-found)

(defn- as-keyword
  "記法の糖衣から受け取った記号をキーワードに揃える。"
  [x context]
  (cond
    (keyword? x) x
    (symbol? x) (keyword x)
    :else (throw (ex-info (str context " はキーワードかシンボルである必要があります")
                          {::value x}))))

(defn type-name
  "値マップが持っている ADT のキーワードを返す。"
  [value]
  (get value type-key))

(defn ctor
  "値マップが持っているコンストラクタのキーワードを返す。"
  [value]
  (get value ctor-key))

(defn fields
  "値マップに格納されたフィールドのマップを返す。"
  [value]
  (get value fields-key))

(defn- ensure-keyword [k context]
  (when-not (keyword? k)
    (throw (ex-info (str context " はキーワードである必要があります") {::value k})))
  k)

(defn- constructor-form->map [form]
  (cond
    (nil? form) {}
    (map? form) form
    (vector? form) {:fields form}
    (sequential? form) {:fields (vec form)}
    :else (throw (ex-info "コンストラクタの記述はマップかシーケンスでなければなりません"
                          {::value form}))))

(defn- ensure-entry [entry]
  (cond
    (map-entry? entry) [(key entry) (val entry)]
    (and (sequential? entry) (= 2 (count entry)))
    [(first entry) (second entry)]
    :else (throw (ex-info "コンストラクタ指定は [キーワード 形式] のペアで記述してください"
                          {::entry entry}))))

(defn- normalize-constructor [idx [tag form]]
  (ensure-keyword tag "コンストラクタ名")
  (let [{:keys [doc fields] :as descriptor} (constructor-form->map form)
        extras (dissoc descriptor :doc :fields)
        field-order (vec (or fields []))]
    (when-not (every? keyword? field-order)
      (throw (ex-info "フィールド名はすべてキーワードである必要があります"
                      {::ctor tag ::fields field-order})))
    [tag {:tag tag
          :doc doc
          :fields field-order
          :arity (count field-order)
          :index idx
          :extras extras}]))

(defn adt
  "ADT 定義を検証し、扱いやすい形に正規化する。

  入力は次のようなリテラルマップ（多くの場合は def）:

    {:name        :Maybe
     :params      [:a]
     :constructors {:Nothing []
                    :Just [:value]}}

  コンストラクタはフィールドのみを書いたベクタ、または `:fields` を含むマップ（任意メタ情報付き）で指定できる。
  戻り値は同じ情報を持つマップだが、正規化済みのコンストラクタ群と :order、それに ::type マーカーが追加される。"
  [{:keys [name constructors params doc] :as spec}]
  (ensure-keyword name "ADT 名")
  (when-not (or (map? constructors) (sequential? constructors))
    (throw (ex-info "コンストラクタ群はマップか [キーワード 形式] のシーケンスで指定してください"
                    {::constructors constructors})))
  (let [pairs-seq (if (map? constructors) (seq constructors) constructors)
        entries (mapv ensure-entry (or pairs-seq []))
        params* (vec (or params []))]
    (doseq [p params*] (ensure-keyword p "型パラメータ"))
    (assoc spec
           ::type ::adt
           :params params*
           :constructors (into {} (map-indexed normalize-constructor entries))
           :order (mapv first entries)
           :doc doc)))

(defn data
  "Haskell の `data` 宣言に寄せた、より簡潔な定義記法。

  例:

    (data
      [:Maybe \"Maybe 型\"
       {:params [:a]}
       [:Nothing]
       [:Just \"値を包む\" value]])

  ベクタの 2 要素目に文字列を書けば型の doc、それに続けてマップを書けば
  `:params` やその他のメタ情報を設定できる。残りはコンストラクタ定義で、
  `[Constructor ...fields]` の形をとる。コンストラクタ内でも同様に冒頭に
  doc 文字列とメタ情報マップを挿入できる。フィールド名はキーワードかシンボルで記述し、
  すべて自動的にキーワードへ変換される。"
  [form]
  (when-not (sequential? form)
    (throw (ex-info "data 記法はシーケンスで与えてください" {::form form})))
  (let [[name & tail] form
        name* (as-keyword name "型名")
        [doc tail] (if (string? (first tail))
                     [(first tail) (rest tail)]
                     [nil tail])
        [options tail] (if (map? (first tail))
                         [(first tail) (rest tail)]
                         [{} tail])
        {:keys [params] :as options*} options
        params* (when params (mapv #(as-keyword % "型パラメータ") params))
        constructors tail]
    (when (empty? constructors)
      (throw (ex-info "コンストラクタを 1 つ以上定義してください" {::form form})))
    (letfn [(parse-constructor [ctor-form]
              (when-not (sequential? ctor-form)
                (throw (ex-info "コンストラクタの記述はベクタ/シーケンスである必要があります"
                                {::constructor ctor-form})))
              (let [[ctor & body] ctor-form
                    ctor* (as-keyword ctor "コンストラクタ名")
                    [c-doc body] (if (string? (first body))
                                   [(first body) (rest body)]
                                   [nil body])
                    [meta body] (if (map? (first body))
                                  [(first body) (rest body)]
                                  [{} body])
                    fields (mapv #(as-keyword % "フィールド名") body)]
                [ctor* (cond-> {:fields fields}
                         c-doc (assoc :doc c-doc)
                         (seq meta) (merge meta))]))]
      (adt (-> options*
               (dissoc :params :doc :constructors)
               (assoc :name name*
                      :doc (or doc (:doc options*))
                      :params params*
                      :constructors (mapv parse-constructor constructors)))))))

(defn value
  "ADT 定義とコンストラクタキーワードから値を生成する。

  引数をフィールド順に並べて渡すか、フィールド名をキーにした 1 つのマップを渡す書き方のどちらにも対応する。

    (value Maybe :Just 10)
    (value Maybe :Just {:value 10})"
  [adt constructor & args]
  (let [{:keys [name constructors]} adt
        {:keys [fields arity]} (get constructors constructor)]
    (when-not fields
      (throw (ex-info "未定義のコンストラクタです" {::adt name ::ctor constructor})))
    (let [payload (if (and (= 1 (count args)) (map? (first args)))
                    (let [m (first args)
                          missing (seq (filter #(not (contains? m %)) fields))
                          field-set (set fields)
                          extra (seq (remove field-set (keys m)))]
                      (when missing
                        (throw (ex-info "必要なフィールドが指定されていません"
                                        {::ctor constructor ::missing missing})))
                      (when extra
                        (throw (ex-info "定義されていないフィールドが含まれています"
                                        {::ctor constructor ::extra extra})))
                      (reduce (fn [acc k] (assoc acc k (get m k)))
                              {}
                              fields))
                    (do
                      (when-not (= arity (count args))
                        (throw (ex-info "コンストラクタの引数個数が一致しません"
                                        {::ctor constructor
                                         ::expected arity
                                         ::received (count args)})))
                      (zipmap fields args)))]
      {type-key name
       ctor-key constructor
       fields-key payload})))

(defn value-of?
  "与えられたマップが ADT 定義に沿っているなら true。"
  [adt value-map]
  (let [{:keys [name constructors]} adt
        vtype (type-name value-map)
        ctor* (ctor value-map)
        payload (fields value-map)
        {:keys [fields]} (get constructors ctor*)]
    (boolean
     (and (= vtype name)
          fields
          (map? payload)
          (= (set (keys payload)) (set fields))))))

(defn match
  "分岐テーブルを使って値にパターンマッチを行う。

  分岐は `コンストラクタ -> ハンドラ` のマップ（`:else` 任意）か、`[コンストラクタ ハンドラ]` の並びとして渡す。
  ハンドラが関数でない場合はそのまま値として扱い、関数ならマッチしたフィールドマップを引数に呼び出す。"
  [value-map branches]
  (let [tag (ctor value-map)
        payload (fields value-map)
        handler (cond
                  (map? branches)
                  (let [res (get branches tag not-found)]
                    (if (not= res not-found)
                      res
                      (get branches :else not-found)))

                  (sequential? branches)
                  (let [pair (some (fn [[match handler]]
                                     (when (= match tag) [match handler]))
                                   branches)]
                    (if pair
                      (second pair)
                      (let [else-pair (some (fn [[match handler]]
                                              (when (= match :else)
                                                [match handler]))
                                            branches)]
                        (if else-pair
                          (second else-pair)
                          not-found))))

                  :else (throw (ex-info "分岐はマップか [キーワード ハンドラ] の並びで記述してください"
                                        {::branches branches})))]
    (when (= handler not-found)
      (throw (ex-info "マッチが非網羅的です" {::ctor tag ::branches branches})))
    (if (fn? handler)
      (handler payload)
      handler)))
