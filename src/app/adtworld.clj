(ns app.adtworld
  "Malli や HoneySQL に通じる「ただのデータ」という美学を、そのまま ADT に適用した実験的ヘルパー。
  型定義・コンストラクタ・値はすべてプレーンな Clojure データなので、実行時に好きなように眺めたり加工できる。"
  (:refer-clojure :exclude [match class]))

(def ^:private type-key ::type)
(def ^:private ctor-key ::ctor)
(def ^:private fields-key ::fields)
(def ^:private not-found ::not-found)
(def ^:private class-registry (atom {}))
(def ^:private instance-registry (atom {}))

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

(defn- operation-form->entry [idx form]
  (when-not (sequential? form)
    (throw (ex-info "型クラスの操作はベクタ/シーケンスで指定してください"
                    {::operation form})))
  (let [[name & tail] form
        name* (as-keyword name "操作名")
        [doc tail] (if (string? (first tail))
                     [(first tail) (rest tail)]
                     [nil tail])
        [meta tail] (if (map? (first tail))
                      [(first tail) (rest tail)]
                      [nil tail])
        params (mapv #(as-keyword % "操作パラメータ") tail)]
    [name* {:tag name*
            :doc doc
            :params params
            :index idx
            :meta (or meta {})}]))

(defn class
  "型クラス定義を正規化する。

  例:

    (class
      [:Eq \"等価性\"
       [:equals left right]])

  最初の項目が型クラス名、続いて doc、さらにオプションのマップで `:params` などを指定し、
  残りを操作のリストとして `[op doc? {:meta ...}? & params]` の形で列挙する。"
  [form]
  (when-not (sequential? form)
    (throw (ex-info "class 記法はシーケンスで与えてください" {::form form})))
  (let [[name & tail] form
        name* (as-keyword name "型クラス名")
        [doc tail] (if (string? (first tail))
                     [(first tail) (rest tail)]
                     [nil tail])
        [options tail] (if (map? (first tail))
                         [(first tail) (rest tail)]
                         [{} tail])
        params* (mapv #(as-keyword % "型クラスの型パラメータ")
                      (or (:params options) []))
        ops (map-indexed operation-form->entry tail)]
    (when (empty? ops)
      (throw (ex-info "型クラスは少なくとも 1 つの操作を定義してください"
                      {::class name*})))
    (assoc options
           ::type ::type-class
          :name name*
           :doc (or doc (:doc options))
           :params params*
           :operations (into {} ops)
           :order (mapv first ops))))

(defn register-class!
  "型クラス定義をレジストリへ登録し、その定義を返す。"
  [class-map]
  (let [name (:name class-map)]
    (ensure-keyword name "型クラス名")
    (swap! class-registry assoc name class-map)
    class-map))

(defn class-registry-snapshot
  "内部レジストリの状態を返す（主にデバッグ用）。"
  []
  @class-registry)

(defn instance-registry-snapshot
  []
  @instance-registry)

(defn- normalize-class-key [class]
  (cond
    (keyword? class) class
    (symbol? class) (keyword class)
    (map? class) (as-keyword (:name class) "型クラス名")
    :else (throw (ex-info "型クラスはキーワード/シンボル/定義で指定してください"
                          {::class class}))))

(defn- normalize-type-key [target]
  (cond
    (keyword? target) target
    (symbol? target) (keyword target)
    (map? target) (cond
                    (:name target) (:name target)
                    (contains? target type-key) (type-name target)
                    :else (throw (ex-info "型定義/値から型名を解釈できません"
                                          {::value target})))
    :else (throw (ex-info "型はキーワード/シンボル/値/定義で指定してください"
                          {::value target}))))

(defn register-instance!
  "型クラスと型に対するインスタンス辞書を登録する。"
  [{:keys [class type doc impl] :as spec}]
  (when-not (map? impl)
    (throw (ex-info "インスタンス定義はマップで記述してください"
                    {::spec spec})))
  (let [class-key (normalize-class-key class)
        type-key (normalize-type-key type)
        entry {:class class-key
               :type type-key
               :doc doc
               :impl impl}]
    (swap! instance-registry assoc [class-key type-key] entry)
    entry))

(defn resolve-instance
  "登録済みの型クラスインスタンスを取得する。"
  [class target]
  (let [class-key (normalize-class-key class)
        type-key (normalize-type-key target)]
    (or (get @instance-registry [class-key type-key])
        (throw (ex-info "インスタンスが見つかりません"
                        {::class class-key ::type type-key})))))

(defn operation
  "型クラスインスタンスから操作関数を取り出す。"
  [class target op]
  (let [op* (as-keyword op "操作名")
        instance (resolve-instance class target)
        f (get-in instance [:impl op*])]
    (or f
        (throw (ex-info "指定した操作はこのインスタンスで定義されていません"
                        {::class (:class instance)
                         ::type (:type instance)
                         ::operation op*})))))

(defn invoke
  "型クラスの操作を呼び出すショートカット。"
  [class target op & args]
  (apply (operation class target op) args))

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

(defmacro defdata
  "型定義を `data` 記法で書き、同名の var に束縛する糖衣。

  例:

    (defdata Maybe
      {:params [a]}
      [:Nothing]
      [:Just value])

  Doc 文字列やオプションマップの並びは `data` と同じで、フィールド名のシンボルも自動で引用される。"
  [name & spec]
  (let [adt-form (vec (cons (keyword name) spec))]
    `(def ~name (data '~adt-form))))

(defmacro defclass
  "型クラス定義を `class` 記法で束縛し、同時にレジストリへ登録する。"
  [name & spec]
  (let [form (vec (cons name spec))]
    `(do
       (def ~name (class '~form))
       (register-class! ~name))))

(defmacro definstance
  "型クラスと型に対するインスタンス辞書を登録する。"
  [class type & body]
  (let [[doc body] (if (string? (first body))
                     [(first body) (rest body)]
                     [nil body])
        impl (first body)]
    (when-not (map? impl)
      (throw (IllegalArgumentException.
              "definstance には操作名をキーに持つマップを渡してください")))
    `(register-instance! {:class '~class
                          :type '~type
                          :doc ~doc
                          :impl ~impl})))

(defmacro mdo
  "モナド辞書を使った do 記法。

  (mdo :Monad Maybe
    [x (safe-div 10 2)
     y (safe-div x 5)]
    [:result (+ x y)])

  という形で書くと、各式が :bind を通じて連鎖し、最後の式が :pure で包まれる。"
  [class target bindings & body]
  (when-not (vector? bindings)
    (throw (IllegalArgumentException. "mdo の束縛はベクタで指定してください")))
  (when (odd? (count bindings))
    (throw (IllegalArgumentException. "mdo の束縛ベクタは偶数個の要素が必要です")))
  (let [pairs (partition 2 bindings)
        result `(do ~@body)]
    (reduce (fn [acc [sym expr]]
              `(invoke ~class ~target :bind ~expr (fn [~sym] ~acc)))
            `(invoke ~class ~target :pure ~result)
            (reverse pairs))))
