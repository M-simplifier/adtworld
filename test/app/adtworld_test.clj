(ns app.adtworld-test
  (:require [clojure.test :refer [deftest is testing]]
            [app.adtworld :as sut]))

(sut/defdata Maybe
  {:params [a]}
  [:Nothing]
  [:Just "値を包む" {:db/tag :just} value])

(def maybe Maybe)

(sut/defclass EqMaybe
  "Maybe の等価判定。"
  [:equals left right])

(sut/definstance EqMaybe Maybe
  {:equals (fn [x y]
             (and (= (sut/ctor x) (sut/ctor y))
                  (= (sut/fields x) (sut/fields y))))})

(sut/defclass MonadMaybe
  [:pure value]
  [:bind monadic f])

(sut/definstance MonadMaybe Maybe
  {:pure (fn [x] (sut/value maybe :Just x))
   :bind (fn [mv f]
           (sut/match mv
             {:Nothing mv
              :Just (fn [{:keys [value]}] (f value))}))})

(deftest adt-definition
  (testing "正規化でメタ情報が保たれる"
    (let [just (get-in maybe [:constructors :Just])]
      (is (= :app.adtworld/adt (::sut/type maybe)))
      (is (= [:Nothing :Just] (:order maybe)))
      (is (= [:value] (:fields just)))
      (is (= "値を包む" (:doc just)))
      (is (= {:db/tag :just} (:extras just))))))

(deftest value-construction
  (testing "位置・マップ両方の引数で生成できる"
    (let [from-pos (sut/value maybe :Just 42)
          from-map (sut/value maybe :Just {:value 42})
          nothing (sut/value maybe :Nothing)]
      (is (= from-pos from-map))
      (is (= :Maybe (sut/type-name from-pos)))
      (is (= :Just (sut/ctor from-pos)))
      (is (= {:value 42} (sut/fields from-pos)))
      (is (= :Nothing (sut/ctor nothing)))
      (is (sut/value-of? maybe from-pos))
      (is (not (sut/value-of? maybe {:some :map})))))
  (testing "不正な引数は例外になる"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"未定義のコンストラクタ"
                          (sut/value maybe :Missing 0)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"引数個数が一致しません"
                          (sut/value maybe :Just 1 2)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"必要なフィールド"
                          (sut/value maybe :Just {})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"定義されていないフィールド"
                          (sut/value maybe :Just {:value 1 :other 2})))))

(deftest pattern-matching
  (let [just (sut/value maybe :Just 10)
        nothing (sut/value maybe :Nothing)]
    (testing "マップ形式の分岐"
      (is (= 10
             (sut/match just
                        {:Nothing 0
                         :Just (fn [{:keys [value]}] value)})))
      (is (= :fallback (sut/match nothing {:else :fallback}))))
    (testing "シーケンス形式の分岐"
      (is (= :none
             (sut/match nothing
                        [[:Just (fn [{:keys [value]}] value)]
                         [:Nothing :none]])))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"非網羅"
                            (sut/match just [[:Nothing 0]]))))))

(deftest typeclass-registry
  (let [just (sut/value maybe :Just 1)
        other (sut/value maybe :Just 2)
        eq-fn (sut/operation :EqMaybe maybe :equals)]
    (is (true? (eq-fn just (sut/value maybe :Just 1))))
    (is (false? (eq-fn just other)))
    (is (true? (sut/invoke :EqMaybe just :equals just just)))
    (is (= :EqMaybe (:class (sut/resolve-instance :EqMaybe maybe))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"インスタンスが見つかりません"
                          (sut/resolve-instance :EqMaybe :Unknown)))))

(deftest mdo-notation
  (let [result (sut/mdo :MonadMaybe Maybe
                        [x (sut/value maybe :Just 2)
                         y (sut/value maybe :Just 3)]
                        [:pair x y])
        failure (sut/mdo :MonadMaybe Maybe
                         [x (sut/value maybe :Nothing)
                          y (sut/value maybe :Just 1)]
                         [:pair x y])]
    (is (= {:value [:pair 2 3]} (sut/fields result)))
    (is (= :Just (sut/ctor result)))
    (is (= :Nothing (sut/ctor failure)))))
