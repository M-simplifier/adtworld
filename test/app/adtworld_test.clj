(ns app.adtworld-test
  (:require [clojure.test :refer [deftest is testing]]
            [app.adtworld :as sut]))

(def maybe
  (sut/data
    [:Maybe {:params ['a]}
     [:Nothing]
     [:Just "値を包む" {:db/tag :just} 'value]]))

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
