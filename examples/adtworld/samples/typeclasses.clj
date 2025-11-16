(ns adtworld.samples.typeclasses
  "型クラス辞書を定義し、Maybe を Functor/Monad として扱う例。"
  (:require [app.adtworld :as adt]))

(adt/defdata Maybe
  {:params [a]}
  [:Nothing]
  [:Just value])

(defn just [x] (adt/value Maybe :Just x))
(def nothing (adt/value Maybe :Nothing))

(adt/defclass Eq
  "等価性。"
  [:equals left right])

(adt/defclass Functor
  "fmap を 1 つ持つだけのシンプルなノート。"
  [:map f structure])

(adt/defclass Monad
  "return/pure と bind を定義する。"
  [:pure value]
  [:bind monadic f])

(adt/definstance Eq Maybe
  {:equals (fn [x y]
             (and (= (adt/ctor x) (adt/ctor y))
                  (= (adt/fields x) (adt/fields y))))})

(adt/definstance Functor Maybe
  {:map (fn [f mv]
          (adt/match mv
            {:Nothing mv
             :Just (fn [{:keys [value]}]
                     (just (f value)))}))})

(adt/definstance Monad Maybe
  {:pure just
   :bind (fn [mv f]
           (adt/match mv
             {:Nothing mv
              :Just (fn [{:keys [value]}] (f value))}))})

(defn fmap [f mv]
  (adt/invoke :Functor Maybe :map f mv))

(defn pure [x]
  (adt/invoke :Monad Maybe :pure x))

(defn bind [mv f]
  (adt/invoke :Monad Maybe :bind mv f))

(defn safe-div [num den]
  (if (zero? den)
    nothing
    (just (/ num den))))

(defn -main [& _]
  (println "Eq? Just 1 vs Just 1:" (adt/invoke :Eq Maybe :equals (just 1) (just 1)))
  (println "Eq? Just 1 vs Nothing:" (adt/invoke :Eq Maybe :equals (just 1) nothing))
  (println "fmap inc (Just 2):" (fmap inc (just 2)))
  (println "safe-div chain:"
           (bind (safe-div 10 2)
                 (fn [x] (bind (safe-div x 0) ; => Nothing
                               (fn [_] (pure :ok))))))
  (println "safe-div chain (success):"
           (bind (safe-div 10 2)
                 (fn [x] (bind (safe-div x 5)
                               (fn [y] (pure [:result x y]))))))
  (println "mdo success:"
           (adt/mdo :Monad Maybe
             [x (safe-div 10 2)
              y (safe-div x 5)]
             [:result x y]))
  (println "mdo short-circuit:"
           (adt/mdo :Monad Maybe
             [x (safe-div 10 0)
              y (safe-div x 5)]
             [:result x y])))
