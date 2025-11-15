(ns adtworld.samples.tree
  "木構造を ADT で表現し、走査や変換をすべてデータ駆動で書く例。"
  (:require [app.adtworld :as adt]))

(def Tree
  (adt/data
    [:Tree "節は任意ラベル、葉は任意値。children には Tree のベクタが入る。"
     [:Leaf "値のみを持つ葉" :value]
     [:Node "ラベル付きの節。children は Tree のベクタ" :label :children]]))

(defn leaf [value]
  (adt/value Tree :Leaf {:value value}))

(defn node
  "children をベクタで受けても可読性が低いので、可変長引数にしている。"
  [label & children]
  (adt/value Tree :Node {:label label
                         :children (vec children)}))

(def sample-tree
  (node :root
        (node :left
              (leaf 10)
              (leaf 5))
        (node :right
              (node :right/inner
                    (leaf 7))
              (leaf 2))))

(defn tree->data
  "Tree の値を純粋な Clojure データに落とし込む。デバッグや可視化用。"
  [tree]
  (adt/match tree
    {:Leaf (fn [{:keys [value]}]
             {:leaf value})
     :Node (fn [{:keys [label children]}]
             {:node label
              :children (mapv tree->data children)})}))

(defn preorder-labels
  "節ラベルの DFS（先行順）一覧を得る。葉は含めない。"
  [tree]
  (adt/match tree
    {:Leaf []
     :Node (fn [{:keys [label children]}]
             (into [label]
                   (mapcat preorder-labels)
                   children))}))

(defn leaf-map
  "葉の値に関数 f を適用した新しい Tree を返す。"
  [f tree]
  (adt/match tree
    {:Leaf (fn [{:keys [value]}]
             (leaf (f value)))
     :Node (fn [{:keys [label children]}]
             (apply node label (map #(leaf-map f %) children)))}))

(defn sum-leaves
  "葉に入っている数値を合計する単純な畳み込み。"
  [tree]
  (adt/match tree
    {:Leaf (fn [{:keys [value]}] value)
     :Node (fn [{:keys [children]}]
             (reduce + 0 (map sum-leaves children)))}))

(defn -main [& _]
  (println "サンプル木:" (tree->data sample-tree))
  (println "節ラベル (preorder):" (preorder-labels sample-tree))
  (println "葉の合計:" (sum-leaves sample-tree))
  (println "葉をインクリメントした木:" (tree->data (leaf-map inc sample-tree))))

(comment
  (tree->data sample-tree)
  ;; => {:node :root,
  ;;     :children
  ;;     [{:node :left, :children [{:leaf 10} {:leaf 5}]}
  ;;      {:node :right,
  ;;       :children
  ;;       [{:node :right/inner, :children [{:leaf 7}]}
  ;;        {:leaf 2}]}]}

  (preorder-labels sample-tree)
  ;; => [:root :left :right :right/inner]

  (sum-leaves sample-tree)
  ;; => 24

  (-> sample-tree
      (leaf-map inc)
      tree->data))
