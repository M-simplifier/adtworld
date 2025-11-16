(ns hooks.mdo
  (:require [clj-kondo.hooks-api :as api]))

(defn- wrap-expr [expr-node]
  (api/list-node [(api/token-node 'do)
                  expr-node
                  (api/token-node nil)]))

(defn mdo
  [{:keys [node]}]
  (let [[_ _class _target bindings & body] (:children node)
        binding-children (if bindings (:children bindings) [])
        rewritten-bindings (->> binding-children
                                (partition 2)
                                (mapcat (fn [[sym expr]]
                                          [sym (wrap-expr expr)]))
                                (api/vector-node))
        body-node (if (seq body)
                    (api/list-node (cons (api/token-node 'do) body))
                    (api/token-node nil))]
    {:node (api/list-node [(api/token-node 'let)
                           rewritten-bindings
                           body-node])}))
