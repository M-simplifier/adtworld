(ns hooks.defdata
  (:require [clj-kondo.hooks-api :as api]))

(defn defdata
  [{:keys [node]}]
  (let [[_ name & spec] (:children node)
        type-kw (keyword (api/sexpr name))
        vector-node (api/vector-node (cons (api/token-node type-kw) spec))
        quoted (api/list-node [(api/token-node 'quote) vector-node])
        call (api/list-node [(api/token-node 'app.adtworld/data) quoted])]
    {:node (api/list-node [(api/token-node 'def)
                           name
                           call])}))
