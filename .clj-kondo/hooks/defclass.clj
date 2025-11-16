(ns hooks.defclass
  (:require [clj-kondo.hooks-api :as api]))

(defn defclass
  [{:keys [node]}]
  (let [[_ name & spec] (:children node)
        form-node (api/vector-node (cons name spec))
        quoted (api/list-node [(api/token-node 'quote) form-node])
        class-call (api/list-node [(api/token-node 'app.adtworld/class) quoted])
        def-node (api/list-node [(api/token-node 'def) name class-call])
        register-call (api/list-node [(api/token-node 'app.adtworld/register-class!) name])]
    {:node (api/list-node [(api/token-node 'do)
                           def-node
                           register-call])}))
