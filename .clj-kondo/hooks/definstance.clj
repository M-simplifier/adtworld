(ns hooks.definstance
  (:require [clj-kondo.hooks-api :as api]))

(defn definstance
  [{:keys [node]}]
  (let [[_ class type & body] (:children node)
        sexprs (map api/sexpr body)
        has-doc? (and (seq sexprs) (string? (first sexprs)))
        doc-node (if has-doc?
                   (first body)
                   (api/token-node nil))
        impl-node (or (nth body (if has-doc? 1 0) nil)
                      (api/token-node nil))
        quoted-class (api/list-node [(api/token-node 'quote) class])
        quoted-type (api/list-node [(api/token-node 'quote) type])
        map-node (api/map-node [(api/keyword-node :class) quoted-class
                                (api/keyword-node :type) quoted-type
                                (api/keyword-node :doc) doc-node
                                (api/keyword-node :impl) impl-node])]
    {:node (api/list-node [(api/token-node 'app.adtworld/register-instance!)
                           map-node])}))
