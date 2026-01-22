(ns maa.hooks
  (:require [clj-kondo.hooks-api :as api]))

(defn nest [clauses]
  (let [[op & more] clauses]
    (if (empty? more)
      ;; final form
      op
      (cond
        (api/list-node? op)
        (api/list-node
         (concat (:children op) [(nest more)]))

        :else
        (api/list-node
         (concat [op (first more)] [(nest (next more))]))))))

(defn unnest [{:keys [node]}]
  (let [[_ & forms] (:children node)]
    {:node (nest forms)}))
