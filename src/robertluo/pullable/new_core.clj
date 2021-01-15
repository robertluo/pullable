(ns robertluo.pullable.new-core)

(defprotocol QueryResult
  (-key [qr]
    "returns the key of a result")
  (-value [qr]
    "returns the value of the result")
  (-data-of [qr]
    "returns the final representation of the result of value"))

;;Model the query result in key and value,
;;the result can then be merged together
(defrecord MapQueryResult [m k v]
  QueryResult
  (-key [_] k)
  (-value [_] v)
  (-data-of [_] {k v}))

(defrecord SeqQueryResult [target k v]
  QueryResult
  (-key [_] k)
  (-value [_] v)
  (-data-of [_] {k v}))

(defn query-result
  ([k v] (query-result {} k v))
  ([m k v] (->MapQueryResult m k v)))

(def seq-result ->SeqQueryResult)

(defn simple-query
  [k]
  (fn [context]
    (if (sequential? context)
      (seq-result
       (empty context)
       k
       (map #(get % k) context))
      (query-result (empty context) k (get context k)))))

(defn join-query
  [kq vq]
  (fn [context]
    (let [key-result (kq context)]
      (query-result
       (empty context)
       (-key key-result)
       (->> key-result
            -value
            vq
            -data-of)))))

(defn vector-query
  [qs]
  (fn [context]
    (let [results (map #(% context) qs)]
      (query-result
       (empty context)
       (map -key results)
       (->> results
            (map -data-of)
            (apply merge))))))
