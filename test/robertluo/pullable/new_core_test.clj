(ns robertluo.pullable.new-core-test
  (:require
   [robertluo.pullable.new-core :as sut]
   [clojure.test :refer [deftest is]]))

(deftest simple-query
  (let [simple-q (sut/simple-query :a)]
    (is (= (sut/seq-result [] :a [2 3])
           (simple-q [{:a 2 :b 3} {:a 3 :b 5}])))))

(deftest join-query
  (let [simple-q (sut/join-query (sut/simple-query :a) (sut/simple-query :b))]
    (is (= (sut/query-result :a {:b 3})
           (simple-q {:a {:b 3 :c 2}})))))

(deftest vector-query
  (let [simple-q (sut/vector-query [(sut/simple-query :a) (sut/simple-query :b)])]
    (is (= (sut/query-result [:a :b] {:a 2 :b 5})
           (simple-q {:a 2 :b 5 :c 3})))))
