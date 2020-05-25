(ns flybot.pullable.core-test
  (:require
   [flybot.pullable.core :as sut]
   [clojure.test :refer [deftest testing is]]))

(deftest simple
  (let [data {:foo "bar" :baz {:foo2 3 :baz2 'ok}}]
    (testing "query with a single key will just returns a single kv map"
      (is (= {:foo "bar"} (sut/-select (sut/query {:key :foo}) data))))
    (testing "query with nil key but with children"
      (is (= {:foo "bar"} (sut/-select (sut/query {:children [(sut/query {:key :foo})]}) data))))
    (testing "when query has children, it will pass sub data to them"
      (is (= {:baz {:foo2 3}} (sut/-select (sut/query {:key :baz :children [(sut/query {:key :foo2})]}) data)))
      (is (= {:baz {:foo2 3 :baz2 'ok}}
             (sut/-select (sut/query {:key :baz :children [(sut/query {:key :foo2})
                                                           (sut/query {:key :baz2})]}) data))))))

(deftest pattern->query
  (testing "nil pattern makes an empty query"
    (is (= (sut/query {}) (sut/pattern->query nil))))
  (testing "single element pattern makes a simple query"
    (is (= (sut/query {:key :a}) (sut/pattern->query :a))))
  (testing "root vector will make a root empty query with children"
    (is (= (sut/query {:children [(sut/query {:key :a}) (sut/query {:key :b})]})
           (sut/pattern->query [:a :b]))))
  (testing "map makes a query with key and children"
    (is (= (sut/query {:key :a :children [(sut/query {:key :b})]})
           (sut/pattern->query {:a [:b]})))))
