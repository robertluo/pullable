(ns robertluo.pullable-test
  (:require
   [robertluo.pullable :as sut]
   [clojure.test :refer [deftest testing is]]))

(deftest pull
  (let [data {:int 8
              :map {:int 9
                    :kw  :foo
                    :recur [{:int 10 :recur [{:int 100 :recur [{:int 1000}]}]}
                            {:int 20 :recur [{:int 200 :recur [{:int 2000 :recur [{:int 20000}]}]}]}]
                    :vec [{:int 5} {:int 8
                                    :kw  :bar}]}
              :fn str
              :fn2 (fn [i] {:val (inc i)})}]
    (testing "simple pull pattern"
      (is (= {:int 8} (sut/pull data :int))))
    (testing "complex pattern"
      (is (= {:int 8 :map {:int 9 :kw :foo}}
             (sut/pull data [:int {:map [:int :kw]}]))))
    (testing "nested join"
      (is (= {:map {:vec [{:int 5} {:int 8}]}}
             (sut/pull data '{:map {:vec :int}}))))
    (testing "list can provide options"
      (is (= {:none 0} (sut/pull data '(:none :not-found 0)))))
    (testing "join with :seq key is an explicit seq, will pull over its element"
      (is (= {:map {:vec [{:int 5} {:int 8}]}}
             (sut/pull data {:map {:vec :int :seq []}}))))
    (testing ":limit and :offset will give a pagination"
      (is (= {:map {:vec [{:int 8}]}}
             (sut/pull data '{:map {:vec [(:int :seq [1 1])]}}))))
    (testing "can pull in a root sequence"
      (is (= [{:int 8} {:int 8}]
             (sut/pull [data data] [:int]))))
    (testing "pull with a :with option, will call the value as a function"
      (is (= {:fn "hello world"}
             (sut/pull data '(:fn :with ["hello" " " "world"])))))
    (testing ":with option can be pulled as if it is a normal one"
      (is (= {:fn2 {:val 8}}
             (sut/pull data '(:fn2 :with [7])))))
    (testing "#pull/depth tag will recur pattern"
      (is (= {:map {:recur [{:int 10 :recur {:int [200]}}
                            {:int 20 :recur {:int [100]}}]}}
             (sut/pull data {:map #pull/depth [{:recur :int} 1]}))))))

(deftest simple
  (is (= [{:a 3} {:a 5}]
         (sut/pull [{:a 3 :b 4} {:a 5 :b 3}] :a))))

(deftest complex-query-as-key
  (testing "join-query can be a key, its key become the key of result"
    (let [data {:a {:b {:c 5}}}]
      (is (= {:a {:b {:c 5}}}
             (sut/pull data '{:a {:b :c}})
             (sut/pull data '{:a [{:b [:c]}]})
             (sut/pull data '{{:a :b} :c})))))
  (testing "vector query as a join key make it union join"
    (let [data {:a {:c 5} :b {:c -5}}]
      (is (= {[:a :b] [{:c -5} {:c 5}]}
             (sut/pull data '{[:a :b] :c})))))
  (testing "with query as a key"
    (let [data {:a (fn [i] {:key i :value (str i)})}]
      (is (= {5 {:key 5 :value "5"}}
             (sut/pull data '{(:a :with [5] :as 5) [:key :value]}))))))

(deftest set-query
  (testing "sets are also can be queried"
    (let [data #{{:a 5} {:a 8}}]
      (is (= #{{:a 8} {:a 5}}
             (sut/pull data ':a))))))

(deftest union-query
  (testing "union query"
    (let [data {:a {:foo 3} :b {:foo 4}}]
      (is (= {:ok [{:foo 4} {:foo 3}]}
             (sut/pull data '{([:a :b] :as :ok) :foo}))))))
