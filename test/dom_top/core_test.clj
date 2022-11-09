(ns dom-top.core-test
  (:require [clojure [pprint :refer [pprint]]
                     [test :refer :all]]
            [clj-commons.primitive-math :as prim]
            [criterium.core :refer [bench quick-bench]]
            [dom-top.core :refer :all])
  (:import (java.util.concurrent BrokenBarrierException
                                 CyclicBarrier)))

(use-fixtures :once (fn [run-tests]
                      (binding [*warn-on-reflection* true]
                        (run-tests))))

(deftest assert+-test
  (testing "passthrough"
    (is (= :foo (assert+ :foo)))
    (is (= :foo (assert+ :foo "failed")))
    (is (= :foo (assert+ :foo IllegalStateException "failed"))))

  (testing "Default error"
    (is (thrown-with-msg? IllegalArgumentException #"\AAssert failed\z"
                          (assert+ false))))

  (testing "Custom message"
    (is (thrown-with-msg? IllegalArgumentException #"\Ahi\z"
                          (assert+ false "hi"))))

  (testing "Custom class"
    (is (thrown-with-msg? RuntimeException #"\AYOU!\?\z"
                          (assert+ nil RuntimeException "YOU!?"))))

  (testing "Ex-info"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"\AAssert failed:\n\{:type :frog-blast\}"
                          (assert+ false {:type :frog-blast})))))

(deftest disorderly-test
  (testing "2 branches"
    (let [n         100
          outcomes  (->> (fn []
                           (let [a (atom [])]
                             [(disorderly
                                (do (swap! a conj 0) :a)
                                (do (swap! a conj 1) :b))
                              @a]))
                         repeatedly
                         (take n))
          returns   (frequencies (map first outcomes))
          effects   (frequencies (map second outcomes))]

      (testing "return vals in order"
        (is (= {[:a :b] n} returns)))

      (testing "evaluates both branches"
        (is (= #{[0 1] [1 0]}
               (set (keys effects)))))

      (testing "roughly as often"
        (->> (vals effects)
             (every? (fn [freq] (<= (Math/abs (double (- freq (/ n 2))))
                                    (Math/sqrt n))))
             is))))

  (testing "3 branches"
    (let [n   100
          outcomes (->> (fn []
                          (let [a (atom [])]
                            [(disorderly
                               (do (swap! a conj 0) :a)
                               (do (swap! a conj 1) :b)
                               (do (swap! a conj 2) :c))
                             @a]))
                        repeatedly
                        (take n))
          returns (frequencies (map first outcomes))
          effects (frequencies (map second outcomes))]

      (testing "return vals in order"
        (is (= {[:a :b :c] n} returns)))

      (testing "evaluates all branches"
        (is (= #{[0 1 2] [0 2 1] [1 0 2] [1 2 0] [2 0 1] [2 1 0]}
               (set (keys effects)))))

      (testing "roughly as often"
        (->> (vals effects)
             (every? (fn [freq]
                       (<= (Math/abs (double (- freq (/ n 6))))
                           (Math/sqrt n))))
             is)))))


(deftest fcatch-test
  (let [ex (RuntimeException. "foo")]
    (is (identical? ex ((fcatch #(throw ex)))))))

(deftest real-pmap-helper-test
  (testing "catches exceptions"
    (let [res (real-pmap-helper (fn [x]
                              (when (= x 0)
                                (Thread/sleep 5)
                                (throw (RuntimeException. "hi")))

                              (throw (BrokenBarrierException. "augh")))
                            (range 5))]
      (is (= (repeat 5 :dom-top.core/crashed) (first res)))
      (is (= {BrokenBarrierException 4
              InterruptedException 1}
             (frequencies (map class (second res))))))))

(deftest real-pmap-test
  (let [n 1000
        b (CyclicBarrier. n)
        results (real-pmap (fn [i] [i (.await b)]) (range n))]
    (testing "preserves input order"
      (is (= (range n) (map first results))))

    (testing "counts down correctly"
      (is (= (range n) (sort (map second results))))))

  (testing "enforces termination before return"
    (let [completed (atom [])]
      (try (real-pmap (fn [dt]
                        (Thread/sleep dt)
                        (swap! completed conj dt)
                        (throw (IllegalStateException. "whoops")))
                      [50 0])
           (catch IllegalStateException e
             (assert (= "whoops") (.getMessage (.getCause e)))))
      (is (= [0] @completed))
      ; Other thread should have died.
      (Thread/sleep 100)
      (is (= [0] @completed))))

  (testing "doesn't deadlock"
    (let [n 6
          b (CyclicBarrier. n)]
      (is (thrown-with-msg? RuntimeException #"Agh!"
                            (real-pmap (fn [i]
                                         (when (= i (dec n))
                                           (throw (RuntimeException. "Agh!")))
                                         (.await b))
                                       (range n)))))))

(deftest bounded-pmap-test
  (let [n       1000
        threads (atom #{})
        results (bounded-pmap (fn [i]
                                (swap! threads conj (Thread/currentThread))
                                (- i))
                              (range n))]
    (testing "Performs transformation preserving order"
      (is (= results (map - (range n)))))

    (testing "Bounded concurrency"
      (is (<= (count @threads)
              (+ 2 (.. Runtime getRuntime availableProcessors)))))))

(deftest with-retry-test
  (testing "no bindings"
    (is (= 1 (with-retry []
               (/ 1 (rand-int 2))
               (catch ArithmeticException e
                 (retry))))))

  (testing "countdown"
    (let [tries (atom [])]
      (is (= :exhausted (with-retry [attempts 5]
                          (swap! tries conj attempts)
                          (/ 1 0)
                          (catch ArithmeticException e
                            (if (< 1 attempts)
                              (retry (dec attempts))
                              :exhausted)))))
      (is (= [5 4 3 2 1] @tries)))))

(deftest letr-test
  (testing "no bindings"
    (is (= (letr []) nil))
    (is (= (letr [] 1 2) 2)))

  (testing "standard bindings"
    (is (= (letr [a 1, b a] 2 a) 1)))

  (testing "early return"
    (let [side-effect (atom false)]
      (is (= (letr [a   1
                    x   (if (pos? a) (return :pos) :neg)
                    foo (reset! side-effect true)]
               x)
             :pos))
      (is (not @side-effect))))

  (testing "using non-return branch"
    (let [side-effect (atom false)]
      (is (= (letr [a   -1
                    x   (if (pos? a) (return :pos) :neg)
                    foo (reset! side-effect true)]
               x)
             :neg))
      (is @side-effect)))

  (testing "multiple return"
    (is (= (letr [a 2
                  _ (when (= a 1) (return :1))
                  _ (when (= a 2) (return :2))
                  _ (when (= a 3) (return :3))]
             4)
           :2))))

(deftest loopr-test
  (testing "simple accumulator"
    (is (= 4 (loopr [count 0] [x [1 2 3 4]] (recur (inc count)))))
    )

  (testing "actually iterates"
    (is (= [1 2 3 4]
           (loopr [out []] [x [1 2 3 4]] (recur (conj out x))))))

  (testing "nested"
    (is (= [[:kyle      :freddie]
            [:felicity  :qubit]
            [:felicity  :spark]]
           ;(pprint (macroexpand (macroexpand
           (loopr [owner-pets []]
                  [person [{:name :kyle, :pets [:freddie]}
                           {:name :felicity, :pets [:qubit, :spark]}]
                   pet (:pets person)]
                  (recur (conj owner-pets [(:name person) pet]))))))

  (testing "multiple accumulators"
    (is (= [5 15]
           (loopr [count 0
                   sum   0]
                  [x [1 2 3 4 5]]
                  (recur (inc count) (+ sum x))))))

  (let [matrix [[1 2 3] [4 5 6] [7 8 9]]]
    (testing "via"
      (is (= [9 45]
             (loopr [count 0, sum 0]
                    [row matrix, x row]
                    (recur (inc count) (+ sum x)))
             (loopr [count 0, sum 0]
                    [row matrix :via :iterator
                     x   row    :via :iterator]
                    (recur (inc count) (+ sum x)))
             (loopr [count 0, sum 0]
                    [row matrix :via :reduce
                     x   row    :via :iterator]
                    (recur (inc count) (+ sum x)))
             (loopr [count 0, sum 0]
                    [row matrix :via :iterator
                     x   row    :via :reduce]
                    (recur (inc count) (+ sum x)))
             (loopr [count 0, sum 0]
                    [row matrix :via :reduce
                     x   row    :via :reduce]
                    (recur (inc count) (+ sum x)))
             )))

    (testing "final"
      (is (= {:count 9, :sum 45, :mean 5}
             (loopr [count 0, sum 0]
                    [row matrix, x row]
                    (recur (inc count) (+ sum x))
                    {:count count, :sum sum, :mean (/ sum count)}))))

    (testing "arrays"
      (is (= 6 (loopr [sum 0]
                      [x (int-array [1 2 3]) :via :array]
                      (recur (+ sum x)))))
      (let [matrix (to-array-2d [[1 2 3] [4 5 6] [7 8 9]])]
        (is (= [9 45]
               (loopr [count 0, sum 0]
                      [row matrix :via :array, x row :via :array]
                      (recur (inc count) (+ sum x)))
               (loopr [count 0, sum 0]
                      [row matrix :via :array, x row :via :reduce]
                      (recur (inc count) (+ sum x)))
               (loopr [count 0, sum 0]
                      [row matrix :via :reduce, x row :via :array]
                      (recur (inc count) (+ sum x)))))))

    (testing "nestable"
      (is (= [9 45]
             (loopr [count 0, sum 0]
                    [row matrix :via :iterator]
                    (let [[sum count] (loopr [sum sum, count count]
                                             [x row :via :iterator]
                                             (recur (+ sum x) (inc count)))]
                      (recur count sum)))
             (loopr [count 0, sum 0]
                    [row matrix :via :reduce]
                    (let [[sum count] (loopr [sum sum, count count]
                                             [x row :via :reduce]
                                             (recur (+ sum x) (inc count)))]
                      (recur count sum))))))

    (testing "allows nested recurs within body"
      ; The reduce tactic rewrites recurs forms, and we need to make sure it
      ; doesn't interfere with inner recurs.
      (is (= 10 (loopr [sum 0] [x [1 2 3] :via :reduce]
                       (letfn [(powersum [x acc] ; fn with recur
                                 (if (zero? x)
                                   acc
                                   (recur (dec x) (+ x acc))))]
                         (recur (+ (loop [i 3] ; trivial loop; returns sum
                                     (if (= i 0)
                                       sum
                                       (recur (dec i))))
                                   (powersum x 0)))))))
      (is (= 10 (loopr [sum 0] [x [1 2 3] :via :iterator]
                       (letfn [(powersum [x acc] ; fn with recur
                                 (if (zero? x)
                                   acc
                                   (recur (dec x) (+ x acc))))]
                         (recur (+ (loop [i 3] ; trivial loop; returns sum
                                     (if (= i 0)
                                       sum
                                       (recur (dec i))))
                                   (powersum x 0)))))))))

  (testing "early return"
    ; Find a value and its index
    (is (= {:x 3, :i 1}
           (loopr [i 0]
                  [x [1 3 2] :via :reduce]
                  (if (= x 3)
                    {:x x, :i i}
                    (recur (inc i))))
           (loopr [i 0]
                  [x [1 3 2] :via :iterator]
                  (if (= x 3)
                    {:x x, :i i}
                    (recur (inc i))))))
    (testing "nested"
      (let [pairs [[1 2] [3 4] [5 6]]]
        (is (= {:x 3, :i 2}
               (loopr [i 0]
                      [pair pairs :via :reduce
                       x    pair  :via :reduce]
                      (if (= x 3)
                        {:x x, :i i}
                        (recur (inc i))))
               (loopr [i 0]
                      [pair pairs :via :reduce
                       x    pair  :via :iterator]
                      (if (= x 3)
                        {:x x, :i i}
                        (recur (inc i))))
               (loopr [i 0]
                      [pair pairs :via :iterator
                       x    pair  :via :reduce]
                      (if (= x 3)
                        {:x x, :i i}
                        (recur (inc i))))
               (loopr [i 0]
                      [pair pairs :via :iterator
                       x    pair  :via :iterator]
                      (if (= x 3)
                        {:x x, :i i}
                        (recur (inc i))))
               )))))

  (testing "no accumulator"
    (testing "reduce"
      (let [acc (atom [])
            xs  [1 2 3]]
        (is (= nil
               (loopr []
                      [x xs :via :reduce]
                      (do (swap! acc conj x)
                          (recur)))))
        (is (= [1 2 3] @acc))))

    (testing "iterator"
      (let [acc (atom [])
            xs  [1 2 3]]
        (is (= nil
               (loopr []
                      [x xs :via :iterator]
                      (do (swap! acc conj x)
                          (recur)))))
        (is (= [1 2 3] @acc))))

    (testing "final"
      (is (= :finished (loopr [] [x [1 2 3]] (recur) :finished))))

    (testing "early return"
      (is (= 2
             (loopr []
                    [x [1 2 3] :via :reduce]
                    (if (even? x)
                      x
                      (recur))
                    :not-found)
             (loopr []
                    [x [1 2 3] :via :iterator]
                    (if (even? x)
                      x
                      (recur))
                    :not-found))))

    (testing "nested"
      (let [matrix [[1 2 3] [4 5 6] [7 8 9]]]
        (is (= [:found 5]
               (loopr []
                      [row matrix :via :reduce
                       x   row    :via :reduce]
                      (if (= x 5)
                        [:found 5]
                        (recur)))
               (loopr []
                      [row matrix :via :iterator
                       x   row    :via :reduce]
                      (if (= x 5)
                        [:found 5]
                        (recur)))
               (loopr []
                      [row matrix :via :reduce
                       x   row    :via :iterator]
                      (if (= x 5)
                        [:found 5]
                        (recur)))
               (loopr []
                      [row matrix :via :iterator
                       x   row    :via :iterator]
                      (if (= x 5)
                        [:found 5]
                        (recur))))))))

  (testing "reduce with multiple destructuring accs and destructuring"
    (is (= {:count 4, :sum 10, :min 1, :max 4}
           (loopr [[count sum] [0 0]
                   [min- max-]   [##Inf ##-Inf]]
                  [x [1 2 3 4]]
                  (recur [(inc count) (+ sum x)]
                         [(min min- x) (max max- x)])
                  {:sum sum :count count :min min- :max max-})))))

(deftest rewrite-tails-test
  (is (= 2 (rewrite-tails inc '1)))
  (is (= '(do 1 2)
         (rewrite-tails inc '(do 1 1))))
  (is (= '(do (do 1 2))
         (rewrite-tails inc '(do (do 1 1)))))

  (let [inc* #(if (number? %) (inc %) %)]
    (is (= '(loop* [x 0] (recur 1))
           (rewrite-tails inc* '(loop [x 0] (recur 1)))))
    (is (= '(if 3 2 nil)
           (rewrite-tails inc* '(if 3 1))))
    (is (= '(if 3 (do 1 1 2) nil)
           (rewrite-tails inc* '(when 3 1 1 1))))
    (is (= '(if (even? x) (do 1 2) (do 1 3))
           (rewrite-tails inc* '(if (even? x) (do 1 1) (do 1 2)))))
    ; Hard to check this because the case* expands into a weird gensym thing.
    (let [form (rewrite-tails inc* '(case x :one 1, :two 2, 3))]
      (is (= 2 (eval `(let [~'x :one] ~form))))
      (is (= 3 (eval `(let [~'x :two] ~form))))
      (is (= 4 (eval `(let [~'x :default] ~form)))))))

(deftest ^:perf loopr-perf-test
  (let [bigvec   (->> (range 10000) vec)
        bigarray (->> (range 10000) long-array)
        bigseq   (->> (range 10000) (map identity))]
    (testing "single accumulators"
      (println "\nSingle-acc loop with seq over vector")
      (quick-bench
        (loop [sum 0
               xs  bigvec]
          (if-not (seq xs)
            sum
            (let [[x & xs] xs]
              (recur (+ sum x) xs)))))

      (println "\nSingle-acc reduce over vector")
      (quick-bench
        (reduce + bigvec))

      (println "\nSingle-acc loopr over vector")
      (quick-bench
        (loopr [sum 0] [x bigvec] (recur (+ sum x)))))

    (testing "multiple accumulators"
      (println "\nMulti-acc loop with seq over vector")
      (quick-bench
        (loop [sum   0
               count 0
               xs    bigvec]
          (if-not (seq xs)
            [sum count]
            (let [[x & xs] xs]
              (recur (+ sum x) (inc count) xs)))))

      (println "\nMulti-acc reduce over vector")
      (quick-bench
        (reduce (fn [[sum count] x]
                  [(+ sum x) (inc count)])
                [0 0]
                bigvec))

      (println "\nMulti-acc loopr (reduce) over vector")
      (quick-bench
        (loopr [sum 0, count 0]
               [x bigvec :via :reduce]
               (recur (+ sum x) (inc count))))

      (println "\nMulti-acc loopr (iterator) over vector")
      (quick-bench
        (loopr [sum 0, count 0]
               [x bigvec :via :iterator]
               (recur (+ sum x) (inc count))))))

  (testing "nested structures"
    (let [people (->> (range 10000)
                      (map (fn [i]
                             {:name i
                              :pets (->> (range (rand-int 10))
                                         (map (fn [j]
                                                {:name j})))}))
                      doall)]
      (println "\nSingle-acc loop with seq over nested seq")
      (quick-bench
        (loop [pet-count 0
               people    people]
          (if-not (seq people)
            pet-count
            (let [[person & people] people]
              (recur (loop [pet-count pet-count
                            pets      (:pets person)]
                       (if-not (seq pets)
                         pet-count
                         (recur (inc pet-count) (next pets))))
                     people)))))

      (println "\nSingle-acc reduce over nested seq")
      (quick-bench
        (reduce (fn [pet-count person]
                  (reduce (fn [pet-count pet]
                            (inc pet-count))
                          pet-count
                          (:pets person)))
                0
                people))

      (println "\nSingle-acc loopr over nested seq")
      (quick-bench
        (loopr [pet-count 0]
               [person people
                pet    (:pets person)]
               (recur (inc pet-count))))

      (println "\nMulti-acc loop with seq over nested seq")
      (quick-bench
        (loop [pet-count    0
               pet-names    #{}
               people       people]
          (if-not (seq people)
            [pet-count pet-names]
            (let [[person & people] people
                  [pet-count pet-names]
                  (loop [pet-count pet-count
                         pet-names pet-names
                         pets      (:pets person)]
                    (if-not (seq pets)
                      [pet-count pet-names]
                      (let [pet (first pets)]
                        (recur (inc pet-count)
                               (conj pet-names (:name pet))
                               (next pets)))))]
              (recur pet-count pet-names
                     people)))))

      (println "\nMulti-acc reduce over nested seq")
      (quick-bench
        (reduce (fn [acc person]
                  (reduce (fn [[pet-count pet-names] pet]
                            [(inc pet-count)
                             (conj pet-names (:name pet))])
                          acc
                          (:pets person)))
                [0 #{}]
                people))

      (println "\nMulti-acc for->reduce over nested seq")
      (quick-bench
        (->> (for [person people
                   pet    (:pets person)]
               (:name pet))
             (reduce (fn [[pet-count pet-names] pet-name]
                       [(inc pet-count)
                        (conj pet-names pet-name)])
                     [0 #{}])))

      (println "\nMulti-acc loopr over nested seq")
      (quick-bench
        (loopr [pet-count 0
                pet-names #{}]
               [person people
                pet    (:pets person)]
               (recur (inc pet-count)
                      (conj pet-names (:name pet)))))
      ))

  (testing "arrays"
    (let [ary (long-array (range 10000))]
      (println "\nSingle-acc reduce over array")
      (quick-bench
        (reduce + ary))

      (println "\nSingle-acc loopr over array")
      (quick-bench
        (loopr [sum 0]
               [x ary :via :array]
               (recur (+ sum x)))))

    (let [matrix (to-array-2d (repeat 1000 (range 1000)))]
      (println "\nSingle-acc reduce over 2d array")
      (quick-bench
        (reduce (partial reduce +) 0 matrix))

      (println "\nSingle-acc loopr over 2d array")
      (quick-bench
        (loopr [sum 0]
               [row                     matrix :via :array
                x   ^"[Ljava.lang.Long;" row    :via :array]
               (recur (+ sum x)))))
    ))


(deftest reducer-test
  (testing "no acc"
    (is (= :done (transduce identity
                            (reducer []
                                     [x]
                                     (recur)
                                     :done)
                            [1 2 3])))

    (testing "early return"
      (is (= 2 (transduce identity
                          (reducer []
                                   [x]
                                   (if (< 1 x)
                                     x
                                     (recur)))
                          [1 2 3])))))

  (testing "single acc"
    (is (= [:sum 10] (transduce identity
                                (reducer [sum 0]
                                         [x]
                                         (recur (+ sum x))
                                         [:sum sum])
                                [1 2 3 4])))

    (testing "early return"
      (is (= [:sum 6] (transduce identity
                                 (reducer [sum 0]
                                          [x]
                                          (if (< 3 x)
                                            sum
                                            (recur (+ sum x)))
                                          [:sum sum])
                                 (range))))))

  (testing "two accs"
    (is (= 5/3 (transduce identity
                          (reducer [sum   0
                                    count 0]
                                   [x]
                                   (recur (+ sum x) (inc count))
                                   (/ sum count))
                          [1 2 2])))

    (testing "type hints"
      (is (= 5/3 (transduce identity
                            (reducer [^long sum   0
                                      ^int  count 0]
                                     [x]
                                     (recur (+ sum x) (inc count))
                                     (/ sum count))
                            [1 2 2]))))

    (testing "referring to earlier accs in init bindings"
      (is (= [:b :a] (transduce identity
                                (reducer [a :a
                                          b [:b a]]
                                         [x]
                                         b)
                                [1 2 3]))))

    (testing "destructuring"
      (is (= {:cats 5/2
              :dogs 11/2}
             (transduce identity
                        (reducer [[cat-sum cat-count :as cat-acc] [0 0]
                                  [dog-sum dog-count :as dog-acc] [0 0]]
                                 [{:keys [cuteness type]}]
                                 (case type
                                   :cat (recur [(+ cuteness cat-sum)
                                                (inc cat-count)]
                                               dog-acc)
                                   :dog (recur cat-acc
                                               [(+ cuteness dog-sum)
                                                (inc dog-count)]))
                                 {:cats (/ cat-sum cat-count)
                                  :dogs (/ dog-sum dog-count)})
                        [{:type :cat, :cuteness 2}
                         {:type :cat, :cuteness 3}
                         {:type :dog, :cuteness 4}
                         {:type :dog, :cuteness 7}]))))

    (testing "early return"
      ; Note that transduce passes reduced values to the final (f acc) arity
      (is (= [:final [:early 5]]
             (transduce identity
                        (reducer [sum 0, count 0 :as acc]
                                 [x]
                                 (if (= count 2)
                                   [:early sum]
                                   (recur (+ sum x) (inc count)))
                                 [:final acc])
                        [4 1 9 9 9]))))

    (testing "early return, no final"
      (is (= [:early 5]
             (transduce identity
                        (reducer [sum 0, count 0]
                                 [x]
                                 (if (= count 2)
                                   [:early sum]
                                   (recur (+ sum x) (inc count))))
                        [4 1 9 9 9]))))
    ))

(binding [*warn-on-reflection* true
          *unchecked-math* :warn-on-boxed]
  (deftest ^:perf reducer-perf-test
    (let [bigvec (->> (range 100000) vec)]
      (testing "multiple accumulators"
        (println "\nRegular vector destructuring")
        (quick-bench
          (transduce identity
                     (fn
                       ([] [0 0])
                       ([[sum count]] (/ sum count))
                       ([[sum count] x]
                        [(+ sum x) (inc count)]))
                     bigvec))

        (println "\nReducer without types")
        (quick-bench
          (transduce identity
                     (reducer [sum 0, count 0]
                              [x]
                              (recur (+ sum x) (inc count))
                              (/ sum count))
                     bigvec))


        (println "\nReducer with types")
        ; For reasons I can't explain this still winds up going through
        ; RT.longCast. :-/
        (quick-bench
          (transduce identity
                     (reducer [^long sum 0, ^long count 0]
                              [^long x]
                              (let [sum'   ^long (prim/+ sum x)
                                    count' ^long (prim/inc count)]
                                (recur sum' count'))
                              (/ sum count))
                     bigvec))

        ))))

(deftest mutable-acc-type-test
  (testing "primitives"
    (let [at (mutable-acc-type '[byte short int long float double])
          a  (.newInstance at)]
      (set! (.x0 a) (byte 1))
      (set! (.x1 a) (short 2))
      (set! (.x2 a) (int 3))
      (set! (.x3 a) (long 4))
      (set! (.x4 a) (float 1.23))
      (set! (.x5 a) (double 4.56))
      (is (identical? (byte 1)      (.x0 a)))
      (is (identical? (short 2)     (.x1 a)))
      (is (identical? (int 3)       (.x2 a)))
      (is (identical? (long 4)      (.x3 a)))
      ; Not sure about these
      (is (= (float 1.23)  (.x4 a)))
      (is (= (double 4.56) (.x5 a)))
      ))

  (testing "arrays"
    (let [at (mutable-acc-type '[bytes shorts ints longs floats doubles objects])
          a  (.newInstance at)]
      (set! (.x0 a) (byte-array 1))
      (set! (.x1 a) (short-array 1))
      (set! (.x2 a) (int-array 1))
      (set! (.x3 a) (long-array 1))
      (set! (.x4 a) (float-array 1))
      (set! (.x5 a) (double-array 1))
      (set! (.x6 a) (object-array 1))
      (is (= 0 (aget ^bytes (.x0 a) 0)))
      (is (= 0 (aget ^shorts (.x1 a) 0)))
      (is (= 0 (aget ^ints (.x2 a) 0)))
      (is (= 0 (aget ^longs (.x3 a) 0)))
      (is (= 0.0 (aget ^floats (.x4 a) 0)))
      (is (= 0.0 (aget ^doubles (.x5 a) 0)))
      (is (= nil (aget ^objects (.x6 a) 0)))))

  (testing "objects"
    (let [at (mutable-acc-type '[String Object])
          a  (.newInstance at)]
      (set! (.x0 a) "foo")
      (set! (.x1 a) [1 2 3])
      (is (= "foo" (.x0 a)))
      (is (= [1 2 3] (.x1 a))))))
