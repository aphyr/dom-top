(ns dom-top.core-test
  (:require [clojure [pprint :refer [pprint]]
                     [test :refer :all]]
            [criterium.core :refer [bench quick-bench]]
            [dom-top.core :refer :all])
  (:import (java.util.concurrent BrokenBarrierException
                                 CyclicBarrier)))

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
                        (recur)))))))

    ))

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

        (println "\nMulti-acc loopr over vector")
        (quick-bench
          (loopr [sum 0, count 0]
                 [x bigvec]
                 (recur (+ sum x) (inc count)))))

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

        (println "\nMulti-acc loopr over nested seq")
        (quick-bench
          (loopr [pet-count 0
                  pet-names #{}]
                 [person people
                  pet    (:pets person)]
                 (recur (inc pet-count)
                        (conj pet-names (:name pet)))))
        ))))
