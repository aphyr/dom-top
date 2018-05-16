(ns dom-top.core-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [dom-top.core :refer :all])
  (:import (java.util.concurrent CyclicBarrier)))

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
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"\AAssert failed"
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

(deftest real-pmap-test
  (let [n 1000
        b (CyclicBarrier. n)
        results (real-pmap (fn [i] [i (.await b)]) (range n))]
    (testing "preserves input order"
      (is (= (range n) (map first results))))

    (testing "counts down correctly"
      (is (= (range n) (sort (map second results)))))

    (testing "enforces termination before return"
      (let [completed (atom [])]
        (try (real-pmap (fn [dt]
                          (Thread/sleep dt)
                          (swap! completed conj dt)
                          (throw (IllegalStateException. "whoops")))
                        [0 50])
             (catch java.util.concurrent.ExecutionException e
               (assert (= "whoops") (.getMessage (.getCause e)))))
        (is (= [0] @completed))
        ; Other thread should have died.
        (Thread/sleep 100)
        (is (= [0] @completed))))))

(defn recursive-pmap
  "Helper for bounded-pmap-test"
  [depth breadth coll]
  (if (= depth 0)
    (reduce + (range 1000000))
    (bounded-pmap (partial recursive-pmap (dec depth) breadth)
                  (range breadth))))

(defn recursive-fj-pmap
  "Helper for bounded-pmap-test"
  [depth breadth coll]
  (if (= depth 0)
    (reduce + (range 1000000))
    (fj-pmap (partial recursive-fj-pmap (dec depth) breadth)
             (range breadth))))

(deftest bounded-pmap-test
  (let [n       1000
        threads (atom #{})
        results (bounded-pmap (fn [i]
                                (swap! threads conj (Thread/currentThread))
                                (- i))
                              (range n))]
    (testing "performs transformation preserving order"
      (is (= results (map - (range n)))))

    (testing "bounded concurrency"
      (is (<= (count @threads)
              (+ 2 (.. Runtime getRuntime availableProcessors))))))

  ; Imagine a system which can run at most 2 concurrent threads, and an
  ; execution graph like
  ;
  ;     (bounded-pmap (fn [letter]
  ;                     (bounded-pmap (fn [number]
  ;                                     [letter number])
  ;                                   [1 2]))
  ;                    [:a :b])
  ;
  ; If a and b run concurrently, a naive implementation won't have the
  ; *additional* threads necessary to actually do the work, and this will
  ; deadlock. Even if it doesn't deadlock, it might run slower than required,
  ; because some threads are just sitting around waiting for results from
  ; other threads instead of doing work; in deep graphs on small systems,
  ; this might add up quick. This test stresses this scenario by generating
  ; really deep recursive pmap graphs.
  (testing "doesn't starve recursive call graphs"
    (let [depth   2
          breadth 10]
      (time (doall (flatten (recursive-pmap depth breadth nil)))))))

(deftest fj-pmap-test
  (let [n       1000
        threads (atom #{})
        results (fj-pmap (fn [i]
                                (swap! threads conj (Thread/currentThread))
                                (- i))
                              (range n))]
    (testing "performs transformation preserving order"
      (is (= results (map - (range n)))))

    (testing "bounded concurrency"
      (is (<= (count @threads)
              (+ 2 (.. Runtime getRuntime availableProcessors))))))

  ; Imagine a system which can run at most 2 concurrent threads, and an
  ; execution graph like
  ;
  ;     (bounded-pmap (fn [letter]
  ;                     (bounded-pmap (fn [number]
  ;                                     [letter number])
  ;                                   [1 2]))
  ;                    [:a :b])
  ;
  ; If a and b run concurrently, a naive implementation won't have the
  ; *additional* threads necessary to actually do the work, and this will
  ; deadlock. Even if it doesn't deadlock, it might run slower than required,
  ; because some threads are just sitting around waiting for results from
  ; other threads instead of doing work; in deep graphs on small systems,
  ; this might add up quick. This test stresses this scenario by generating
  ; really deep recursive pmap graphs.
  (testing "doesn't starve recursive call graphs"
    (let [depth   2
          breadth 10]
      (time (doall (flatten (recursive-fj-pmap depth breadth nil)))))))

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
