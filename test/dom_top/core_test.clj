(ns dom-top.core-test
  (:require [clojure.test :refer :all]
            [dom-top.core :refer :all])
  (:import (java.util.concurrent CyclicBarrier)))

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
      (is (= (range n) (sort (map second results)))))))

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
