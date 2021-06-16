(ns dom-top.core
  "Unorthodox control flow."
  (:require [clojure.pprint :refer [pprint]]
            [clojure.walk :as walk]))

(defmacro assert+
  "Like Clojure assert, but throws customizable exceptions (by default,
  IllegalArgumentException), and returns the value it checks, instead of nil.

  Clojure assertions are a little weird. Syntactically, they're a great
  candidate for runtime validation of state--making sure you got an int instead
  of a map, or that an object you looked up was present. However, they don't
  *return* the thing you pass them, which makes it a bit akward to use them in
  nested expressions. You typically have to do a let binding and then assert.
  So... let's return truthy values! Now you can

      (assert+ (fetch-person-from-db :liu)
               \"Couldn't fetch Liu!\")

  Moreover, Clojure assertions sensibly throw AssertionError. However,
  AssertionError is an error that \"should never occur\" and \"a reasonable
  application should not try to catch.\" There are LOTS of cases where you DO
  expect assertions to fail sometimes and intend to catch them: for instance,
  validating user input, or bounds checks. So we're going to throw
  customizable exceptions.

  Oh, and you can throw maps too. Those become ex-infos.

      (assert+ (thing? that)
               {:type   :wasn't-a-thing
                :I'm    [:so :sorry]})"
  ([x]
   `(assert+ ~x "Assert failed"))
  ([x message]
   `(assert+ ~x IllegalArgumentException ~message))
  ([x ex-type message]
   `(or ~x (throw (let [m# ~message]
                    (if (map? m#)
                      (ex-info (str "Assert failed:\n"
                                   (with-out-str (pprint m#)))
                               m#)
                      (new ~ex-type m#)))))))

(defmacro disorderly
  "This is a chaotic do expression. Like `do`, takes any number of forms. Where
  `do` evaluates forms in order, `disorderly` evaluates them in a random order.
  Where `do` returns the result of evaluating the final form, `disorderly`
  returns a sequence of the results of each form, in lexical (as opposed to
  execution) order, making it suitable for binding results.

  This is particularly helpful when you want side effects, but you're not
  exactly sure how. Consider, for instance, testing that several mutations of
  an object all commute.

      (disorderly (do (prn 1) :a)
                  (do (prn 2) :b))

  ... prints either 1 then 2, or 2 then 1, but always returns (:a :b). Note
  that `disorderly` is *not* concurrent: branches evaluate in some order; it's
  just not a deterministic one."
  ([a]
   (list a))
  ([a b]
   `(if (< (rand) 0.5)
      (let [a# ~a
            b# ~b]
        (list a# b#))
      (let [b# ~b
            a# ~a]
        (list a# b#))))
  ([a b & more]
   ; With six branches, it makes more sense to use fns and pay the invocation
   ; overhead, I think. We'll make a bunch of fns and update a mutable object
   ; array when each fn is called
   (let [results (gensym 'results)
         forms   (->> (cons a (cons b more))
                      (map-indexed (fn [i form]
                                     `(fn [] (aset ~results ~i ~form))))
                      vec)]
     `(let [~results (object-array ~(count forms))]
        (doseq [f# (shuffle ~forms)]
          (f#))
        (seq ~results)))))

(defn fcatch
  "Takes a function and returns a version of it which returns, rather than
  throws, exceptions.

      ; returns RuntimeException
      ((fcatch #(throw (RuntimeException. \"hi\"))))"
  [f]
  (fn wrapper [& args]
    (try (apply f args)
         (catch Exception e e))))

(defn bounded-future-call
  "Like clojure.core/future-call, but runs on the bounded agent executor
  instead of the unbounded one. Useful for CPU-bound futures."
  [f]
  ; Adapted from clojure.core/future-call
  (let [f (#'clojure.core/binding-conveyor-fn f)
        fut (.submit clojure.lang.Agent/pooledExecutor ^Callable f)]
    (reify
      clojure.lang.IDeref
      (deref [_] (#'clojure.core/deref-future fut))
      clojure.lang.IBlockingDeref
      (deref
        [_ timeout-ms timeout-val]
        (#'clojure.core/deref-future fut timeout-ms timeout-val))
      clojure.lang.IPending
      (isRealized [_] (.isDone fut))
      java.util.concurrent.Future
      (get [_] (.get fut))
      (get [_ timeout unit] (.get fut timeout unit))
      (isCancelled [_] (.isCancelled fut))
      (isDone [_] (.isDone fut))
      (cancel [_ interrupt?] (.cancel fut interrupt?)))))

(defmacro bounded-future
  "Like future, but runs on the bounded agent executor. Useful for CPU-bound
  futures."
  [& body]
  `(bounded-future-call (^{:once true} fn* [] ~@body)))

(defn bounded-pmap
  "Like pmap, but spawns tasks immediately, and uses the global bounded agent
  threadpool. Ideal for computationally bound tasks, especially when you might
  want to, say, pmap *inside* each of several parallel tasks without spawning
  eight gazillion threads."
  [f coll]
  (->> coll
       (map (fn launcher [x] (bounded-future (f x))))
       doall
       (map deref)))

(defn real-pmap-helper
  "Helper for real-pmap. Maps f over coll, collecting results and exceptions.
  Returns a tuple of [results, exceptions], where results is a sequence of
  results from calling `f` on each element (`nil` if f throws); and exceptions
  is a sequence of exceptions thrown by f, in roughly time order."
  [f coll]
  (let [exceptions    (atom [])
        thread-group  (ThreadGroup. "real-pmap")
        results       (vec (take (count coll) (repeatedly promise)))
        threads (mapv (fn build-thread [i x result]
                        (Thread.
                          thread-group
                          (bound-fn []
                            (try (deliver result (f x))
                                 (catch Throwable t
                                   ; Note that we're not necessarily guaranteed
                                   ; to execute this code. If our call of (f x)
                                   ; throws, we could wind up here, and then
                                   ; another thread with a failure could
                                   ; interrupt us, causing us to jump out of
                                   ; this catch block. However, so long as
                                   ; nobody *outside* this thread group
                                   ; interrupts us, the first interrupt (or any
                                   ; throwable) we get from calling (f x) will
                                   ; be stored in `exception`. If someone
                                   ; outside this thread group interrupts us,
                                   ; chances are it was via interrupting the
                                   ; coordinator thread, and that has its own
                                   ; mechanism for cleaning up and throwing.
                                   (swap! exceptions conj t)
                                   (.interrupt thread-group))))
                          (str "real-pmap " i)))
                      (range)
                      coll
                      results)]
    ; Launch threads
    (doseq [^Thread t threads] (.start t))

    ; Wait for completion. Normally I'd await a barrier, but because of the way
    ; we interrupt threads, I don't think there's any point where we *could*
    ; update a barrier safely. What we *can* do reliably, though, is join the
    ; thread. That can throw InterruptedException, so we catch that, check that
    ; it's really dead, and if so, move on. If we get interrupted and the
    ; thread *isn't* dead, then it's probably that someone interrupted this
    ; real-pmap call, rather than the underlying thread, and
    ; we interrupt the thread group (just in case), and rethrow our own
    ; interrupt.
    (doseq [^Thread t threads]
      (try
        (.join t)
        (catch InterruptedException e
          (when (.isAlive t)
            ; We were interrupted by an outside force, not the thread we
            ; were joining. Clean up our thread group and rethrow.
            (.interrupt thread-group)
            (throw e)))))

    ; OK, all threads are now dead. Return!
    [(mapv (fn [result]
             (if (realized? result)
               @result
               (if (seq @exceptions)
                 ; OK, we know what might have caused this.
                 ::crashed
                 ; Oh shoot, we actually have NO exceptions recorded, which
                 ; might have happened if a thread caught an exception, then
                 ; was interrupted before it could store that exception's cause
                 ; in the exceptions atom. In that case, we'll throw an
                 ; IllegalStateException here.
                 (throw (IllegalStateException. "A real-pmap worker thread crashed *during* exception handling and was unable to record what that exception was.")))))
           results)
     @exceptions]))

(defn real-pmap
  "Like pmap, but spawns tasks immediately, and launches real Threads instead
  of using a bounded threadpool. Useful when your tasks might block on each
  other, and you don't want to deadlock by exhausting the default clojure
  worker threadpool halfway through the collection. For instance,

      (let [n 1000
            b (CyclicBarrier. n)]
        (pmap (fn [i] [i (.await b)]) (range n)))

  ... deadlocks, but replacing `pmap` with `real-pmap` works fine.

  If any thread throws an exception, all mapping threads are interrupted, and
  the original exception is rethrown. This prevents deadlock issues where
  mapping threads synchronize on some resource (like a cyclicbarrier or
  countdownlatch), but one crashes, causing other threads to block indefinitely
  on the barrier. Note that we do not include a ConcurrentExecutionException
  wrapper.

  All pmap threads should terminate before real-pmap returns or throws. This
  prevents race conditions where mapping threads continue doing work
  concurrently with, say, clean-up code intended to run after the call to
  (pmap).

  If the thread calling (pmap) itself is interrupted, all bets are off."
  [f coll]
  (let [[results exceptions] (real-pmap-helper f coll)]
    (when (seq exceptions)
      ; We'll take the first one as our canonical exception.
      (throw (first exceptions)))
    results))

(defrecord Retry [bindings])

(defmacro with-retry
  "It's really fucking inconvenient not being able to recur from within (catch)
  expressions. This macro wraps its body in a (loop [bindings] (try ...)).
  Provides a (retry & new bindings) form which is usable within (catch) blocks:
  when this form is returned by the body, the body will be retried with the new
  bindings. For instance,

      (with-retry [attempts 5]
        (network-request...)
        (catch RequestFailed e
          (if (< 1 attempts)
            (retry (dec attempts))
            (throw e))))"
  [initial-bindings & body]
  (assert (vector? initial-bindings))
  (assert (even? (count initial-bindings)))
  (let [bindings-count (/ (count initial-bindings) 2)
        body (walk/prewalk (fn [form]
                             (if (and (seq? form)
                                      (= 'retry (first form)))
                               (do (assert (= bindings-count
                                              (count (rest form))))
                                   `(Retry. [~@(rest form)]))
                               form))
                           body)
        retval (gensym 'retval)]
    `(loop [~@initial-bindings]
       (let [~retval (try ~@body)]
         (if (instance? Retry ~retval)
           (recur ~@(map (fn [i] `(nth (.bindings ~retval) ~i))
                              (range bindings-count)))
           ~retval)))))

(deftype Return [value])

(defn letr-rewrite-return
  "Rewrites (return x) to (Return. x) in expr. Returns a pair of [changed?
  expr], where changed is whether the expression contained a return."
  [expr]
  (let [return? (atom false)
        expr    (walk/prewalk
                  (fn [form]
                    (if (and (seq? form)
                             (= 'return (first form)))
                      (do (assert
                            (= 2 (count form))
                            (str (pr-str form) " should have one argument"))
                          (reset! return? true)
                          `(Return. ~(second form)))
                      form))
                  expr)]
    [@return? expr]))

(defn letr-partition-bindings
  "Takes a vector of bindings [sym expr, sym' expr, ...]. Returns
  binding-groups: a sequence of vectors of bindgs, where the final binding in
  each group has an early return. The final group (possibly empty!) contains no
  early return."
  [bindings]
  (->> bindings
       (partition 2)
       (reduce (fn [groups [sym expr]]
                 (let [[return? expr] (letr-rewrite-return expr)
                       groups (assoc groups
                                     (dec (count groups))
                                     (-> (peek groups) (conj sym) (conj expr)))]
                   (if return?
                     (do (assert (symbol? sym)
                                 (str (pr-str sym " must be a symbol")))
                         (conj groups []))
                     groups)))
               [[]])))

(defn letr-let-if
  "Takes a sequence of binding groups and a body expression, and emits a let
  for the first group, an if statement checking for a return, and recurses;
  ending with body."
  [groups body]
  (assert (pos? (count groups)))
  (if (= 1 (count groups))
    ; Final group with no returns
    `(let ~(first groups) ~@body)

    ; Group ending in a return
    (let [bindings  (first groups)
          final-sym (nth bindings (- (count bindings) 2))]
      `(let ~bindings
         (if (instance? Return ~final-sym)
           (.value ~final-sym)
           ~(letr-let-if (rest groups) body))))))

(defmacro letr
  "Let bindings, plus early return.

  You want to do some complicated, multi-stage operation assigning lots of
  variables--but at different points in the let binding, you need to perform
  some conditional check to make sure you can proceed to the next step.
  Ordinarily, you'd intersperse let and if statements, like so:

      (let [res (network-call)]
        (if-not (:ok? res)
          :failed-network-call

          (let [people (:people (:body res))]
            (if (zero? (count people))
              :no-people

              (let [res2 (network-call-2 people)]
                ...

  This is a linear chain of operations, but we're forced to nest deeply because
  we have no early-return construct. In ruby, we might write

      res = network_call
      return :failed_network_call if not x.ok?

      people = res[:body][:people]
      return :no-people if people.empty?

      res2 = network_call_2 people
      ...

  which reads the same, but requires no nesting thanks to Ruby's early return.
  Clojure's single-return is *usually* a boon to understandability, but deep
  linear branching usually means something like

  - Deep nesting         (readability issues)
  - Function chaining    (lots of arguments for bound variables)
  - Throw/catch          (awkward exception wrappers)
  - Monadic interpreter  (slow, indirect)

  This macro lets you write:

      (letr [res    (network-call)
             _      (when-not (:ok? res) (return :failed-network-call))
             people (:people (:body res))
             _      (when (zero? (count people)) (return :no-people))
             res2   (network-call-2 people)]
        ...)

  letr works like let, but if (return x) is ever returned from a binding, letr
  returns x, and does not evaluate subsequent expressions.

  If something other than (return x) is returned from evaluating a binding,
  letr binds the corresponding variable as normal. Here, we use _ to indicate
  that we're not using the results of (when ...), but this is not mandatory.
  You cannot use a destructuring bind for a return expression.

  letr is not a *true* early return--(return x) must be a *terminal* expression
  for it to work--like (recur). For example,

      (letr [x (do (return 2) 1)]
        x)

  returns 1, not 2, because (return 2) was not the terminal expression. Someone
  clever should fix this.

  (return ...) only works within letr's bindings, not its body."
  [bindings & body]
  (assert (vector? bindings))
  (assert (even? (count bindings)))
  (let [groups (letr-partition-bindings bindings)]
    (letr-let-if (letr-partition-bindings bindings) body)))
