(ns dom-top.core
  "Unorthodox control flow."
  (:require [clojure [pprint :refer [pprint]]
                     [string :as str]
                     [walk :as walk]]
            [clojure.java.io :as io]
            [riddley.walk :refer [macroexpand-all walk-exprs]])
  (:import (java.lang Iterable)
           (java.util Iterator)
           (java.io File
                    FileOutputStream)
           ; oh no
           (clojure.asm ClassVisitor
                        ClassWriter
                        Opcodes
                        Type)
           (clojure.lang DynamicClassLoader
                         RT)))

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
                      (new ~ex-type ^String m#)))))))

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
           (recur ~@(map (fn [i] (let [retval (vary-meta retval
                                                         assoc :tag `Retry)]
                                   `(nth (.bindings ~retval) ~i)))
                              (range bindings-count)))
           ~retval)))))

(deftype Return [value]
  Object
  (toString [this]
    (str "(Return. " (pr-str value) ")")))

(defmethod print-method Return
  [^Return ret ^java.io.Writer w]
  (.write w (.toString ret)))

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

(defn rewrite-tails*
  "Helper for rewrite-tails which doesn't macroexpand."
  [f form]
  (if-not (seq? form)
    (f form)
    (case (first form)
      (do let* letfn*)
      (list* (concat (butlast form) [(rewrite-tails* f (last form))]))

      if
      (let [[_ test t-branch f-branch] form]
        (list 'if test (rewrite-tails* f t-branch) (rewrite-tails* f f-branch)))

      case*
      (let [[a b c d default clauses & more] form]
        (list* a b c d (rewrite-tails* f default)
               (->> clauses
                    (map (fn [[index [test expr]]]
                           [index [test (rewrite-tails* f expr)]]))
                    (into (sorted-map)))
               more))

      (f form))))

(defn rewrite-tails
  "Takes a Clojure form and invokes f on each of its tail forms--the final
  expression in a do or let, both branches of an if, values of a case, etc."
  [f form]
  (rewrite-tails* f (macroexpand-all form)))

(declare loopr-helper)

(defn loopr-iterator
  "A single loopr layer specialized for traversal using a mutable iterator.
  Builds a form which returns a single accumulator, or a vector of
  accumulators, or a Return, after traversing each x in xs (and more element
  bindings within)."
  [accumulator-bindings [{:keys [lhs rhs] :as eb} & more-element-bindings]
   body {:keys [acc-count] :as opts}]
  (let [accs (map first (partition 2 accumulator-bindings))
        bname (:name eb)
        iter (gensym (str bname "-iter-"))
        res  (gensym (str bname "-res-"))
        rhs  (vary-meta rhs assoc :tag `Iterable)]
    `(let [~iter ^Iterator (.iterator ~rhs)]
       ; Bind each accumulator to itself initially
       (loop [~@(mapcat (juxt identity identity) accs)]
         (if-not (.hasNext ~iter)
           ; We're done iterating
           ~(case (int acc-count)
              0 nil
              1 (first accs)
              `[~@accs])
           (let [~lhs (.next ~iter)]
             ~(if more-element-bindings
                ; More iteration to do within. Descend, come back, recur.
                `(let [~res ~(loopr-helper accumulator-bindings
                                           more-element-bindings
                                           body opts)]
                   (if (instance? Return ~res)
                     ; Early return!
                     ~res
                     (recur ~@(case (int acc-count)
                                0 []
                                1 [res]
                                (map-indexed (fn [i acc] `(nth ~res ~i))
                                             accs)))))
                ; This is the deepest level; use body directly. It'll contain a
                ; compatible recur form. We need to rewrite the body:
                ; (recur x y) -> (recur x y)
                ; x           -> (Return. x)
                (rewrite-tails (fn rewrite-tail [form]
                                 (if (and (seq? form) (= 'recur (first form)))
                                   form
                                   `(Return. ~form)))
                                 body))))))))

(defn loopr-reduce
  "A single loopr layer specialized for traversal using `reduce`. Builds a form
  which returns a single accumulator, or a vector of accumulators, or a Return,
  after traversing each x in xs (and more element bindings within). Reduce is
  often faster over Clojure data structures than an iterator."
  [accumulator-bindings [{:keys [lhs rhs] :as eb} & more-element-bindings]
   body {:keys [acc-count] :as opts}]
  (let [accs (map first (partition 2 accumulator-bindings))
        res  (gensym (str (:name eb) "-res-"))
        acc  (case (int acc-count)
               0 '_
               1 (first accs)
               (vec accs))
        ; The first accumulator we encode in the function arg
        first-acc (case (int acc-count)
                   0 '_
                   (first accs))
        ; Remaining accumulators are stored in volatiles, which we bind to
        ; these expressions each round
        rest-accs (next accs)
        ; The names of each volatile
        rest-acc-volatiles (map-indexed (fn [i acc]
                                          (gensym
                                            (if (symbol? acc)
                                              (str acc "-vol-")
                                              (str i "-vol-"))))
                                        (next accs))
        ; A let binding vector for the initial values of our volatiles
        rest-acc-init-binding (when rest-accs
                                (mapcat (fn [volatile acc]
                                          [volatile `(volatile! ~acc)])
                                        rest-acc-volatiles
                                        rest-accs))
        ; Stores the result of the inner loop
        inner-res  (gensym 'inner-res-)
        ; Stores the result of our reduce
        reduce-res (gensym 'res-)]
    `(let [~@rest-acc-init-binding
           ~reduce-res
       (reduce (fn ~(symbol (str "reduce-" (:name eb))) [~first-acc ~lhs]
                 ; Fetch our current volatiles
                 (let [~@(->> rest-acc-volatiles
                              (map (partial list `deref))
                              (mapcat vector rest-accs))]
                   ~(if more-element-bindings
                      ; More iteration!
                      `(let [~inner-res ~(loopr-helper accumulator-bindings
                                                       more-element-bindings
                                                       body opts)]
                         ; Early return?
                         (if (instance? Return ~inner-res)
                           (reduced ~inner-res)
                           ~(if (< acc-count 2)
                              inner-res
                              ; Update our volatiles and pull out the first acc
                              `(do ~@(map-indexed
                                       (fn [i volatile]
                                         `(vreset! ~volatile
                                                   (nth ~inner-res ~(inc i))))
                                       rest-acc-volatiles)
                                   (first ~inner-res)))))
                      ; This is the deepest level. Rewrite body to replace
                      ; terminal expressions:
                      ; (recur x)   -> x
                      ; (recur x y) -> [x y]
                      ; x           -> (reduced (Return. x))
                      (rewrite-tails
                        (fn rewrite-tail [form]
                          (if (and (seq? form) (= 'recur (first form)))
                            ; Recur
                            (case (int acc-count)
                              0 nil
                              1 (do (assert (= 1 (count (rest form))))
                                    (first (rest form)))
                              ; For multiple accumulators, we want to set the
                              ; rest accs as a side effect, and return the
                              ; first acc.
                              (let [[recur_ first-acc-value & rest-acc-values]
                                    form]
                                `(do ~@(map (fn [volatile value]
                                              `(vreset! ~volatile ~value))
                                            rest-acc-volatiles
                                            rest-acc-values)
                                     ~first-acc-value)))
                            ; Early return
                            `(reduced (Return. ~form))))
                        body))))
               ~(if (zero? acc-count) nil first-acc)
               ~rhs)]
       ~(case (int acc-count)
          ; For 0 or single accs, return the reduce value itself
          (0, 1) reduce-res
          ; With multiple accs, return a vector of their values.
          `(if (instance? Return ~reduce-res)
             ; Early return
             ~reduce-res
             ; Multiple return
             [~reduce-res
              ~@(map (partial list `deref) rest-acc-volatiles)])))))


(defn loopr-array
  "A single loopr layer specialized for traversal over arrays. Builds a form
  which returns a single accumulator, or a vector of accumulators, or a Return,
  after traversing each x in xs using `aget`."
  [accumulator-bindings [{:keys [lhs rhs] :as eb} & more-element-bindings]
   body {:keys [acc-count] :as opts}]
  (let [accs (map first (partition 2 accumulator-bindings))
        bname (:name eb)
        i     (gensym (str bname "-i-"))
        i-max (gensym (str bname "-i-max-"))
        res   (gensym (str bname "-res-"))]
    `(let [~i-max (alength ~rhs)]
       (loop [; Our index into the array
              ~i (int 0)
              ; Initialize each accumulator to itself.
              ~@(mapcat (juxt identity identity) accs)]
         (if (= ~i ~i-max)
           ; Done
           ~(case (int acc-count)
              0 nil
              1 (first accs)
              `[~@accs])
           ; Get an x
           (let [~lhs (aget ~rhs ~i)]
             ~(if more-element-bindings
                ; Descend into inner loop
                `(let [~res ~(loopr-helper accumulator-bindings
                                           more-element-bindings
                                           body opts)]
                   (if (instance? Return ~res)
                     ~res
                     (recur (unchecked-inc-int ~i)
                            ~@(case (int acc-count)
                                0 []
                                1 [res]
                                (map-indexed (fn [i acc] `(nth ~res ~i))
                                             accs)))))
                ; This is the deepest level. Evaluate body, but with early
                ; return for non-recur tails.
                (rewrite-tails (fn rewrite-tail [form]
                                 (if (and (seq? form) (= 'recur (first form)))
                                   `(recur (unchecked-inc-int ~i)
                                           ~@(rest form))
                                   `(Return. ~form)))
                               body))))))))

(defn loopr-helper
  "Helper for building each stage of a nested loopr. Takes an accumulator
  binding vector, a vector of element bindings maps {:lhs, :rhs, :name}, a
  body expression, and an option map with

  :acc-count - The number of accumulators"
  [accumulator-bindings element-bindings body opts]
  (if (empty? element-bindings)
    ; Done!
    body
    ; Generate an iterator loop around the top-level element bindings.
    (let [strategy (case (:via (first element-bindings))
                     :array    loopr-array
                     :iterator loopr-iterator
                     :reduce   loopr-reduce
                     ; With multiple accumulators, vector destructuring can
                     ; make reduce more expensive.
                     nil (if (< 2 (count accumulator-bindings))
                           loopr-iterator
                           ; With single accumulators, Clojure's internal
                           ; reduce is usually more efficient
                           loopr-reduce))]
      (strategy accumulator-bindings element-bindings body opts))))

(defmacro loopr
  "Like `loop`, but for reducing over (possibly nested) collections. Compared to
  `loop`, makes iteration implicit. Compared to reduce, eliminates the need for
  nested reductions, fn wrappers, and destructuring multiple accumulators.
  Compared to `for`, loopr is eager, and lets you carry accumulators.

  Takes an initial binding vector for accumulator variables, (like `loop`);
  then a binding vector of loop variables to collections (like `for`); then a
  body form, then an optional final form. Iterates over each element of the
  collections, like `for` would, and evaluates body with that combination of
  elements bound.

  Like `loop`, the body should generally contain one or more (recur ...) forms
  with new values for each accumulator. Any non-recur form in tail position
  causes loopr to return that value immediately.

  When the loop completes normally, loopr returns:

  - The value of the final expression, which has access to the accumulators, or
  - If no `final` is given...
    - With zero accumulators, returns `nil`
    - With one accumulator, returns that accumulator
    - With multiple accumulators, returns a vector of each.

  For example,

    (loopr [sum 0]
           [x [1 2 3]]
      (recur (+ sum x)))

  returns 6: the sum of 1, 2 and 3.

  This would typically be written as `(reduce + [1 2 3])`, and for single
  accumulators or single loops using `reduce` or `loop` is often more concise.
  Loopred's power comes from its ability to carry multiple accumulators and to
  traverse multiple dimensions. For instance, to get the mean of all elements
  in a matrix:

    (loopr [count 0
            sum   0]
           [row [[1 2 3] [4 5 6] [7 8 9]]
            x   row]
      (recur (inc count) (+ sum x))
      (/ sum count))
    ; returns 45/9 = 5

  Here, we have a body which recurs, and a final expression `(/ sum count)`,
  which is evaluated with the final value of the accumulators. Compare this to
  the equivalent nested reduce:

    (let [[sum count] (reduce (fn [[count sum] row]
                                (reduce (fn [[count sum] x]
                                          [(inc count) (+ sum x)])
                                        [count sum]
                                        row))
                              [0 0]
                              [[1 2 3] [4 5 6] [7 8 9]])]
      (/ sum count))

  This requires an enclosing `let` binding to transform the loop results, two
  calls to reduce, each with their own function, creating and destructuring
  vectors at each level, and keeping track of accumulator initial values far
  from their point of use. The structure of accumulators is encoded in five
  places instead of two, which makes it harder to change accumulators later.
  It also requires deeper indentation. Here's the same loop expressed as a
  flat `loop` over seqs:

    (loop [count 0
           sum   0
           rows  [[1 2 3] [4 5 6] [7 8 9]]
           row   (first rows)]
      (if-not (seq rows)
        (/ sum count)       ; Done with iteration
        (if-not (seq row)   ; Done with row; move on to next row
          (recur count sum (next rows) (first (next rows)))
          (let [[x & row'] row]
            (recur (inc count) (+ sum x) rows row')))))

  This version is less indented but also considerably longer, and the
  interweaving of traversal machinery and accumulation logic makes it
  difficult to understand. It is also significantly slower than the nested
  `reduce`, on account of seq allocation--vectors can more efficiently reduce
  over their internal structure.

  Depending on how many accumulators are at play, and which data structures are
  being traversed, it may be faster to use `loop` with an iterator, `loop` with
  `aget`, or `reduce` with a function. loopr compiles to (possibly nested)
  `reduce` when given a single accumulator, and to (possibly nested) `loop`
  with mutable iterators when given multiple accumulators. You can also control
  the iteration tactic for each collection explicitly:

    (loopr [count 0
            sum   0]
           [row [[1 2 3] [4 5 6] [7 8 9]] :via :reduce
            x   row                       :via :iterator]
      (recur (inc count) (+ sum x))
      (/ sum count))

  This compiles into a `reduce` over rows, and a `loop` over each row using an
  iterators. For array iteration, use `:via :array`:

    (loopr [sum 0]
           [x (long-array (range 10000)) :via :array]
           (recur (+ sum x)))
    ; => 49995000

  Note that alength/aget are *very* sensitive to type hints; use `lein check`
  to ensure that you're not using reflection, and add type hints as necessary.
  On my older xeon, this is roughly an order of magnitude faster than (reduce +
  longs). For nested array reduction, make sure to hint inner collections, like
  so:

    (loopr [sum 0]
           [row                        matrix :via :array
            x   ^\"[Ljava.lang.Long;\" row    :via :array]
           (recur (+ sum x)))))

  Like `loop`, `loopr` supports early return. Any non `(recur ...)` form in
  tail position in the body is returned immediately, without visiting any other
  elements in the collection(s). To search for the first odd number in
  collection, returning that number and its index:

    (loopr [i 0]
           [x [0 3 4 5]]
           (if (odd? x)
             {:i i, :x x}
             (recur (inc i))))
    ; => {:i 1, :x 3}

  When no accumulators are provided, loopr still iterates, returning any
  early-returned value, or the final expression when iteration completes, or
  `nil` otherwise. Here we find an key in a map by value. Note that we can also
  destructure in iterator bindings.

    (loopr []
           [[k v] {:x 1, :y 2}]
           (if (= v 2)
             k
             (recur))
           :not-found)
    ; => :y"
  [accumulator-bindings element-bindings body & [final]]
  (assert (<= 2 (count element-bindings))) ; TODO: determine semantics for this?
  (assert (even? (count accumulator-bindings)))
  (assert (even? (count element-bindings)))
  ; Parse element bindings into a vector of maps
  (let [element-bindings
        (loop [forms     element-bindings
               bindings  []]
          (if-not (seq forms)
            bindings
            (let [[f1 f2 & fs] forms
                  i (count bindings)]
              (if (keyword? f1)
                ; Options for last binding
                (case f1
                  :via (recur fs (update bindings (dec i) assoc :via f2))
                  (throw (IllegalArgumentException.
                           (str "Unrecognized element binding option: "
                                (pr-str f1)
                                " - expected :via"))))
                ; New binding
                (let [; Choose a friendly name for this binding.
                      binding-name (if (symbol? f1)
                                     f1
                                     (symbol (str "iter-" i)))
                      binding {:name binding-name
                               :lhs  f1
                               :rhs  f2}]
                  (recur fs (conj bindings binding)))))))
        acc-names   (map first (partition 2 accumulator-bindings))
        acc-count   (count acc-names)
        acc         (case acc-count
                      0 (gensym 'res-)
                      1 (first acc-names)
                      (vec acc-names))
        opts        {:acc-count acc-count}
        res         (gensym 'res-)]
    `(let [~@accumulator-bindings
           ~res ~(loopr-helper accumulator-bindings element-bindings body opts)]
       (if (instance? Return ~res)
         (.value ~(vary-meta res assoc :tag `Return))
         ~(if final
            `(let [~acc ~res] ~final)
            res)))))

(defonce
  ^{:doc "The classloader we use to load mutable acc-types. We store this to
         prevent it from being GCed and rendering types unusable."
    :tag 'clojure.lang.DynamicClassLoader}
  mutable-acc-class-loader
  (RT/makeClassLoader))

(defonce
  ^{:doc "A mutable cache of mutable accumulator types we've generated. Stores
         a map of type hints (e.g. ['long 'Object]) to classes (e.g.
         MutableAcc-long-Object)."}
  mutable-acc-cache*
  (atom {}))

(defn type->desc
  "Takes a type (e.g. 'int, 'objects, 'longs, 'Foo) and converts it to a JVM
  type descriptor like \"I\"."
  [t]
  (.getDescriptor
    (case t
      byte          Type/BYTE_TYPE
      short         Type/SHORT_TYPE
      int           Type/INT_TYPE
      long          Type/LONG_TYPE
      float         Type/FLOAT_TYPE
      double        Type/DOUBLE_TYPE
      bytes         (Type/getType "[B")
      shorts        (Type/getType "[S")
      ints          (Type/getType "[I")
      longs         (Type/getType "[J")
      floats        (Type/getType "[F")
      doubles       (Type/getType "[D")
      objects       (Type/getType "[Ljava/lang/Object;")
      ; Everything else is an Object for us
      (Type/getType Object))))

(defn load-class!
  "Takes a class name as a string (e.g. foo.bar.Baz) and bytes for its class
  file. Loads the class dynamically, and also emits those bytes to
  *compile-path*, for AOT. Returns class."
  [^String class-name ^bytes class-bytes]
  (let [klass (.defineClass mutable-acc-class-loader class-name class-bytes nil)
        ; Spit out bytes to compile-path as well
        class-file (io/file *compile-path*
                            (str (str/replace class-name "." File/separator)
                                 ".class"))]
    (io/make-parents class-file)
    (with-open [out (FileOutputStream. class-file)]
      (.write out class-bytes))
    klass))

(defn mutable-acc-type
  "Takes a list of types as symbols and returns the class of a mutable
  accumulator which can store those types. May compile new classes on the fly,
  or re-use a cached class.

  This method largely courtesy of Justin Conklin! *hat tip*"
  [types]
  (let [; All objects are the same as far as we're concerned.
        types (mapv (fn [type]
                      (if (and type (re-find #"^[a-z]" (name type)))
                        type
                        'Object))
                    types)]
    (or (get @mutable-acc-cache* types)
        (let [class-name (str "dom_top.core.MutableAcc-"
                              (str/join "-" (map name types)))
              base-type "java/lang/Object"
              ; Construct class bytecode
              cv (doto (ClassWriter. ClassWriter/COMPUTE_FRAMES)
                   (.visit Opcodes/V1_7
                           (bit-or Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL)
                           (.replace class-name \. \/)
                           nil base-type nil))]
          ; Constructor
          (doto (.visitMethod cv Opcodes/ACC_PUBLIC "<init>" "()V" nil nil)
            (.visitCode)
            (.visitVarInsn Opcodes/ALOAD 0)
            ; Super
            (.visitMethodInsn Opcodes/INVOKESPECIAL base-type
                              "<init>" "()V" false)
            (.visitInsn Opcodes/RETURN)
            (.visitMaxs -1 -1)
            (.visitEnd))
          ; Fields
          (doseq [[i t] (map vector (range) types)]
            (doto (.visitField cv Opcodes/ACC_PUBLIC (str "x" i)
                               (type->desc t) nil nil)
              (.visitEnd)))
          ; And load
          (let [klass (load-class! class-name (.toByteArray cv))]
            ; Cache class for reuse
            (swap! mutable-acc-cache* assoc types klass)
            klass)))))

(defmacro reducer
  "Syntactic sugar for writing reducing/transducing functions with multiple
  accumulators. Much like `loopr`, this takes a binding vector of loop
  variables and their initial values, a single binding vector for an element of
  the collection, a body which calls (recur) with new values of the
  accumulators (or doesn't recur, for early return), and a final expression,
  which is evaluated with the accumulators and returned at the end of the
  reduction. Returns a function with 0, 1, and 2-arity forms suitable for use
  with `transduce`.

    (transduce identity
               (reducer [sum 0, count 0]
                        [x]
                        (recur (+ sum x) (inc count))
                        (/ sum count))
               [1 2 2])
    ; => 5/3

  This is logically equivalent to:

    (transduce identity
               (fn ([] [0 0])
                   ([[sum count]] (/ sum count))
                   ([[sum count] x]
                    [(+ sum x) (inc count)]))
               [1 2 2])

  For zero and one-accumulator forms, these are equivalent. However, `reducer`
  is faster for reducers with more than one accumulator. Its identity arity
  creates unsynchronized mutable accumulators (including primitive types, if
  you hint your accumulator variables), and the reduction arity mutates that
  state in-place to skip the need for vector creation & destructuring on each
  reduction step. This makes it about twice as fast as a plain old reducer fn.

  These functions also work out-of-the-box with Tesser, clojure.core.reducers,
  and other Clojure fold libraries.

  If you want to use a final expression with a `reduced` form *and* multiple
  accumulators, add an `:as foo` to your accumulator binding vector. This
  symbol will be available in the final expression, bound to a vector of
  accumulators if the reduction completes normally, or bound to whatever was
  returned early. Using `:as foo` signals that you intend to use early return
  and may not *have* accumulators any more--hence the accumulator bindings will
  not be available in the final expression.

    (transduce identity
               (reducer [sum 0, count 0 :as acc]
                        [x]
                        (if (= count 2)
                          [:early sum]
                          (recur (+ sum x) (inc count)))
                        [:final acc])
               [4 1 9 9 9])
    ; => [:early 5]"
  [accumulator-bindings element-bindings body & [final]]
  (assert (even? (count accumulator-bindings)))
  (assert (= 1 (count element-bindings)))
  (let [element-name (first element-bindings)
        [acc-bindings [_ acc-as-name]] (split-with (complement #{:as})
                                                   accumulator-bindings)
        acc-pairs    (partition 2 acc-bindings)
        acc-names    (mapv first acc-pairs)
        acc-inits    (mapv second acc-pairs)
        acc-count    (count acc-names)]
    (if (< acc-count 2)
      ; Construct a plain old reducer
      (let [acc-name (or (first acc-names) '_)]
        `(fn ~(symbol (str "reduce-" element-name))
           ([] ~(first acc-inits))
           ([~acc-name] ~(if final final acc-name))
           ([~acc-name ~element-name]
            ~(rewrite-tails
               (fn rewrite-tail [form]
                 (if (and (seq? form) (= 'recur (first form)))
                   ; We have a 0 or 1-arity recur form
                   (let [[_ acc-value] form]
                     (assert (= acc-count (count (rest form))))
                     acc-value)
                   ; Early return
                   `(reduced ~form)))
               body))))
      ; What kind of types does our accumulator need?
      (let [types    (mapv (comp :tag meta) acc-names)
            acc-type (symbol (.getName ^Class (mutable-acc-type types)))
            acc-name (with-meta (gensym "acc-") {:tag acc-type})
            fields   (->> (range (count types))
                          (map (comp symbol (partial str "x"))))
            ; [(. acc x0) (. acc x1) ...]
            get-fields (mapv (fn [type field]
                               (with-meta (list '. acc-name field)
                                          {:tag type}))
                             types fields)
            ; [foo (. acc x0) bar (. acc x1) ...]
            ; When we bind we want to strip hints off locals; compiler will
            ; complain
            bind-fields (vec (interleave
                               (map #(vary-meta % dissoc :tag) acc-names)
                               get-fields))
            ; The argument passed to our final arity
            final-name (gensym "final-")]
        `(fn ~(symbol (str "reduce-" element-name))
           ; Construct accumulator and initialize its fields.
           ([] (let [~acc-name (new ~acc-type)
                     ; Bindings like [foo init, _ (set! (. acc x0) foo), ...]
                     ~@(mapcat (fn [acc-name field init]
                                 ; Can't type hint locals with primitive inits
                                 (let [acc (vary-meta acc-name dissoc :tag)]
                                   [acc init
                                    '_ (list 'set! field acc)]))
                                 acc-names get-fields acc-inits)]
                 ~acc-name))
           ; Finalizer; destructure and evaluate final, or just return accs as
           ; vector
           ~(if final
              (if acc-as-name
                `([~final-name]
                  ; Bind input to acc-as-name, converting our mutable accs to a
                  ; vector.
                  (if (instance? ~acc-type ~final-name)
                    ; Normal return
                    (let [~acc-name ~final-name
                          ~acc-as-name ~get-fields]
                      ~final)
                    ; Early return
                    (let [~acc-as-name ~final-name]
                      ~final)))
                ; No early return. Just bind accs.
                `([~acc-name]
                  (let [~@bind-fields]
                    ~final)))
              ; No final expression; return reduced or a vector
              `([~final-name]
                (if (instance? ~acc-type ~final-name)
                  ; Normal return
                  (let [~acc-name ~final-name]
                    ~get-fields)
                  ; Early return
                  ~final-name)))
           ; Reduce: destructure acc and turn recur into mutations
           ([~acc-name ~element-name]
            (let ~bind-fields
              ~(rewrite-tails
                 (fn rewrite-tail [form]
                   (if (and (seq? form) (= 'recur (first form)))
                     ; Recur becomes mutate and return acc
                     (do (assert (= acc-count (count (rest form))))
                         `(do ~@(map (fn [get-field value]
                                      `(set! ~get-field ~value))
                                    get-fields
                                    (rest form))
                              ~acc-name))
                     ; Early return becomes a reduced value
                     `(reduced ~form)))
                 body))))))))
