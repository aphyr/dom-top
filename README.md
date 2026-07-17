# Dom Top

Unorthodox control flow, for Clojurists with masochistic sensibilities.

![](https://clojars.org/dom-top/latest-version.svg)
[![cljdoc badge](https://cljdoc.org/badge/dom-top/dom-top)](https://cljdoc.org/d/dom-top/dom-top)

## Tour

See [dom-top.core](src/dom_top/core.clj) for comprehensive documentation with
examples.

### Iteration

- `with-retry` provides `recur` that works through `try/catch` blocks;
  particularly useful for retrying network operations.
- `letr` provides let bindings with early return; particular useful for
  aborting early on failure cases.
- `loopr` expresses reductions with multiple accumulators over multiple
  dimensions. It combines the nested iteration of `for`, the multiple
  accumulators/dimensions of `loop`, and the concise iteration/accumulation
  structure of `reduce`. Also, it's fast.
- `reducer` builds a reducing function for multiple accumulators, carrying
  state in a dynamically-compiled, primitive-aware, mutable accumulator
  datatype. For simple reductions it's about twice as fast as an idiomatic
  vector accumulator.

### Concurrency

- `ex-promise` is a promise which can throw exceptions, like `future` does.
- `bounded-future` is just like `future`, but for CPU-bound tasks.
- `bounded-pmap`, by contrast, puts a global limit on parallelism for CPU-bound
  tasks.
- `real-pmap` provides a fully parallel version of `map`, which spawns one
  thread per element, instead of running on a limited threadpool.
- `timeout` times out IO-bound work.

### Error Handling

- `assert+` works like `assert`, but returns truthy values being tested, and
  throws other types of exceptions (including maps, via ex-info!)
- `fcatch` lifts functions that throw exceptions into functions that *return*
  exceptions.

### Other

- `disorderly` is a `do` block that evaluates statements in a new, random order
  every time, instead of sequentially.

## Why would you WANT this?

Look, this is a judgement-free zone, OK? We all have our reasons.

## Thanks

[Justin Conklin](https://github.com/jgpc42) offered advice on ASM and wrote the
core of dom-top's generator of mutable accumulators.

## License

Copyright © 2017--2026 Kyle Kingsbury

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
