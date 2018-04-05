# Dom Top

Unorthodox control flow, for Clojurists with masochistic sensibilities.
Available via [clojars](https://clojars.org/dom-top).

## Tour

See [dom-top.core](src/dom_top/core.clj) for comprehensive documentation with
examples.

- `assert+` works like `assert`, but returns truthy values being tested, and
  throws other types of exceptions (including maps, via ex-info!)
- `fcatch` lifts functions that throw exceptions into functions that *return*
  exceptions.
- `disorderly` is a `do` block that evaluates statements in a new, random order
  every time, instead of sequentially.
- `letr` provides let bindings with early return; particular useful for
  aborting early on failure cases.
- `bounded-future` is just like `future`, but for CPU-bound tasks.
- `real-pmap` provides a fully parallel version of `map`, which spawns one
  thread per element, instead of running on a limited threadpool.
- `bounded-pmap`, by contrast, puts a global limit on parallelism for CPU-bound
  tasks.
- `with-retry` provides `recur` that works through `try/catch` blocks;
  particularly useful for retrying network operations.

## Why would you WANT this?

Look, this is a judgement-free zone, OK? We all have our reasons.

## License

Copyright © 2017, 2018 Kyle Kingsbury

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
