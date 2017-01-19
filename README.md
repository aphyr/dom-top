# Dom Top

Unorthodox control flow, for Clojurists with masochistic sensibilities.
Available via [clojars](https://clojars.org/dom-top).

## Tour

See [dom-top.core](src/dom_top/core.clj) for comprehensive documentation with
examples.

- `fcatch` lifts functions that throw exceptions into functions that *return*
  exceptions.
- `letr` provides let bindings with early return; particular useful for
  aborting early on failure cases.
- `real-pmap` provides a fully parallel version of `map`, which spawns one
  thread per element, instead of running on a limited threadpool.
- `with-retry` provides `recur` that works through `try/catch` blocks;
  particularly useful for retrying network operations.

## Why would you WANT this?

Look, this is a judgement-free zone, OK? We all have our reasons.

## License

Copyright © 2017 Kyle Kingsbury

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
