# Adding custom primitive types into default comparator

SRFI 114 allows Scheme implementations to extend definition of the `default-comparator` to handle
other types supported by the Scheme system (e.g., sets and bags of the SRFI 113). However, it does
not define any interface for that, leaving the decision to SRFI implementation itself.

This SRFI implementation defines a procedure `register-default-comparator!` exported by the `(srfi
114 default-comparator)` library. The procedure receives a comparator as its single argument and
registers it as one of the extension comparators. The extension comparators will be considered when
the operand of `default-comparator` is not of a primitive type defined by the SRFI: the empty list,
a pair, a boolean, a character, a string, a symbol, a number, a vector, or a bytevector.

The extension comparator should define any comparator procedures you wish to use (e.g., equality
predicate if you want to check equality of your objects via `default-comparator`). It is not an error
to leave the procedures undefined in your custom comparator if you never actually use them.

According to the SRFI, `default-comparator` must define a strict order among the objects of distinct
types. The order is the following: primitive types in the order specified by the SRFI < extension types
in the order of their registration < any other types which are not accepted by the registered comparators.
This order is absolute and does not depend on comparison or equality procedures defined by the registered
comparators. (Also, do remember that the order in which the libraries are loaded is not defined, so
is the registration order of your types.)

For example, if you register _foo_ at first, and then register _bar_ after it, then all instances of
_foo_ will be strictly greater than any bytevector, all instances of _bar_ will be strictly greater
that any instance of _foo_, and all procedures (not registered within the default comparator) will be
strictly greater that any instance of _bar_. Ordering between the instances of _foo_ or _bar_ is defined
by their respective comparators. If no comparison or equality procedure is defined for _foo_, any
instance of _bar_ will still be not equal to and greater than any instance of _foo_. However, all
procedures will still compare equal among themselves as there is no comparator registered for them.

Please note that `register-default-comparator!` is not thread-safe with respect to itself or the
default comparator. R7RS does not specify whether the libraries can be loaded concurrently (and if
they can, what means are used for that). So even if there is some implementation that actually loads
libraries concurrently, we cannot safely use mutexes from SRFI 18 (as they may not work due to
different threading libraries being used by the implementation for purpose of loading libraries and
to implement user-level threads). Therefore, concurrency issues are not addressed by this SRFI
implementation and are left in the unportable domain. Fix them youself if your implementation breaks
because of it. Sorry.
