(define-library (srfi 114)
  (export comparator? comparator-comparison-procedure?
   comparator-hash-function? boolean-comparator char-comparator
   char-ci-comparator string-comparator string-ci-comparator
   symbol-comparator exact-integer-comparator integer-comparator
   rational-comparator real-comparator complex-comparator
   number-comparator pair-comparator list-comparator vector-comparator
   bytevector-comparator default-comparator make-comparator
   make-inexact-real-comparator make-vector-comparator
   make-bytevector-comparator make-list-comparator
   make-vectorwise-comparator make-listwise-comparator
   make-car-comparator make-cdr-comparator make-pair-comparator
   make-improper-list-comparator make-selecting-comparator
   make-refining-comparator make-reverse-comparator
   make-debug-comparator eq-comparator eqv-comparator equal-comparator
   comparator-type-test-procedure comparator-equality-predicate
   comparator-comparison-procedure comparator-hash-function
   comparator-test-type comparator-check-type comparator-equal?
   comparator-compare comparator-hash comparator-min comparator-max
   make-comparison< make-comparison> make-comparison<= make-comparison>=
   make-comparison=/< make-comparison=/> make= make< make> make<= make>=
   if3 if=? if<? if>? if<=? if>=? if-not=? =? <? >? <=? >=?
   in-open-interval? in-closed-interval? in-open-closed-interval?
   in-closed-open-interval?)

  (import (scheme base)
          (scheme case-lambda)
          (scheme char)
          (scheme complex)
          (scheme inexact)
          (only (srfi 69) hash hash-by-identity string-hash string-ci-hash))

  (include "114/types.scm"
           "114/default-comparator.scm"
           "114/comparison-utils.scm"
           "114/standard-comparisons.scm"
           "114/standard-comparators.scm"
           "114/constructors.scm"
           "114/debug-comparator.scm"))
