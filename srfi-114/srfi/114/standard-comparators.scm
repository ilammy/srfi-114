;; standard-comparators.scm -- standard comparators
;; Copyright (c) 2015 ilammy <a.lozovsky@gmail.com>
;; 3-clause BSD license: http://github.com/ilammy/srfi-114/blob/master/LICENSE


;; Standard comparators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define boolean-comparator
  (make-comparator boolean? boolean=? boolean-comparison hash-by-identity))

(define char-comparator
  (make-comparator char? char=? char-comparison hash))

(define char-ci-comparator
  (make-comparator char? char-ci=? char-ci-comparison hash))

(define string-comparator
  (make-comparator string? string=? string-comparison string-hash))

(define string-ci-comparator
  (make-comparator string? string-ci=? string-ci-comparison string-ci-hash))

(define symbol-comparator
  (make-comparator symbol? symbol=? symbol-comparison hash-by-identity))

(define exact-integer-comparator
  (make-comparator exact-integer? = real-number-comparison hash))

(define integer-comparator
  (make-comparator integer? = real-number-comparison hash))

(define rational-comparator
  (make-comparator rational? = real-number-comparison hash))

(define real-comparator
  (make-comparator real? = real-number-comparison hash))

(define complex-comparator
  (make-comparator complex? = complex-number-comparison hash))

(define number-comparator
  (make-comparator number? = complex-number-comparison hash))

(define pair-comparator
  (make-comparator pair? #t pair-comparison hash))

(define list-comparator
  (make-comparator list? #t list-comparison hash))

(define vector-comparator
  (make-comparator vector? #t vector-comparison hash))

(define bytevector-comparator
  (make-comparator bytevector? #t bytevector-comparison hash))


;; Wrapped equality predicates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eq-comparator    (make-comparator #t eq?    #f hash-by-identity))
(define eqv-comparator   (make-comparator #t eqv?   #f hash))
(define equal-comparator (make-comparator #t equal? #f hash))
