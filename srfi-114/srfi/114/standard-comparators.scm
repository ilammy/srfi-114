;; standard-comparators.scm -- standard comparators
;; Copyright (c) 2015 ilammy <a.lozovsky@gmail.com>
;; 3-clause BSD license: http://github.com/ilammy/srfi-114/blob/master/LICENSE


;; Standard comparators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define null-comparator
  (make-comparator null? null-equality null-comparison null-hash))

(define boolean-comparator
  (make-comparator boolean? boolean-equality boolean-comparison boolean-hash))

(define char-comparator
  (make-comparator char? char-equality char-comparison char-hash))

(define char-ci-comparator
  (make-comparator char? char-ci-equality char-ci-comparison char-ci-hash))

(define string-comparator
  (make-comparator string? string-equality string-comparison string-hash))

(define string-ci-comparator
  (make-comparator string? string-ci-equality string-ci-comparison string-ci-hash))

(define symbol-comparator
  (make-comparator symbol? symbol-equality symbol-comparison symbol-hash))

(define exact-integer-comparator
  (make-comparator exact-integer? real-number-equality real-number-comparison real-number-hash))

(define integer-comparator
  (make-comparator integer? real-number-equality real-number-comparison real-number-hash))

(define rational-comparator
  (make-comparator rational? real-number-equality real-number-comparison real-number-hash))

(define real-comparator
  (make-comparator real? real-number-equality real-number-comparison real-number-hash))

(define complex-comparator
  (make-comparator complex? complex-number-equality complex-number-comparison complex-number-hash))

(define number-comparator
  (make-comparator number? complex-number-equality complex-number-comparison complex-number-hash))

(define pair-comparator
  (make-comparator pair? pair-equality pair-comparison pair-hash))

(define list-comparator
  (make-comparator list? list-equality list-comparison list-hash))

(define vector-comparator
  (make-comparator vector? vector-equality vector-comparison vector-hash))

(define bytevector-comparator
  (make-comparator bytevector? bytevector-equality bytevector-comparison bytevector-hash))


;; Wrapped equality predicates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eq-comparator    (make-comparator #t eq?    #f srfi-69:hash-by-identity))
(define eqv-comparator   (make-comparator #t eqv?   #f srfi-69:hash))
(define equal-comparator (make-comparator #t equal? #f srfi-69:hash))
