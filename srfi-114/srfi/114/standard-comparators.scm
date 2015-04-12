;; standard-comparators.scm -- standard comparators
;; Copyright (c) 2015 ilammy <a.lozovsky@gmail.com>
;; 3-clause BSD license: http://github.com/ilammy/srfi-114/blob/master/LICENSE


;; Standard comparators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define null-comparator
  (make-comparator null? null-equality null-comparison srfi-69:hash-by-identity))

(define boolean-comparator
  (make-comparator boolean? boolean-equality boolean-comparison srfi-69:hash-by-identity))

(define char-comparator
  (make-comparator char? char-equality char-comparison srfi-69:hash))

(define char-ci-comparator
  (make-comparator char? char-ci-equality char-ci-comparison srfi-69:hash))

(define string-comparator
  (make-comparator string? string-equality string-comparison srfi-69:string-hash))

(define string-ci-comparator
  (make-comparator string? string-ci-equality string-ci-comparison srfi-69:string-ci-hash))

(define symbol-comparator
  (make-comparator symbol? symbol-equality symbol-comparison srfi-69:hash-by-identity))

(define exact-integer-comparator
  (make-comparator exact-integer? real-number-equality real-number-comparison srfi-69:hash))

(define integer-comparator
  (make-comparator integer? real-number-equality real-number-comparison srfi-69:hash))

(define rational-comparator
  (make-comparator rational? real-number-equality real-number-comparison srfi-69:hash))

(define real-comparator
  (make-comparator real? real-number-equality real-number-comparison srfi-69:hash))

(define complex-comparator
  (make-comparator complex? complex-number-equality complex-number-comparison srfi-69:hash))

(define number-comparator
  (make-comparator number? complex-number-equality complex-number-comparison srfi-69:hash))

(define pair-comparator
  (make-comparator pair? pair-equality pair-comparison srfi-69:hash))

(define list-comparator
  (make-comparator list? list-equality list-comparison srfi-69:hash))

(define vector-comparator
  (make-comparator vector? vector-equality vector-comparison srfi-69:hash))

(define bytevector-comparator
  (make-comparator bytevector? bytevector-equality bytevector-comparison srfi-69:hash))


;; Wrapped equality predicates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eq-comparator    (make-comparator #t eq?    #f srfi-69:hash-by-identity))
(define eqv-comparator   (make-comparator #t eqv?   #f srfi-69:hash))
(define equal-comparator (make-comparator #t equal? #f srfi-69:hash))
